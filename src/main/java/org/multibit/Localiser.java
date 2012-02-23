/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Utils;

/**
 * class used for producing localised messages it contains a resource bundle and
 * has some helper functions to do message formatting
 * 
 * it also pulls in a file language.properties which lists the number of
 * languages and language codes
 * 
 * @author jim
 * 
 */
public class Localiser {

    private static final Logger log = LoggerFactory.getLogger(Localiser.class);

    public static final String MULTIBIT_RESOURCE_BUNDLE_DIRECTORY =  "/i18n";
    public static final String MULTIBIT_RESOURCE_BUNDLE_NAME = "viewer";
    public static final String SEPARATOR = "_";
    public static final String PROPERTY_NAME_SUFFIX = ".properties";
    public static final String VERSION_PROPERTY_KEY_NAME = "version";
    public static final String VERSION_PROPERTIES_FILENAME = "/version.properties";
    public static final String LANGUAGE_PROPERTIES_FILENAME = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/language.properties";
    
    public static final String FALLBACK_LANGUAGE_CODE = "en";
    
    private ResourceBundle resourceBundle;
    private MessageFormat formatter;

    private Properties versionProperties;

    private Properties languageProperties;

    private Locale locale;

    private final static String MISSING_RESOURCE_TEXT = "Missing resource : ";

    /**
     * Localiser hardwired to English - mainly for testing
     */
    public Localiser() {
        this(new Locale("en"));
    }

    /**
     * create a Localiser using a ResourceBundle based on the specified
     * 'bundleName' with Locale 'locale'
     * 
     * @param locale
     */
    public Localiser(Locale locale) {
        formatter = new MessageFormat("");

        languageProperties = new Properties();
        try {
            java.net.URL languagePropertiesURL = Localiser.class.getResource(LANGUAGE_PROPERTIES_FILENAME);
            if (languagePropertiesURL != null) {
                languageProperties.load(languagePropertiesURL.openStream());
            }
        } catch (IOException ioe) {
            log.error(ioe.getMessage(), ioe);
        }

        setLocale(locale);
    }

    public String getString(String key) {
        // see if it is the number of languages or a language code
        if (languageProperties != null) {
            String toReturn = (String)languageProperties.get(key);
            if (toReturn != null) {
                return toReturn;
            }
        }
        
        if (resourceBundle != null) {
            try {
                return resourceBundle.getString(key);
            } catch (NullPointerException npe) {
                return MISSING_RESOURCE_TEXT + key + " (npe)";
            } catch (ClassCastException cce) {
                return MISSING_RESOURCE_TEXT + key + " (cce)";
            } catch (MissingResourceException mre) {
                return MISSING_RESOURCE_TEXT + key + " (mre)";
            }
        } else {
            return MISSING_RESOURCE_TEXT + key;
        }
    }

    public String getString(String key, Object[] parameters) {
        if (resourceBundle != null) {
            try {
                String pattern = resourceBundle.getString(key);
                formatter.applyPattern(pattern);
                return formatter.format(parameters);
            } catch (NullPointerException npe) {
                return MISSING_RESOURCE_TEXT + key + " (npe)";
            } catch (IllegalArgumentException iae) {
                return MISSING_RESOURCE_TEXT + key + " (iae)";
            } catch (ClassCastException cce) {
                return MISSING_RESOURCE_TEXT + key + " (cce)";
            } catch (MissingResourceException mre) {
                return MISSING_RESOURCE_TEXT + key + " (mre)";
            } catch (StringIndexOutOfBoundsException sioobe) {
                return MISSING_RESOURCE_TEXT + key + " (sioobe)";
            }
        } else {
            return MISSING_RESOURCE_TEXT + key;
        }
    }

    public Locale getLocale() {
        return locale;
    }

    public void setLocale(Locale locale) {
        formatter.setLocale(locale);
        this.locale = locale;

        String propertyFilename = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/" + locale.getLanguage() + "/" + MULTIBIT_RESOURCE_BUNDLE_NAME + PROPERTY_NAME_SUFFIX;
        String propertyFilenameBase = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/" + FALLBACK_LANGUAGE_CODE + "/" + MULTIBIT_RESOURCE_BUNDLE_NAME + PROPERTY_NAME_SUFFIX;
        boolean foundIt = false;
        try {
            InputStream inputStream = Localiser.class.getResourceAsStream(propertyFilename);
            if (inputStream != null) {
                resourceBundle = new PropertyResourceBundle(new InputStreamReader(inputStream, "UTF8"));
                foundIt = true;
            }
        } catch (FileNotFoundException e) {
            log.error(e.getMessage(), e);
        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }

        if (!foundIt) {
            // just get the base version i.e. English
            try {
                InputStream inputStream = Localiser.class.getResourceAsStream(propertyFilenameBase);
                if (inputStream != null) {
                    resourceBundle = new PropertyResourceBundle(new InputStreamReader(inputStream, "UTF8"));
                }
            } catch (FileNotFoundException e) {
                log.error(e.getMessage(), e);

            } catch (IOException e) {
                log.error(e.getMessage(), e);

            }
        }
    }

    /**
     * get the version number specified in the version.properties file TODO -
     * this should probably be somewhere else
     * 
     * @return
     */
    public String getVersionNumber() {
        String version = "";
        if (versionProperties == null) {
            versionProperties = new Properties();
            try {
                java.net.URL versionPropertiesURL = Localiser.class.getResource(VERSION_PROPERTIES_FILENAME);
                if (versionPropertiesURL != null) {
                    versionProperties.load(versionPropertiesURL.openStream());
                }
            } catch (IOException ioe) {
                log.error(ioe.getMessage(), ioe);
            }
        }

        if (versionProperties != null) {
            version = versionProperties.getProperty(VERSION_PROPERTY_KEY_NAME);
            if (version == null) {
                version = "";
            }
        }
        return version;
    }

    /**
     * @param value
     *            The value
     * @param addUnit
     *            True if a unit should be appended
     * @param blankZero
     *            True if a zero value should be returned as blank
     * @return the given value in nanocoins as a 0.12 type string - 2 sig figs
     */
    public String bitcoinValueToFriendlyString(BigInteger value, boolean addUnit, boolean blankZero) {
        if (blankZero && value.compareTo(BigInteger.ZERO) == 0) {
            return "";
        }

        boolean negative = value.compareTo(BigInteger.ZERO) < 0;
        if (negative) {
            value = value.negate();
        }
        BigInteger coins = value.divide(Utils.COIN);
        BigInteger cents = value.remainder(Utils.COIN);
        String toReturn = String.format("%s%d.%02d", negative ? "-" : "", coins.intValue(), cents.intValue() / 1000000);
        if (addUnit) {
            toReturn = toReturn + " " + getString("sendBitcoinPanel.amountUnitLabel");
        }
        return toReturn;
    }

    /**
     * Returns the given value in nanocoins as a 0.1234 type string down to 100
     * mikes
     **/
    public String bitcoinValueToString4(BigInteger value, boolean addUnit, boolean blankZero) {
        if (blankZero && value.compareTo(BigInteger.ZERO) == 0) {
            return "";
        }

        boolean negative = value.compareTo(BigInteger.ZERO) < 0;
        if (negative) {
            value = value.negate();
        }
        BigInteger coins = value.divide(Utils.COIN);
        BigInteger fraction = value.remainder(Utils.COIN);

        String toReturn = "";
        if (negative) {
            toReturn = "-";
        }
        toReturn = String.format("%s%d.%04d", negative ? "-" : "", coins.intValue(), fraction.intValue() / 10000);
        if (addUnit) {
            toReturn = toReturn + " " + getString("sendBitcoinPanel.amountUnitLabel");
        }
        return toReturn;
    }

    /**
     * Returns the given value in nanocoins as a number with as many digits as
     * it needs
     **/
    public String bitcoinValueToString(BigInteger value, boolean addUnit, boolean blankZero) {
        if (blankZero && value.compareTo(BigInteger.ZERO) == 0) {
            return "";
        }

        boolean negative = value.compareTo(BigInteger.ZERO) < 0;
        if (negative) {
            value = value.negate();
        }
        BigInteger coins = value.divide(Utils.COIN);
        BigInteger fraction = value.remainder(Utils.COIN);

        String toReturn = "";
        if (negative) {
            toReturn = "-";
        }
        toReturn = toReturn + (coins.floatValue() + ((float) fraction.intValue()) / Utils.COIN.intValue());
        if (addUnit) {
            toReturn = toReturn + " " + getString("sendBitcoinPanel.amountUnitLabel");
        }
        return toReturn;
    }
}
