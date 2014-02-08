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
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import org.joda.money.BigMoney;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.dogecoin.core.Utils;

/**
 * Class used for producing localised messages it contains a resource bundle and
 * has some helper functions to do message formatting.
 * 
 * It also pulls in a file language.properties which lists the number of
 * languages and language codes.
 * 
 * @author jim
 * 
 */
public class Localiser {

    private static final Logger log = LoggerFactory.getLogger(Localiser.class);

    public static final String MULTIBIT_RESOURCE_BUNDLE_DIRECTORY = "/i18n";
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
    
    private NumberFormat numberFormat;
    private NumberFormat numberFormatNotLocalised;
    
    public static final int NUMBER_OF_FRACTION_DIGITS_FOR_BITCOIN = 8;
    
    private java.text.DecimalFormatSymbols decimalFormatSymbols;

    /**
     * Localiser hardwired to English - mainly for testing
     */
    public Localiser() {
        this(new Locale("en"));
    }

    /**
     * Create a Localiser using a ResourceBundle based on the specified
     * 'bundleName' with Locale 'locale'.
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
        
        numberFormat = NumberFormat.getInstance(locale);
        numberFormat.setMaximumFractionDigits(NUMBER_OF_FRACTION_DIGITS_FOR_BITCOIN);
        numberFormatNotLocalised = NumberFormat.getInstance(Locale.ENGLISH);
        numberFormatNotLocalised.setMaximumFractionDigits(NUMBER_OF_FRACTION_DIGITS_FOR_BITCOIN);
        
        decimalFormatSymbols = new java.text.DecimalFormatSymbols(locale);
    }

    synchronized public String getString(String key) {
        if (key == null) {
            return "";
        }

        // See if it is the number of languages or a language code.
        if (languageProperties != null) {
            String toReturn = (String) languageProperties.get(key);
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

    synchronized public String getString(String key, Object[] parameters) {
        if (key == null) {
            return "";
        }

        if (resourceBundle != null) {
            try {
                String pattern = resourceBundle.getString(key);
                // Change any apostrophes to  \u2032 as MessageFormatter swallows them
                pattern = pattern.replaceAll("\u0027", "\u2032");
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
        String propertyFilename = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/" + locale.getLanguage() + "/"
                + MULTIBIT_RESOURCE_BUNDLE_NAME + PROPERTY_NAME_SUFFIX;
        String propertyFilenameBase = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/" + FALLBACK_LANGUAGE_CODE + "/"
                + MULTIBIT_RESOURCE_BUNDLE_NAME + PROPERTY_NAME_SUFFIX;

        if ("he".equals(locale.getLanguage()) || "iw".equals(locale.getLanguage())) {
            // Hebrew can be he or iw
            this.locale = new Locale("iw");
            propertyFilename = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/he/" + MULTIBIT_RESOURCE_BUNDLE_NAME + PROPERTY_NAME_SUFFIX;
        } if ("id".equals(locale.getLanguage()) || "in".equals(locale.getLanguage())) {
            // Indonesian can be id or in
            this.locale = new Locale("in");
            propertyFilename = MULTIBIT_RESOURCE_BUNDLE_DIRECTORY + "/id/" + MULTIBIT_RESOURCE_BUNDLE_NAME + PROPERTY_NAME_SUFFIX;
        } else {
            this.locale = locale;
        }

        formatter.setLocale(locale);

        numberFormat = NumberFormat.getInstance(locale);
        numberFormat.setMaximumFractionDigits(NUMBER_OF_FRACTION_DIGITS_FOR_BITCOIN);
        
        decimalFormatSymbols = new java.text.DecimalFormatSymbols(locale);

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
     * Get the version number specified in the version.properties file.
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
     * Returns the given value in nanocoins as a 0.12345678 type string.
     * This function is localised. 
     **/
    public String bitcoinValueToString(BigInteger value, boolean addUnit, boolean blankZero) {
        if (blankZero && value.compareTo(BigInteger.ZERO) == 0) {
            return "";
        }

        boolean negative = value.compareTo(BigInteger.ZERO) < 0;
        if (negative) {
            value = value.negate();
        }

        String toReturn = "";
        if (negative) {
            toReturn = "-";
        }
        
        if (value == null) {
            throw new IllegalArgumentException("Value cannot be null");
        }
                
        BigDecimal valueInBTC = new BigDecimal(value).divide(new BigDecimal(Utils.COIN));
        numberFormat.setGroupingUsed(false);
        toReturn = toReturn + numberFormat.format(valueInBTC.doubleValue());
        numberFormat.setGroupingUsed(true);

        if (addUnit) {
            toReturn = toReturn + " " + getString("sendBitcoinPanel.amountUnitLabel");
        }
        return toReturn;     
    }
    /**
     * Returns the given value in nanocoins as a 0.12345678 type string.
     * This function is NOT localised. 
     **/
    public String bitcoinValueToStringNotLocalised(BigInteger value, boolean addUnit, boolean blankZero) {
        if (blankZero && value.compareTo(BigInteger.ZERO) == 0) {
            return "";
        }

        boolean negative = value.compareTo(BigInteger.ZERO) < 0;
        if (negative) {
            value = value.negate();
        }

        String toReturn = "";
        if (negative) {
            toReturn = "-";
        }
        
        if (value == null) {
            throw new IllegalArgumentException("Value cannot be null");
        }
                
        BigDecimal valueInBTC = new BigDecimal(value).divide(new BigDecimal(Utils.COIN));
        numberFormatNotLocalised.setGroupingUsed(false);
        toReturn = toReturn + numberFormatNotLocalised.format(valueInBTC.doubleValue());
        numberFormatNotLocalised.setGroupingUsed(true);

        if (addUnit) {
            toReturn = toReturn + " " + getString("sendBitcoinPanel.amountUnitLabel");
        }
        return toReturn;     
    }
    
    /**
     * Returns a formatted money onject
     **/
    public String bigMoneyValueToString(BigMoney value) {        
        if (value == null) {
            throw new IllegalArgumentException("Value cannot be null");
        }
           
        String toReturn = "";
        toReturn = numberFormat.format(value.getAmount().doubleValue());

        return toReturn;     
    }

    public java.text.DecimalFormatSymbols getDecimalFormatSymbols() {
        return decimalFormatSymbols;
    }
}
