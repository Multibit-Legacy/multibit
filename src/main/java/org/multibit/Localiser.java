package org.multibit;

import com.google.bitcoin.core.Utils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.text.MessageFormat;
import java.util.*;

/**
 * class used for producing localised messages it contains a resource bundle and
 * has some helper functions to do message formatting
 * 
 * @author jim
 * 
 */
public class Localiser {

    private static final Logger log = LoggerFactory.getLogger(Localiser.class);

    public static final String VIEWER_RESOURCE_BUNDLE_NAME = "/i18n/viewer";
    public static final String SEPARATOR = "_";
    public static final String PROPERTY_NAME_SUFFIX = ".properties";
    public static final String VERSION_PROPERTY_KEY_NAME = "version";
    public static final String VERSION_PROPERTIES_FILENAME = "/version.properties";
    private ResourceBundle resourceBundle;
    private MessageFormat formatter;

    private Properties versionProperties;

    private Locale locale;
    private String bundleName;

    private final static String MISSING_RESOURCE_TEXT = "Missing resource : ";

    /**
     * Localiser hardwired to English - mainlt for testing
     */
    public Localiser() {
        this(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale("en"));
    }
    
    /**
     * create a Localiser using a ResourceBundle based on the specified
     * 'bundleName' with Locale 'locale'
     * 
     * @param bundleName
     * @param locale
     */
    public Localiser(String bundleName, Locale locale) {
        this.bundleName = bundleName;
        formatter = new MessageFormat("");
        setLocale(locale);
    }

    public String getString(String key) {
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
            } catch (StringIndexOutOfBoundsException e) {
                return MISSING_RESOURCE_TEXT + key + " (e)";
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

        String propertyFilename = bundleName + SEPARATOR + locale.getLanguage() + PROPERTY_NAME_SUFFIX;
        String propertyFilenameBase = bundleName + PROPERTY_NAME_SUFFIX;
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
                InputStream inputStream =  Localiser.class.getResourceAsStream(propertyFilenameBase);
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
     * @param value The value
     * @param addUnit True if a unit should be appended
     * @param blankZero True if a zero value should be returned as blank
     * @return the given value in nanocoins as a 0.12 type string - 2 sig figs
     */
    public static String bitcoinValueToFriendlyString(BigInteger value, boolean addUnit, boolean blankZero) {
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
            toReturn = toReturn + " " + "BTC";
        }
        return toReturn;
    }
    
    /** 
     * Returns the given value in nanocoins as a 0.1234 type string down to 100 mikes
     **/
    public static String bitcoinValueToString4(BigInteger value, boolean addUnit, boolean blankZero) {
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
        //toReturn = toReturn + (coins.floatValue() + ((float)cents.intValue()) / Utils.COIN.intValue());
        toReturn = String.format("%s%d.%04d", negative ? "-" : "", coins.intValue(), fraction.intValue() / 10000);
        if (addUnit) {
            toReturn = toReturn + " " + "BTC";
        }
        return toReturn;
    }
    
    /** 
     * Returns the given value in nanocoins as a number with as many digits as it needs
     **/
    public static String bitcoinValueToString(BigInteger value, boolean addUnit, boolean blankZero) {
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
        toReturn = toReturn + (coins.floatValue() + ((float)fraction.intValue()) / Utils.COIN.intValue());
        if (addUnit) {
            toReturn = toReturn + " " + "BTC";
        }
        return toReturn;
    }
}
