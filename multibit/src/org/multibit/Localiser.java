package org.multibit;

import java.io.IOException;
import java.math.BigInteger;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;

import javax.swing.KeyStroke;

import org.multibit.viewsystem.swing.MultiBitFrame;

import com.google.bitcoin.core.Utils;

/**
 * class used for producing localised messages it contains a resource bundle and
 * has some helper functions to do message formatting
 * 
 * @author jim
 * 
 */
public class Localiser {
    public static final String VIEWER_RESOURCE_BUNDLE_NAME = "i18n/viewer";
    public static final String VERSION_PROPERTY_KEY_NAME = "version";
    public static final String VERSION_PROPERTIES_FILENAME = "/version.properties";
    private ResourceBundle resourceBundle;
    private MessageFormat formatter;

    private Properties versionProperties;
    
    private Locale locale;
    private String bundleName;

    private final String MISSING_RESOURCE_TEXT = "Missing resource : ";

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
    
    /**
     * get the mnemonic key code for the passed in internationalisation key
     * @param key
     * @return
     */
    public int getMnemonic(String key) {
        if (resourceBundle != null) {
            try {
                return KeyStroke.getKeyStroke(getString(key)).getKeyCode();
            } catch (NullPointerException npe) {
                return 0;
            } catch (ClassCastException cce) {
                return 0;
            } catch (MissingResourceException mre) {
                return 0;
            }
        } else {
            return 0;
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

        try {
            resourceBundle = ResourceBundle.getBundle(bundleName, locale);
        } catch (MissingResourceException mre) {
            // if you cannot find the ResourceBundle you carry on but keys are not looked up
            mre.printStackTrace();
        } catch (NullPointerException npe) {
            // if you cannot find the ResourceBundle you carry on but keys are not looked up
            npe.printStackTrace();
        }
    }
    
    /**
     * get the version number specified in the version.properties file
     * TODO - this should probably be somewhere else
     * @return
     */
    public String getVersionNumber() {
        String version = "";
        if (versionProperties == null) {
            versionProperties = new Properties();
            try {
                java.net.URL versionPropertiesURL = MultiBitFrame.class.getResource(VERSION_PROPERTIES_FILENAME);
                if (versionPropertiesURL != null) {
                    versionProperties.load(versionPropertiesURL.openStream());
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
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
    
    /** Returns the given value in nanocoins as a 0.12 type string. */
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
    

}
