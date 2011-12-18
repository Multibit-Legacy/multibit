package org.multibit;

import org.multibit.network.FileHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class ApplicationDataDirectoryLocator {
    private String applicationDataDirectory = null;
    
    private static final Logger log = LoggerFactory.getLogger(ApplicationDataDirectoryLocator.class);

    private static final Logger log2 = LoggerFactory.getLogger(MultiBit.class);

    public ApplicationDataDirectoryLocator() {
        applicationDataDirectory = getApplicationDataDirectory();
        log.debug("Level 1");
        log.info("Application data directory = '" + applicationDataDirectory + "'");
        log.warn("Level 3");
        log2.debug("Level 1 - mb");
        log2.info("Level 2 - mb");
        log2.warn("Level 3 - mb");
        log2.error("Level 4 - mb");
    }
    
    /**
     * get the directory for the user's application data
     * 
     * This is worked out as follows:
     * 
     * 1. See if there is a multibit.properties in MultiBit's current working
     * directory If there is, use this directory as the application data
     * directory This is for backwards compatibility and for running everything
     * from a USB drive
     * 
     * 2. Otherwise set the working directory as follows:
     * 
     * PC System.getenv("APPDATA")/MultiBit
     * 
     * e.g. C:/Documents and Settings/Administrator/Application Data/MultiBit
     * 
     * Mac System.getProperty("user.home")/Library/Application Support/MultiBit
     * 
     * e.g. /Users/jim/Library/Application Support/MultiBit
     * 
     * Linux System.getProperty("user.home")/MultiBit
     * 
     * e.g. /Users/jim/MultiBit
     */
    public String getApplicationDataDirectory() {
        if (applicationDataDirectory != null) {
            return applicationDataDirectory;
        }
        
        File multibitPropertiesFile = new File(FileHandler.USER_PROPERTIES_FILE_NAME);
        if (multibitPropertiesFile.exists()) {
            // applicationDataDirectory is the local directory;
            applicationDataDirectory = "";
        } else {
            String operatingSystemName = System.getProperty("os.name");
            if (operatingSystemName.startsWith("Windows")) {
                // Windows os
                applicationDataDirectory = System.getenv("APPDATA") + File.separator + "MultiBit";
            } else {
                if (operatingSystemName.startsWith("Mac")) {
                    // Mac os
                    applicationDataDirectory = System.getProperty("user.home") + "/Library/Application Support/MultiBit";
                } else {
                    // treat as Linux/ unix variant
                    applicationDataDirectory = System.getProperty("user.home") + "/MultiBit";
                }
            }
            
            // create the application data directory if it does not exist
            File directory = new File(applicationDataDirectory);
            if (!directory.exists()) {
                boolean created = directory.mkdir();
                if (!created) {
                    log.error("Could not create the application data directory of '" + applicationDataDirectory + "'");
                }
            }
        }

        return applicationDataDirectory;
    }
}

