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

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.multibit.file.FileHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ApplicationDataDirectoryLocator {
    private String applicationDataDirectory = null;
    
    private static final Logger log = LoggerFactory.getLogger(ApplicationDataDirectoryLocator.class);

    public ApplicationDataDirectoryLocator() {
        this.applicationDataDirectory = getApplicationDataDirectory();
        log.info("Application data directory.1 = '{}'",applicationDataDirectory);
    }
    
    public ApplicationDataDirectoryLocator(File applicationDataDirectory) {
        this.applicationDataDirectory = applicationDataDirectory.getAbsolutePath();
        log.info("Application data directory.2 = '{}'",applicationDataDirectory);
    }
    
    /**
     * Get the directory for the user's application data.
     * 
     * This is worked out as follows:
     * 
     * 1. See if there is a multibit.properties in MultiBitInternal's current working
     * directory If there is, use this directory as the application data
     * directory This is for backwards compatibility and for running everything
     * from a USB drive
     * 
     * 2. On Mac only.  See if there is a multibit.properties 4 levels up
     * This is for running everything from a USB drive where you want to
     * "escape" from inside the Mac app 
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
                    if ( (new File("../../../../" + FileHandler.USER_PROPERTIES_FILE_NAME)).exists()) {
                        applicationDataDirectory = new File("../../../..").getAbsolutePath();
                    } else {
                        applicationDataDirectory = System.getProperty("user.home") + "/Library/Application Support/MultiBit";
                    }
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
    
    /**
     * Get the installation directory.
     * This is the directory into which MultiBitInternal was installed.
     * @throws IOException 
     * 
     * @TODO when running locally it is possible that the working directory directory and installation directory are different. Fix.
     */
    public String getInstallationDirectory() throws IOException {
        String operatingSystemName = System.getProperty("os.name");
        if (operatingSystemName != null && operatingSystemName.startsWith("Mac")) {
            // With the bundled JRE the location of the MultiBitInternal jar is specified in the java.library.path property.
            return System.getProperty("java.library.path");
        } else {
            File installationDirectory = new File("");
            log.debug("installationDirectory from file create = " + installationDirectory.getCanonicalPath());
            return installationDirectory.getCanonicalPath();
        }
    }
}

