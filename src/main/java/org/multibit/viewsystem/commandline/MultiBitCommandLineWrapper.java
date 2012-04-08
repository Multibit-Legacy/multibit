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
package org.multibit.viewsystem.commandline;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

import org.multibit.ApplicationDataDirectoryLocator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.rolling.RollingFileAppender;
import ch.qos.logback.core.rolling.TimeBasedRollingPolicy;

/**
 * Wrapper around MultiBitCommandLine - put console
 * output to a file
 * 
 * @author jim
 * 
 */
public class MultiBitCommandLineWrapper {
    public static final String OUTPUT_DIRECTORY = "log";
    public static final String CONSOLE_OUTPUT_FILENAME = "multibit_commandline_console.log";
    public static final String DEBUG_OUTPUT_FILENAME = "multibit_commandline_debug.log";

    private static Logger log = LoggerFactory.getLogger(MultiBitCommandLineWrapper.class);

    /**
     * Start multibit user interface when running in a jar
     * This will adjust the logging framework output to ensure that console output is sent
     * to a file appender in the client to ensure that no clues to failure are missed
     * @param args The optional command line arguments ([0] can be a Bitcoin URI
     */
    @SuppressWarnings("rawtypes")
    public static void main(String args[]) {
        // TODO Refactor this to work with a different Logger appender
        // redirect the console output to a file
        PrintStream originalStream = null;
        PrintStream fileStream = null;
        try {
            // Get the current data directory
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator();
            
            String outputDirectory;
            String consoleOutputFilename;
            String debugOutputFilename;
            
            if ("".equals(applicationDataDirectoryLocator.getApplicationDataDirectory())) {
                // Use defaults
                outputDirectory = OUTPUT_DIRECTORY;
                consoleOutputFilename = OUTPUT_DIRECTORY + File.separator + CONSOLE_OUTPUT_FILENAME;
                debugOutputFilename = OUTPUT_DIRECTORY + File.separator + DEBUG_OUTPUT_FILENAME;
            } else {
                // Use defined data directory as the root
                outputDirectory = applicationDataDirectoryLocator.getApplicationDataDirectory() + File.separator
                        + OUTPUT_DIRECTORY;
                consoleOutputFilename = applicationDataDirectoryLocator.getApplicationDataDirectory() + File.separator
                        + OUTPUT_DIRECTORY + File.separator + CONSOLE_OUTPUT_FILENAME;
                debugOutputFilename = applicationDataDirectoryLocator.getApplicationDataDirectory() + File.separator
                        + OUTPUT_DIRECTORY + File.separator + DEBUG_OUTPUT_FILENAME;
            }

            // Get the rolling file appender from Logback and adjust it to meet local requirements
            ch.qos.logback.classic.Logger logback_logger = (ch.qos.logback.classic.Logger) LoggerFactory
                    .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
            RollingFileAppender<ILoggingEvent> rfappender = (RollingFileAppender<ILoggingEvent>) logback_logger
                    .getAppender("DETAIL_APPENDER");
            rfappender.setFile(debugOutputFilename);
            TimeBasedRollingPolicy rollingPolicy = (TimeBasedRollingPolicy) rfappender.getRollingPolicy();
            rollingPolicy.setFileNamePattern(debugOutputFilename + ".%d{yyyy-MM-dd}.gz");

            rollingPolicy.stop();
            rfappender.stop();
            rollingPolicy.start();
            rfappender.start();
            
            log = LoggerFactory.getLogger(MultiBitCommandLineWrapper.class);
            // Saving the orginal stream
            originalStream = System.out;

            // create output directory
            (new File(outputDirectory)).mkdir();

            // create output console log
            (new File(consoleOutputFilename)).createNewFile();

            fileStream = new PrintStream(new FileOutputStream(consoleOutputFilename, true));

            // Redirecting console output to file
            //System.setOut(fileStream);
            // Redirecting runtime exceptions to file
            //System.setErr(fileStream);
        } catch (FileNotFoundException e) {
            if (log != null) {
                log.error("Error in IO Redirection", e);
            }
        } catch (Exception e) {
            // Gets printed in the file
            if (log != null) {
                log.debug("Error in redirecting output & exceptions to file", e);
            }
        } finally {
            // call the main MultiBit code
            MultiBitCommandLine.main(args);

            // Restoring back to console
            //System.setOut(originalStream);
            //System.setErr(originalStream);
            // Gets printed in the console
            if (log != null) {
                log.debug("Redirecting file output back to console");
            }
        }
    }
}
