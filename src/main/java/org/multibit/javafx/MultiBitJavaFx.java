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
package org.multibit.javafx;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.MultiBit;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.net.URL;
import java.util.List;

/**
 * Main MultiBit entry class for when running in an executable jar - put console
 * output to a file.
 * <p/>
 * This uses Javafx to start up so that the args passed on the command line by the
 * Windows executable can be accessed.
 */
public final class MultiBitJavaFx extends Application {

    public static final String OUTPUT_DIRECTORY = "log";
    public static final String CONSOLE_OUTPUT_FILENAME = "multibit.log";

    private static Logger log = LoggerFactory.getLogger(MultiBitJavaFx.class);

    private static String[] argsPassedInMain = null;

    static {
        java.awt.Toolkit.getDefaultToolkit();
    }

    /**
     * Constructor used by JavaFX
     */
    public MultiBitJavaFx() {
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        String[] argsToUse = null;

        URL fxml = MultiBitFrame.class.getResource("/sample.fxml");
        log.debug("fxml = " + fxml);
        Parent root = FXMLLoader.load(fxml);
        primaryStage.setTitle("Hello World");
        primaryStage.setScene(new Scene(root, 300, 275));
        primaryStage.show();

        // Normally the args passed into the Main are used.
        // However when MultiBit is packaged on Windows the bitcoin URI is passed
        // to the exe file but only 'comes in' via Javafx parameters.

        if (argsPassedInMain != null) {
            for (String arg : argsPassedInMain) {
                log.debug("argsPassedInMain = " + arg);
                primaryStage.setTitle("argsPassedTo main = " + arg);
            }
            argsToUse = argsPassedInMain;
        } else {
            // See if there is a parameter available in JavaFx.
            List<String> unNamedParameters = getParameters().getUnnamed();
            if (unNamedParameters != null && unNamedParameters.size() > 0) {
                for (String arg : unNamedParameters) {
                    log.debug("unNamedParameters = " + arg);
                    primaryStage.setTitle("unNamedParameters" + arg);
                }
                argsToUse = (String[]) unNamedParameters.toArray();
            }
        }

        // Redirect the console output to a file.
        PrintStream fileStream;
        try {
            // Get the current data directory.
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator();

            String outputDirectory;
            String consoleOutputFilename;

            if ("".equals(applicationDataDirectoryLocator.getApplicationDataDirectory())) {
                // Use defaults.
                outputDirectory = OUTPUT_DIRECTORY;
                consoleOutputFilename = OUTPUT_DIRECTORY + File.separator + CONSOLE_OUTPUT_FILENAME;
            } else {
                // Use defined data directory as the root.
                outputDirectory = applicationDataDirectoryLocator.getApplicationDataDirectory() + File.separator
                        + OUTPUT_DIRECTORY;
                consoleOutputFilename = applicationDataDirectoryLocator.getApplicationDataDirectory() + File.separator
                        + OUTPUT_DIRECTORY + File.separator + CONSOLE_OUTPUT_FILENAME;
            }

            log = LoggerFactory.getLogger(MultiBitJavaFx.class);

            // Create output directory.
            (new File(outputDirectory)).mkdir();

            // Create output console log.
            (new File(consoleOutputFilename)).createNewFile();

            fileStream = new PrintStream(new FileOutputStream(consoleOutputFilename, true));

            if (fileStream != null) {
                // Redirecting console output to file .
                System.setOut(fileStream);
                // Redirecting runtime exceptions to file.
                System.setErr(fileStream);
            }
        } catch (FileNotFoundException e) {
            if (log != null) {
                log.error("Error in IO Redirection", e);
            } else {
                System.out.println("MultiBit start up : Error in IO Redirection : " + e.getClass().getCanonicalName() + " " + e.getMessage());
            }
        } catch (Exception e) {
            if (log != null) {
                log.error("Error in redirecting output & exceptions to file", e);
            } else {
                System.out.println("MultiBit start up : Error in redirecting output & exceptions to file : " + e.getClass().getCanonicalName() + " " + e.getMessage());
            }
        } finally {
            // Call the main MultiBit code.
            MultiBit.main(argsToUse);

            //Thread.sleep(5000);
        }
    }


    /**
     * Start multibit user interface when running in a jar.
     * This will adjust the logging framework output to ensure that console output is sent
     * to a file appender in the client.
     *
     * @param args The optional command line arguments ([0] can be a Bitcoin URI
     */
    public static void main(String[] args) {
        argsPassedInMain = args;
        launch(args);
    }
}
