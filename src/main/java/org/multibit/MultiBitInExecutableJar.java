package org.multibit;

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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

/**
 * Main MultiBit entry class for when running in an executable jar - put console output to a file
 * 
 * @author jim
 * 
 */
public class MultiBitInExecutableJar {
    public static final String OUTPUT_DIRECTORY = "log";
    public static final String OUTPUT_FILENAME = "multibit_console.log";

    private static final Logger log = LoggerFactory.getLogger(MultiBitInExecutableJar.class);

    /**
     * start multibit user interface when running in a jar
     */
    public static void main(String args[]) {
      // TODO Refactor this to work with a different Logger appender
        // redirect the console output to a file
       PrintStream orgStream   = null;
        PrintStream fileStream  = null;
        try {
            // Saving the orginal stream
            orgStream = System.out;
            
            // create output directory
            (new File(OUTPUT_DIRECTORY)).mkdir();
            String outputFilename = OUTPUT_DIRECTORY + File.separator + OUTPUT_FILENAME;
            
            fileStream = new PrintStream(new FileOutputStream(outputFilename,true));
            
            // Redirecting console output to file
            System.setOut(fileStream);
            // Redirecting runtime exceptions to file
            System.setErr(fileStream);
            
            // call the main MultiBit code
            MultiBit.main(args);
        }
        catch (FileNotFoundException e) {
            log.error("Error in IO Redirection", e);
        }
        catch (Exception e) {
            //Gets printed in the file
            log.debug("Redirecting output & exceptions to file", e);
        }
        finally {
            //Restoring back to console
            System.setOut(orgStream);
            //Gets printed in the console
            log.debug("Redirecting file output back to console");
        }
    }
}
