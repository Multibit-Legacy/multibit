package org.multibit.protocolhandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

/**
 *
 * Read and set Windows registry settings using reg on a command line.
 * The reg syntax is described here: http://ss64.com/nt/reg.html .
 *
 * @author Oleg Ryaboy, based on work by Miguel Enriquez
 * @author Jim Burton
 */
public class WindowsRegistry {
    public static final String HKEY_CLASSES_ROOT = "HKCR";

    public static final String REG_COMMAND = "REG";

    public static final String QUERY_TOKEN = "QUERY";
    public static final String ADD_TOKEN = "ADD";

    public static final String VALUE_TOKEN = " /v ";
    public static final String VALUE_EMPTY_TOKEN = " /ve ";
    public static final String DATA_TOKEN = " /d ";
    public static final String TYPE_TOKEN = " /t ";
    public static final String SPACE = " ";

    public static final String TYPE_STRING = "REG_SZ";
    public static final String TYPE_DWORD = "REG_DWORD";


    private static final Logger log = LoggerFactory.getLogger(WindowsRegistry.class);

    private WindowsRegistry() {

    }

    /**
     * @param location path in the registry
     * @param key registry key
     * @return registry value or null if not found
     */
    public static final String readRegistry(String location, String key){
        try {
            // Run reg query, then read output with StreamReader (internal class).
            String command = REG_COMMAND + SPACE + QUERY_TOKEN + SPACE + "\"" + location + "\"";
            if (key == null || key.equals("")) {
                // If key is blank or null, run REG with /ve and no key - this reads the default value.
                command = command + VALUE_EMPTY_TOKEN;
            } else {
                command = command + VALUE_TOKEN + key;
            }
            log.debug("command = " + command);
            Process process = Runtime.getRuntime().exec(command);

            StreamReader reader = new StreamReader(process.getInputStream());
            reader.start();
            process.waitFor();
            reader.join();
            String output = reader.getResult();

            log.debug("output =  '" + output + "'.");

            // Output has various formats.
            // No key specified:
            //HKEY_CLASSES_ROOT\bitcoin
            //        (Default)    REG_SZ    URL:Bitcoin Protocol
            //URL Protocol    REG_SZ
            //UseOriginalUrlEncoding    REG_DWORD    0x1

            // Key specified:
            // TODO

            return output;
        }
        catch (Exception e) {
            log.error(e.getClass().getCanonicalName() + e.getMessage());
            return null;
        }
    }

    /**
     * @param location path in the registry
     * @param key registry key
     * @param value value to set key to
     */
    public static void setRegistry(String location, String key, String value){
        setRegistry(location, key, value, TYPE_STRING);
    }

        /**
         * @param location path in the registry
         * @param key registry key
         * @param value value to set key to
         * @param type type of value
         */
    public static void setRegistry(String location, String key, String value, String type){
        try {
            // Run reg add, then read output with StreamReader (internal class)
            // Example command line: REG ADD HKCU\Software\SS64 /v Sample /d "some test data"
            String command = REG_COMMAND + SPACE + ADD_TOKEN + SPACE + "\"" + location + "\"";
            if (key == null || key.equals("")) {
                // If key is blank or null, run REG with /ve and no key - this sets the default value.
                command = command + VALUE_EMPTY_TOKEN;
            } else {
                command = command + VALUE_TOKEN + key;
            }
            if (TYPE_DWORD.equalsIgnoreCase(type)) {
                command = command + TYPE_TOKEN + TYPE_DWORD + SPACE;
            }
            command = command + DATA_TOKEN + value;
            log.debug("command = " + command);
            Process process = Runtime.getRuntime().exec(command);

            StreamReader reader = new StreamReader(process.getInputStream());
            reader.start();
            process.waitFor();
            reader.join();
            String output = reader.getResult();

            log.debug("output =  '" + output + "'.");
        }
        catch (Exception e) {
            log.error(e.getClass().getCanonicalName() + e.getMessage());
        }

    }

    static class StreamReader extends Thread {
        private InputStream is;
        private StringWriter sw= new StringWriter();

        public StreamReader(InputStream is) {
            this.is = is;
        }

        public void run() {
            try {
                int c;
                while ((c = is.read()) != -1)
                    sw.write(c);
            }
            catch (IOException e) {
            }
        }

        public String getResult() {
            return sw.toString();
        }
    }
    public static void main(String[] args) {

        // Sample usage
        String value = WindowsRegistry.readRegistry("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\"
                + "Explorer\\Shell Folders", "Personal");
        System.out.println(value);
    }
}

