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
    //public static final String HKEY_CURRENT_USER = "HKCU";
    //public static final String HKEY_LOCAL_MACHINE = "HKLM";
    public static final String HKEY_CLASSES_ROOT = "HKCR";

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
            // Run reg query, then read output with StreamReader (internal class)
            Process process = Runtime.getRuntime().exec("reg query " +
                    '"'+ location + "\" /v " + key);

            StreamReader reader = new StreamReader(process.getInputStream());
            reader.start();
            process.waitFor();
            reader.join();
            String output = reader.getResult();

            System.out.println("WindowsRegistry#readRegistry read '" + output + "'.");

            // Output has the following format:
            // \n<Version information>\n\n<key>\t<registry type>\t<value>
            if( ! output.contains("\t")){
                return null;
            }

            // Parse out the value
            String[] parsed = output.split("\t");
            return parsed[parsed.length-1];
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
        try {
            // Run reg add, then read output with StreamReader (internal class)
            // Example command line: REG ADD HKCU\Software\SS64 /v Sample /d "some test data"
            Process process = Runtime.getRuntime().exec("reg add " + location + " /v " + key
                    + " /d " + value);

            StreamReader reader = new StreamReader(process.getInputStream());
            reader.start();
            process.waitFor();
            reader.join();
            String output = reader.getResult();

            System.out.println("WindowsRegistry#setRegistry set of location = " + location + ", key = " + key + ", value = " + value + ".\n Result = '" + output + "'.");

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

