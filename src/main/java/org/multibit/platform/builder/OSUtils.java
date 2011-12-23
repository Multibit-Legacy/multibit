package org.multibit.platform.builder;

/**
 * <p>Utilities to provide the following to {@link org.multibit.platform.GenericApplicationFactory}:</p>
 * <ul>
 * <li>Detection of operating system</li>
 * </ul>
 *
 * @since 0.2.0
 *         
 */
public class OSUtils {

    public static String getOsName() {
        return System.getProperty("os.name", "unknown");
    }

    public static String platform() {
        String osName = System.getProperty("os.name", "generic").toLowerCase();
        if (osName.startsWith("windows")) {
            return "win32";
        } else if (osName.startsWith("linux")) {
            return "linux";
        } else if (osName.startsWith("sunos")) {
            return "solaris";
        } else if (osName.startsWith("mac") || osName.startsWith("darwin")) {
            return "mac";
        } else return "generic";
    }

    public static boolean isWindows() {
        return (getOsName().toLowerCase().contains("windows"));
    }

    public static boolean isLinux() {
        return getOsName().toLowerCase().contains("linux");
    }

    public static boolean isUnix() {
        final String os = getOsName().toLowerCase();

        // XXX: this obviously needs some more work to be "true" in general (see bottom of file)
        if ((os.contains("sunos")) || (os.contains("linux"))) {
            return true;
        }

        if (isMac() && (System.getProperty("os.version", "").startsWith("10."))) {
            return true;
        }

        return false;
    }

    public static boolean isMac() {
        final String os = getOsName().toLowerCase();
        return os.startsWith("mac") || os.startsWith("darwin");
    }

    public static boolean isSolaris() {
        final String os = getOsName().toLowerCase();
        return os.contains("sunos");
    }

}