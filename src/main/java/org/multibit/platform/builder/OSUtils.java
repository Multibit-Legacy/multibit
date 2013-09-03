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
public final class OSUtils {

    /**
     * Utility class should not have a public constructor
     */
    private OSUtils() {
    }

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