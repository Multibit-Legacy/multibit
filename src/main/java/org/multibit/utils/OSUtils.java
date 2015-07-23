package org.multibit.utils;

/**
 * Copyright 2015 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.lang.management.ManagementFactory;

/**
 * <p>Utilities to provide the following to application:</p>
 * <ul>
 * <li>Detection of operating system</li>
 * <li>Detection of debugger</li>
 * </ul>
 *
 * @since 0.0.1
 *
 */
public final class OSUtils {

  /**
   * Utility class should not have a public constructor
   */
  private OSUtils() {
  }

  /**
   * @return The current platform name from "os.name" with "unknown" as a default
   */
  public static String getOsName() {
    return System.getProperty("os.name", "unknown");
  }

  /**
   * @return The current platform name from "os.version" with "unknown" as a default
   */
  public static String getOsVersion() {
    return System.getProperty("os.version", "unknown");
  }

  /**
   * @return The current platform name from "os.name" with "generic" as a default
   */
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
    } else {
      return "generic";
    }
  }

  /**
   * @return True if Windows is detected
   */
  public static boolean isWindows() {
    return (getOsName().toLowerCase().contains("windows"));
  }

  /**
   * @return True if Windows XP or earlier is detected
   */
  public static boolean isWindowsXPOrEarlier() {
    if (isWindows()) {

      try {
        Double version = Double.valueOf(getOsVersion());
        if (version < 6) {
          return true;
        }
      } catch (NumberFormatException e) {
        return false;
      }
    }

    return false;
  }

  /**
   * @return True if Linux is detected
   */
  public static boolean isLinux() {
    return getOsName().toLowerCase().contains("linux");
  }

  /**
   * @return True if Sun or Linux is detected
   */
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

  /**
   * @return True if OSX is detected
   */
  public static boolean isMac() {
    final String os = getOsName().toLowerCase();
    return os.startsWith("mac") || os.startsWith("darwin");
  }

  /**
   * @return True if the Solaris OS is detected
   */
  public static boolean isSolaris() {
    final String os = getOsName().toLowerCase();
    return os.contains("sunos");
  }

  /**
   * @return True if the Java Debug Wire Protocol is enabled
   */
  public static boolean isDebuggerAttached() {

    return ManagementFactory.getRuntimeMXBean().
      getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
  }


  /**
   * @return True if the underlying platform is 64-bit (i.e. correctly detects a 32-bit JRE on a 64-bit Windows install as 64-bit)
   */
  public static boolean is64Bit() {

    boolean result;
    if (System.getProperty("os.name").contains("Windows")) {
      result = (System.getenv("ProgramFiles(x86)") != null);
    } else {
      result = (System.getProperty("os.arch").contains("64"));
    }

    return result;
  }
}
