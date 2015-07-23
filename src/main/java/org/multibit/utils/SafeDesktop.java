package org.multibit.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.io.IOException;
import java.net.URI;

/**
 * <p>Utilities to provide the following to application:</p>
 * <ul>
 * <li>Safe replacements for Desktop methods</li>
 * </ul>
 *
 * @since 0.1.0
 */
public class SafeDesktop {

  private static final Logger log = LoggerFactory.getLogger(SafeDesktop.class);

  /**
   * Utilities have private constructors
   */
  private SafeDesktop() {
  }

  /**
   * <p>A safe method to make the best attempts at opening a browser to the given URI</p>
   * <p>Callers do not need to log failures</p>
   *
   * @param uri The URI to pass to the browser
   *
   * @return True if the browser opened successfully (i.e. no visible error to the JVM)
   */
  public static boolean browse(final URI uri) {

    if (Desktop.isDesktopSupported()) {

      try {
        Desktop desktop = Desktop.getDesktop();
        if (desktop != null) {
          Desktop.getDesktop().browse(uri);
          // Assume success
          return true;
        }

        // Consider using xdg

      } catch (RuntimeException | IOException e) {
        // Probably a weird setup - just stop now
        log.warn("Failed to open browser.", e);
      }


    } else {
      // GNOME libraries are probably not available check OS
      if (!OSUtils.isLinux()) {
        // This is Windows/OS X so probably a weird setup - just stop now
        log.warn("No default browser configured.");
        return false;
      }

      // Might have a chance with KDE libraries since we're on Linux
      log.warn("Attempting to open browser with xdg-open");
      try {
        new ProcessBuilder(
          "xdg-open",
          uri.toString()
        ).start();

        // Need to include a call to waitFor() and examine the exit code to
        // accurately determine success. This requires wrapping the code in
        // a timeout executor which is more complex than it's worth at present

        // Assume success
        return true;

      } catch (IOException e) {
        log.warn("Failed to open external browser through 'xdg-open {}'", uri.toString(), e);
      }

    }

    // Must have failed to be here
    return false;
  }
}
