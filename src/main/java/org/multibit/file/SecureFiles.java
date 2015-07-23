package org.multibit.file;

import org.multibit.utils.OSUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.security.SecureRandom;

/**
 * <p>Utilties to provide the following to applications:</p>
 * <ul>
 * <li>Access to secure file operations (delete, create with access restrictions etc)</li>
 * </ul>
 * <p>Uses Java new I/O and Guava Files where possible</p>
 *
 * @since 0.0.1
 */
public class SecureFiles {

  private static final Logger log = LoggerFactory.getLogger(SecureFiles.class);

  private static SecureRandom secureRandom = new SecureRandom();

  private static boolean initialised = false;

   // Nonsense bytes to fill up deleted files - these have no meaning.
  static final byte[] NONSENSE_BYTES = new byte[]{(byte) 0xF0, (byte) 0xA6, (byte) 0x55, (byte) 0xAA, (byte) 0x33,
    (byte) 0x77, (byte) 0x33, (byte) 0x37, (byte) 0x12, (byte) 0x34, (byte) 0x56, (byte) 0x78, (byte) 0xC2, (byte) 0xB3,
    (byte) 0xA4, (byte) 0x9A, (byte) 0x30, (byte) 0x7F, (byte) 0xE5, (byte) 0x5A, (byte) 0x23, (byte) 0x47, (byte) 0x13,
    (byte) 0x17, (byte) 0x15, (byte) 0x32, (byte) 0x5C, (byte) 0x77, (byte) 0xC9, (byte) 0x73, (byte) 0x04, (byte) 0x2D,
    (byte) 0x40, (byte) 0x0F, (byte) 0xA5, (byte) 0xA6, (byte) 0x43, (byte) 0x77, (byte) 0x33, (byte) 0x3B, (byte) 0x62,
    (byte) 0x34, (byte) 0xB6, (byte) 0x72, (byte) 0x32, (byte) 0xB3, (byte) 0xA4, (byte) 0x4B, (byte) 0x80, (byte) 0x7F,
    (byte) 0xC5, (byte) 0x43, (byte) 0x23, (byte) 0x47, (byte) 0x13, (byte) 0xB7, (byte) 0xA5, (byte) 0x32, (byte) 0xDC,
    (byte) 0x79, (byte) 0x19, (byte) 0xB1, (byte) 0x03, (byte) 0x9D};
  static final int BULKING_UP_FACTOR = 16;
  static final byte[] SECURE_DELETE_FILL_BYTES = new byte[NONSENSE_BYTES.length * BULKING_UP_FACTOR];

  private static void initialise() {
    // Make some SECURE_DELETE_FILL_BYTES bytes = x BULKING_UP_FACTOR the
    // NONSENSE just to save write time.
    for (int i = 0; i < BULKING_UP_FACTOR; i++) {
      System.arraycopy(
        NONSENSE_BYTES, 0,
        SECURE_DELETE_FILL_BYTES, NONSENSE_BYTES.length * i,
        NONSENSE_BYTES.length);
    }
    initialised = true;
  }

  /**
   * Utilities have private constructor
   */
  private SecureFiles() {
  }

  /**
   * Delete a file with an overwrite of all of the data.
   * <p/>
   * Set bit patterns are used rather than random numbers to avoid a
   * futex_wait_queue_me error on Linux systems (related to /dev/random usage)
   *
   * @param file The file to secure delete
   * @throws java.io.IOException if the operation fails for any reason
   */
  public static synchronized void secureDelete(File file) throws IOException {
    if (!initialised) {
      initialise();
    }

    log.trace("Start of secureDelete");

    if (OSUtils.isWindows()) {
      // Use slow secure delete
      slowSecureDelete(file);
    } else {
      fastSecureDelete(file);
    }
    log.trace("End of secureDelete");
  }

  /**
     * Delete a file with an overwrite of all of the data.
     * <p/>
     * Set bit patterns are used rather than random numbers to avoid a
     * futex_wait_queue_me error on Linux systems (related to /dev/random usage)
     *
     * @param file The file to secure delete
     *
     * @throws java.io.IOException if the operation fails for any reason
     */
    public static synchronized void slowSecureDelete(File file) throws IOException {
      if (file != null && file.exists()) {
        try (RandomAccessFile raf = new RandomAccessFile(file, "rws")) {
          // Prep for file delete as this can be fiddly on Windows
          // Make sure it is writable and any references to it are garbage
          // collected and finalized.
          if (!file.setWritable(true)) {
            throw new IOException("Could not write to file " + file.getAbsolutePath());
          }
          System.gc();

          long length = file.length();
          raf.seek(0);
          raf.getFilePointer();
          int pos = 0;
          while (pos < length) {
            raf.write(SECURE_DELETE_FILL_BYTES);
            pos += SECURE_DELETE_FILL_BYTES.length;
          }
        }
        boolean deleteSuccess = file.delete();
        log.trace("Result of delete of file '" + file.getAbsolutePath() + "' was " + deleteSuccess);
      }
    }

  /**
   * An alternative secure delete algorithm from http://www.cafeaulait.org/books/javaio2/ioexamples/14/SecureDelete.java
   *
   * @param file the file to secure delete
   */
  private static void fastSecureDelete(File file) throws IOException {
    if (file != null && file.exists()) {
      RandomAccessFile raf = null;
      FileChannel channel = null;
      MappedByteBuffer buffer;
      try {
        raf = new RandomAccessFile(file, "rw");
        channel = raf.getChannel();

        buffer = channel.map(
                FileChannel.MapMode.READ_WRITE,
                0,
                raf.length()
        );

        // Overwrite with random data; one byte at a time
        byte[] data = new byte[1];
        while (buffer.hasRemaining()) {
          secureRandom.nextBytes(data);
          buffer.put(data[0]);
        }

        // Ensure we push this out to the file system
        buffer.force();
        channel.close();
      } finally {
        buffer = null;

        if (channel != null) {
          channel.close();
          channel = null;
        }
        if (raf != null) {
          raf.close();
          raf = null;
        }
      }

      // Delete file
      // Use JDK7 NIO Files to delete the file since it offers the following benefits:
      // * best chance at an atomic operation
      // * relies on native code
      // * works on Windows
      boolean deleteSuccess = Files.deleteIfExists(file.toPath());
      log.trace("Result of initial delete was {} for:\n'{}'", deleteSuccess, file.getAbsolutePath());

      if (OSUtils.isWindows()) {
        // Work around an issue on Windows whereby files are not deleted
        File canonical = file.getCanonicalFile();
        if (canonical.exists() && !canonical.delete())
          log.debug("Failed to delete canonical file {}", file.getCanonicalPath());
      }
    }
  }
}
