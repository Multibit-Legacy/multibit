package org.multibit.trezor;

import com.google.protobuf.AbstractMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.TrezorListener;

import java.io.*;

/**
 * This class is a emulator of a Trezor device used by the MockTrezor
 * to loop back events for demos etc.
 */
public class TrezorEmulator implements Trezor {

    private static Logger log = LoggerFactory.getLogger(TrezorEmulator.class);

    private static final byte[] TREZOR_SUCCESS_RESPONSE = new byte[]{33, 33, 0, 2, 0, 0, 0, 0};

    /**
     * The stream from the Trezor to the calling device.
     */
    private OutputStream out;

    /**
     * THe stream to the Trezor coming from the calling device.
     */
    private InputStream in;

    private boolean isConnected = false;

    private static final int RESPONSE_TIME = 50; // Add in a 50 milliseconds delay to all responses.

    private byte[] inputBytes;

    public TrezorEmulator() {
       out = new ByteArrayOutputStream(16384);

       inputBytes = new byte[16384];
       in = new ByteArrayInputStream(inputBytes);
    }

    public OutputStream getOutputStream() {
        return out;
    }

    public InputStream getInputStream() {
        return in;
    }

    @Override
    public void connect() {
        try {
            Thread.sleep(RESPONSE_TIME);
        } catch (InterruptedException e) {
            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
        }

        // You can always connect to a TrezorEmulator as long as it is not already connected.
        if (isConnected) {
            // TODO Reply with an error.
        } else {
            try {
                out.write(TREZOR_SUCCESS_RESPONSE);
            } catch (IOException e) {
                log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
            }
        }
    }

    @Override
    public void close() {
     }

    @Override
    public void sendMessage(AbstractMessage message) throws IOException {
    }

    @Override
    public void addListener(TrezorListener trezorListener) {
    }

    @Override
    public void removeListener(TrezorListener trezorListener) {
    }
}
