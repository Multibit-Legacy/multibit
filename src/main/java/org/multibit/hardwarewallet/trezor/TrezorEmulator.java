package org.multibit.hardwarewallet.trezor;

import com.google.protobuf.AbstractMessage;
import com.google.protobuf.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.TrezorListener;

import java.io.*;

/**
 * This class is an almost empty emulator of a Trezor device used by the MockTrezor.
 * The emulation is done currently at the object level in MockTrezor.
 */
public class TrezorEmulator implements Trezor {

    private static Logger log = LoggerFactory.getLogger(TrezorEmulator.class);

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
            isConnected = true;
        }
    }

    @Override
    public void close() {
        isConnected = false;
    }

    @Override
    public void sendMessage(Message message) throws IOException {
    }

    @Override
    public void addListener(TrezorListener trezorListener) {
    }

    @Override
    public void removeListener(TrezorListener trezorListener) {
    }
}
