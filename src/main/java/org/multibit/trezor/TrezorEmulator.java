package org.multibit.trezor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * This class is an incomplete emulation of a Trezor device used by the MockTrezor
 * to loop back events for demos etc.
 */
public class TrezorEmulator {

    private OutputStream out;
    private InputStream in;

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
}
