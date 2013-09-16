package org.multibit.trezor;

import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.protobuf.AbstractMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.trezors.AbstractTrezor;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * A mock Trezor device that you can use to loop back events to the MultiBit UI.
 */
public class MockTrezor extends AbstractTrezor implements Trezor {

    private static final Integer DEFAULT_USB_VENDOR_ID = 0x10c4;
    private static final Integer DEFAULT_USB_PRODUCT_ID = 0xea80;

    private static final Logger log = LoggerFactory.getLogger(MockTrezor.class);

    private Optional<Integer> vendorIdOptional = Optional.absent();
    private Optional<Integer> productIdOptional = Optional.absent();
    private Optional<String> serialNumberOptional = Optional.absent();

    /*
     * The DataOutputStream that is writing TO the Trezor emulator.
     */
    private DataOutputStream out = null;

    /**
     * THe DataInputStream that is being read FROM the Trezor emulator.
     */
    private DataInputStream in = null;

    private TrezorEmulator device = null;

    /**
     * <p>Create a new instance of a Mock Trezor device (standard)</p>
     *
     * @param vendorIdOptional     The vendor ID (default is 0x10c4)
     * @param productIdOptional    The product ID (default is 0xea80)
     * @param serialNumberOptional The device serial number (default is to accept any)
     */
    public MockTrezor(Optional<Integer> vendorIdOptional,
                      Optional<Integer> productIdOptional,
                      Optional<String> serialNumberOptional) {

        this.vendorIdOptional = vendorIdOptional;
        this.productIdOptional = productIdOptional;
        this.serialNumberOptional = serialNumberOptional;

    }

    @Override
    public synchronized void connect() {

        Preconditions.checkState(device == null, "Device is already connected");

        device = new TrezorEmulator();

        // Add unbuffered data streams for easy data manipulation
        out = new DataOutputStream(device.getOutputStream());
        DataInputStream in = new DataInputStream(device.getInputStream());

        // Monitor the input stream
        monitorDataInputStream(in);
    }

    @Override
    public synchronized void internalClose() {
        Preconditions.checkNotNull(device, "Device is not connected");

        // Throw away the TrezorEmulator.
        device = null;
    }

    @Override
    public void sendMessage(AbstractMessage message) throws IOException {

        Preconditions.checkNotNull(message, "Message must be present");
        Preconditions.checkNotNull(device, "Device is not connected");

        // Log the message being sent to the TrezorEmulator
        log.debug("Sending to TrezorEmulator the message '" + message.toString() + "'");

        // Apply the message to
        writeMessage(message, out);
     }

}
