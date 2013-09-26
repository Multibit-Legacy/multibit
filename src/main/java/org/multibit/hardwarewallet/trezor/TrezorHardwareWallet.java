package org.multibit.hardwarewallet.trezor;

import org.multibit.hardwarewallet.HardwareWallet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.clients.TrezorClient;
import uk.co.bsol.trezorj.core.clients.TrezorClients;

/**
 * <p>[Pattern] to provide the following to {@link Object}:</p>
 * <ul>
 * <li></li>
 * </ul>
 * <p>Example:</p>
 * <pre>
 * </pre>
 *
 * @since 0.0.1
 *         
 */
public class TrezorHardwareWallet implements HardwareWallet {

    private Logger log = LoggerFactory.getLogger(TrezorHardwareWallet.class);

    private static final String DUMMY_SERIAL_ID = "123456";
    /**
     * Whether or not the actual device is connected or not.
     */
    private boolean connected;

    /**
     * Whether or not the actual device is initialised or not.
     */
    private boolean initialised;

    /**
     * The serial id for the device
     */
    private String serialId;

    private TrezorClient trezorClient;

    public TrezorHardwareWallet(Trezor trezor) {

        this.trezorClient = TrezorClients.newNonBlockingSocketInstance(
          "localhost",
          3000,
          TrezorClients.newSessionId());

        initialised = false;
    }

    @Override
    public void setConnected(boolean isConnected) {
        connected = isConnected;
    }

    @Override
    public boolean isConnected() {
        return connected;
    }

    @Override
    public void initialise() {

        //trezorClient.initialize();

    }

    /**
     * Boolean indicating whether the device is initialised or not
     */
    @Override
    public boolean isInitialised() {
        return initialised;
    }

    @Override
    public void setSerialId(String serialId) {
        this.serialId = serialId;
    }

    @Override
    public String getSerialId() {
        return serialId;
    }

    @Override
    public void setInitialised(boolean initialised) {
        this.initialised = initialised;
    }

    /**
     * @return Trezor The physical device that this class wraps.
     *         (In the future this will be a more general class, or perhaps Object)
     */
    public Trezor getTrezorClient() {
        return null;
    }
}
