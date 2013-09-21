package org.multibit.hardwarewallet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Default class used to wrap an actual hardware wallet and keep track of its state
 */
public  class DefaultHardwareWallet implements HardwareWallet {

    private Logger log = LoggerFactory.getLogger(DefaultHardwareWallet.class);

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

    private Trezor implementation;

    public DefaultHardwareWallet(Trezor trezor) {
        implementation = trezor;
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
        if (implementation != null) {
            // In real life the Trezor would be initialised and metadata returned back.
            // Here we just wait a second and then say we are initialised and set the serialId.
            ExecutorService executorService = Executors.newSingleThreadExecutor();

            executorService.submit(new Runnable() {
                @Override
                public void run() {
                    // Pretend it takes a second to initialise.
                    try {
                        Thread.sleep(1000);
                        log.debug("TESTCODE : Saying that we always initialise successfully and have serialId = " + DUMMY_SERIAL_ID) ;

                        serialId = DUMMY_SERIAL_ID;
                        initialised = true;
                    } catch (InterruptedException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                }
            });
        }
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
     *
     * @return Trezor The physical device that this class wraps.
     * (In the future this will be a more general class, or perhaps Object)
     */
    public Trezor getImplementation() {
        return implementation;
    }
}
