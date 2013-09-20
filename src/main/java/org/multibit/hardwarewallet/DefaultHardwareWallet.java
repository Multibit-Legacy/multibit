package org.multibit.hardwarewallet;

import uk.co.bsol.trezorj.core.Trezor;

/**
 * Default class used to wrap an actual hardware wallet and keep track of its state
 */
public  class DefaultHardwareWallet implements HardwareWallet {

    /**
     * Whether or not the actual device is connected or not.
     */
    private boolean connected;

    /**
     * Whether or not the actual device is initialised or not.
     */
    private boolean initialised;

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
            // TODO connect and initialise device, setting the state returned such as serial id
            // Once initialised the initialised state flag should be set.
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
