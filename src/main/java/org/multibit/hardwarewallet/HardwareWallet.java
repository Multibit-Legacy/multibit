package org.multibit.hardwarewallet;

import uk.co.bsol.trezorj.core.Trezor;

/**
 * Interface denoting a hardware wallet.
 *
 * This is a generic interface hiding the implementation details of a specific type of hardware wallet.
 * You can listen to events coming from a HardwareWallet using a HardwareWalletListener.
 */
public interface HardwareWallet {

    /**
     * Set whether the device is physically connected or not.
     * This should only be used by the device layer - the user should not set and unset this.
     * @param isConnected
     */
    public void setConnected(boolean isConnected);

    /**
     * Indicates whether the device is connected or not.
     * 'Connected' means 'Is the device physically plugged into the host machine ?'
     */
    public boolean isConnected();

    /**
     * Initialise a hardware wallet.
     * This will start a session with the device and read any initial status information e.g. serial ID
     * Sometime later the device will become initialised and isInitialised will return true.
     */
    public void initialise();

    /**
     * Sets a hardware wallet to be initialised.
     * This should only be used by the device layer - the user should not set and unset this.
     */
    public void setInitialised(boolean isInitialised);

    /**
     * Indicates whether the device is initialised or not.
     */
    public boolean isInitialised();

    /**
     * Set the serial id.
     * This should only be used by the device layer.
     */
    public void setSerialId(String serialId);

    /**
     * Get the serial id of the device.
     * This is only available after initialisation - it will be null before then.
     */
    public String getSerialId();

    /**
     * Get the actual hardware wallet implementation.
     * TODO In future use a more generic implementation or perhaps Object
     */
    public Trezor getImplementation();
}
