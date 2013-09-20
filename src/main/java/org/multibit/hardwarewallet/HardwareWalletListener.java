package org.multibit.hardwarewallet;

import uk.co.bsol.trezorj.core.Trezor;

/**
 * Listener interface to listen to events from a hardware wallet.
 */
public interface HardwareWalletListener {

    /**
     * A hardware wallet has connected to the host machine.
     * @returns hardwareWallet the hardware wallet that has connected.
     */
    public void hasConnected(HardwareWallet hardwareWallet);

    /**
     * A hardware wallet has disconnected from the host machine.
     * @returns hardwareWallet the hardware wallet that has disconnected.
     */
    public void hasDisconnected(HardwareWallet hardwareWallet);

    /**
     * The hardware wallet is now initialised and available for use
     */
    public void hasInitialised(HardwareWallet hardwareWallet);
}
