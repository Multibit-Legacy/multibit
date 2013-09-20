package org.multibit.hardwarewallet;

/**
 * Listener interface to listen to events from a hardware wallet.
 */
public interface HardwareWalletListener {

    /**
     * A hardware wallet has connected to the host machine.
     * @param hardwareWallet the hardware wallet that has connected.
     */
    public void hasConnected(HardwareWallet hardwareWallet);

    /**
     * A hardware wallet has disconnected from the host machine.
     * @param hardwareWallet the hardware wallet that has disconnected.
     */
    public void hasDisconnected(HardwareWallet hardwareWallet);

    /**
     * The hardware wallet is now initialised and available for use
     * @param hardwareWallet THe hardwareWallet that has just initialised.
     */
    public void hasInitialised(HardwareWallet hardwareWallet);
}
