package org.multibit.hardwarewallet;

import uk.co.bsol.trezorj.core.Trezor;

/**
 * Listener interface to listen to events from a hardware wallet.
 * (In the future a more generic class than Trezor will be used for the hardware wallet instance).
 */
public interface HardwareWalletListener {

    /**
     * A hardware wallet has connected to the host machine.
     * @returns Trezor the hardware wallet that has connected.
     */
    public void hardwareWalletHasConnected(Trezor trezor);

    /**
     * A hardware wallet has disconnected from the host machine.
     * @returns Trezor the hardware wallet that has disconnected.
     */
    public void hardwareWalletHasDisconnected(Trezor trezor);

}
