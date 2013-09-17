package org.multibit.hardwarewallets.trezor;

import org.multibit.hardwarewallets.trezor.MockTrezor;

/**
 * Class to create a Mock Trezor device.
 * You can use this to loop back Trezor events back to MultiBit to drive the UI.
 *
 */

public class MockTrezorFactory {

    /**
     * Utilities do not require a public constructor
     */
    private MockTrezorFactory() {
    }

    /**
     * <p>Create a new instance of a mock Trezor device (standard)</p>
     * @return mock Trezor
     */
    public static MockTrezor newMockTrezor() {
        return new MockTrezor();
    }

}

