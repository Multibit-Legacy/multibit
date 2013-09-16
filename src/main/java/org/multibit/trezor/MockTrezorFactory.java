package org.multibit.trezor;

/**
 * Class to create a Mock Trezor device.
 * You can use this to loop back Trezor events back to MultiBit to drive the UI.
 *
 */

import com.google.common.base.Optional;

public class MockTrezorFactory {

    /**
     * Utilities do not require a public constructor
     */
    private MockTrezorFactory() {
    }

    /**
     * <p>Create a new instance of a mock Trezor device (standard)</p>
     * @param vendorIdOptional The vendor ID (default is 0x10c4)
     * @param productIdOptional The product ID (default is 0xea80)
     * @param serialNumberOptional The device serial number (default is to accept any)
     * @return mock Trezor
     */
    public static MockTrezor newMockTrezor(Optional<Integer> vendorIdOptional, Optional<Integer> productIdOptional, Optional<String> serialNumberOptional) {

        return new MockTrezor(vendorIdOptional, productIdOptional, serialNumberOptional);
    }

}

