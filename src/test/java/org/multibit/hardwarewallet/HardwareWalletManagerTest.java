/**
 * Copyright 2013 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.hardwarewallet;

import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;
import org.multibit.CreateControllers;
import org.multibit.Localiser;
import org.multibit.controller.bitcoin.BitcoinController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;

public class HardwareWalletManagerTest extends TestCase {
    BitcoinController controller;
    
    @Before
    public void setUp() throws Exception {
        // Create MultiBit controller
        final Localiser localiser = new Localiser();
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(localiser);
        controller = controllers.bitcoinController;
    }

    @Test
    public void testHardwareWalletManagerBasic() throws Exception {
        // Initialise.
        HardwareWalletManager hardwareWalletManager = HardwareWalletManager.INSTANCE;
        assertNotNull(hardwareWalletManager);

        hardwareWalletManager.initialise(controller);

        // At construction there is no hardware wallet present.
        assertNull("HardwareWallet should be null at construction",  hardwareWalletManager.getHardwareWallet());

        // Create a HardwareWallet object. This also wires up the HardwareWalletManager to listen for trezor events.
        HardwareWallet hardwareWallet = hardwareWalletManager.createMockTrezor();
        Trezor mockTrezor = hardwareWallet.getImplementation();

        assertNotNull("No mockTrezor device was created", mockTrezor);
        assertFalse("HardwareWallet was in wrong state at creation", hardwareWallet.isConnected());

        // Add a hardwareWalletListener to the manager.
        TestHardwareWalletListener hardwareWalletListener = new TestHardwareWalletListener();
        assertFalse("hardwareWalletListener was in the wrong connected state at creation", hardwareWalletListener.connected);
        assertFalse("hardwareWalletListener was in the wrong initialisation state at creation", hardwareWalletListener.initialised);
        hardwareWalletManager.addListener(hardwareWalletListener);

        // Connect up the mockTrezor - this is the physical equivalent of plugging in a Trezor.
        mockTrezor.connect();

        // Let events propagate.
        Thread.sleep(10);

        // HardwareWallet should be connected.
        assertTrue("HardwareWallet did not detect a connection event", hardwareWallet.isConnected());
        //assertTrue("hardwareWalletListener was in the wrong connected state after connection", hardwareWalletListener.connected);
        //assertFalse("hardwareWalletListener was in the wrong initialisation after connection", hardwareWalletListener.initialised);

        // No serial id is available yet as the device is not initialised.
        assertNull("No serial id should be available", hardwareWallet.getSerialId());

        // Initialise the device.
        hardwareWallet.initialise();

        // After some period of time the wallet should be initialised.
        // Give it 4 seconds (should really listen for hasInitalised event on HardwareWalletListener
        Thread.sleep(4000);

        // HardwareWallet should be initialised.
        assertTrue("HardwareWallet did not initialise", hardwareWallet.isInitialised());

        // It should now have a non-missing serial id
        assertNotNull("Serial id should be available", hardwareWallet.getSerialId());

        // Close the Trezor.
        // This is the physical equivalent of removing a Trezor device.
        mockTrezor.close();

        // Let events propagate.
        Thread.sleep(10);

        // HardwareWallet should be disconnected
        assertFalse("HardwareWallet did not detect a disconnection event", hardwareWallet.isConnected());
        // assertFalse("hardwareWalletListener was in the wrong connected state after disconnection", hardwareWalletListener.connected);

        // Remove the hardwareWalletListener.
        hardwareWalletManager.removeListener(hardwareWalletListener);

        // Destroy the trezor device
        hardwareWalletManager.destroyMockTrezor();
        assertNull("HardwareWallet should be null after destruction",  hardwareWalletManager.getHardwareWallet());
    }

    class TestHardwareWalletListener implements HardwareWalletListener {
        private Logger log = LoggerFactory.getLogger(TestHardwareWalletListener.class);

        public boolean connected = false;
        public boolean initialised = false;

        @Override
        public void hasConnected(HardwareWallet hardwareWallet) {
            connected = true;
            log.debug("Trezor " + hardwareWallet.getImplementation().toString() + " has connected.");
        }

        @Override
        public void hasDisconnected(HardwareWallet hardwareWallet) {
            connected = false;
            log.debug("Trezor " + hardwareWallet.getImplementation().toString() + " has disconnected.");
        }

        @Override
        public void hasInitialised(HardwareWallet hardwareWallet) {
           initialised = true;
        }
    }
}
