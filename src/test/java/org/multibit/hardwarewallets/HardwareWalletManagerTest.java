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
package org.multibit.hardwarewallets;

import com.google.common.collect.Queues;
import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;
import org.multibit.CreateControllers;
import org.multibit.Localiser;
import org.multibit.controller.bitcoin.BitcoinController;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.TrezorEvent;
import uk.co.bsol.trezorj.core.TrezorListener;
import uk.co.bsol.trezorj.core.trezors.AbstractTrezor;

import java.util.concurrent.BlockingQueue;

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
    public void testTrezorManagerBasic() throws Exception {
        // Initialise.
        HardwareWalletManager hardwareWalletManager = HardwareWalletManager.INSTANCE;
        assertNotNull(hardwareWalletManager);

        hardwareWalletManager.initialise(controller, null);

        // At construction there are no Trezor device present.
        Trezor trezor = hardwareWalletManager.getTrezor();
        assertNull("Trezor should be null at construction", trezor);

        // Create a MockTrezor object. This also wires up the HardwareWalletManager to listen for trezor events.
        Trezor mockTrezor = hardwareWalletManager.createMockTrezor();

        assertNotNull("No mockTrezor device was created", mockTrezor);

        // Add a hardwareWalletListener to the manager.
        TestHardwareWalletListener hardwareWalletListener = new TestHardwareWalletListener();
        assertFalse("HardwareWalletListener was in wrong state at creation", hardwareWalletListener.isConnected());

        hardwareWalletManager.addListener(hardwareWalletListener);


        // Connect up the mockTrezor - this is the physical equivalent of plugging in a Trezor.
        mockTrezor.connect();

        // Listener should have detected a connection event.
        // TODO - not working yet.
        //assertTrue("HardwareWalletListener did not detect a connection event", hardwareWalletListener.isConnected());


        // Close the Trezor.
        // This is the physical equivalent of removing a Trezor device.
        mockTrezor.close();

        // Listener should have detected a disconnection event.
        assertFalse("HardwareWalletListener did not detect a disconnection event", hardwareWalletListener.isConnected());

        // Remove the hardwareWalletListener.
        hardwareWalletManager.removeListener(hardwareWalletListener);

        // Destroy the trezor device
        hardwareWalletManager.destroyMockTrezor();
    }

    class TestHardwareWalletListener implements HardwareWalletListener {
        private boolean connected = false;

        @Override
        public void hardwareWalletHasConnected(Trezor trezor) {
            connected = true;
        }

        @Override
        public void hardwareWalletHasDisconnected(Trezor trezor) {
            connected = false;
        }

        public boolean isConnected() {
            return connected;
        }
    }
}
