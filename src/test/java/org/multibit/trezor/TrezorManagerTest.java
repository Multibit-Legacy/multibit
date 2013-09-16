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
package org.multibit.trezor;

import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;
import org.multibit.CreateControllers;
import org.multibit.Localiser;
import org.multibit.controller.bitcoin.BitcoinController;
import uk.co.bsol.trezorj.core.Trezor;

import java.util.Collection;

public class TrezorManagerTest extends TestCase {
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
        TrezorManager trezorManager = TrezorManager.INSTANCE;
        assertNotNull(trezorManager);

        trezorManager.initialise(controller, null);

        // At construction there are no Trezor devices connected.
        Collection<Trezor> trezors = trezorManager.getTrezors();
        assertNotNull("Trezors should be empty, not null", trezors);

        assertEquals("There should initially be no Trezor devices available", 0, trezors.size());


        // Create a MockTrezor object.
        Trezor mockTrezor = trezorManager.createMockTrezor();

        assertNotNull("No mockTrezor device was created", mockTrezor);
        assertEquals("There were the wrong number of trezors under management", 1, trezorManager.getTrezors().size());

        // Connect up the mockTrezor - this should reply with a success message.
        // This is the physical equivalent of plugging in a Trezor.
        // TODO - hook up listener to listener for reply.

        mockTrezor.connect();

        // Close the Trezor.
        // This is the physical equivalent of removing a Trezor device.
    }
}
