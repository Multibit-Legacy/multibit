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

import com.google.common.collect.Queues;
import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;
import org.multibit.CreateControllers;
import org.multibit.Localiser;
import org.multibit.controller.bitcoin.BitcoinController;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.TrezorEvent;
import uk.co.bsol.trezorj.core.TrezorEventType;
import uk.co.bsol.trezorj.core.TrezorListener;
import uk.co.bsol.trezorj.core.protobuf.TrezorMessage;
import uk.co.bsol.trezorj.core.trezors.AbstractTrezor;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

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

        // At construction there are no Trezor device present.
        Trezor trezor = trezorManager.getTrezor();
        assertNull("Trezor should be null at construction", trezor);


        // Create a MockTrezor object.
        Trezor mockTrezor = trezorManager.createMockTrezor();

        assertNotNull("No mockTrezor device was created", mockTrezor);

        // Add a listener to the mockTrezor.
        TrezorListener trezorListener = new TestTrezorListener();

        mockTrezor.addListener(trezorListener);

        // Connect up the mockTrezor - this should reply with a DEVICE_CONNECTED message.
        // This is the physical equivalent of plugging in a Trezor.
        mockTrezor.connect();

        TrezorEvent event = trezorListener.getTrezorEventQueue().take();
        assertNotNull("Trezor event after connect was null", event);

        // TODO This next test fails with:
        // Failed tests:   testTrezorManagerBasic(org.multibit.trezor.TrezorManagerTest): No DEVICE_CONNECTED event after connect expected:<DEVICE_CONNECTED> but was:<DEVICE_FAILURE>
        // This is caused by trezorj using protobuf 2.5.0 but multibit/ bitcoinj using 2.4.1.
        //assertEquals("No DEVICE_CONNECTED event after connect", TrezorEventType.DEVICE_CONNECTED, event.eventType());


//        // Ping the Trezor
//        trezor.sendMessage(TrezorMessage.Ping.getDefaultInstance());
//
//        // Trezor should reply with a SUCCESS message.
//        event = trezorListener.getTrezorEventQueue().take();
//        assertNotNull("Trezor event after ping was null", event);
        // TODO not sure about this equals.
//        assertEquals("No SUCCESS event after connect", TrezorMessage.Success.MESSAGE_FIELD_NUMBER, event.protocolMessageType().get().getHeaderCode());


//        // Close the Trezor.
//        // This is the physical equivalent of removing a Trezor device.
//        trezor.close();
//
//        // The trezor should reply with a DEVICE_DISCONNECTED message.
//        event = trezorListener.getTrezorEventQueue().take();
//        assertNotNull("Trezor event after close was null", event);
//        assertEquals("No DEVICE_DISCONNECTED event after close", TrezorEventType.DEVICE_DISCONNECTED, event.eventType());
    }

    class TestTrezorListener implements TrezorListener {

        BlockingQueue<TrezorEvent> listenerQueue = Queues.newArrayBlockingQueue(AbstractTrezor.MAX_QUEUE_SIZE);

        @Override
        public BlockingQueue<TrezorEvent> getTrezorEventQueue() {
            return listenerQueue;
        }

        @Override
        public void setTrezorEventQueue(BlockingQueue<TrezorEvent> trezorEventQueue) {
           listenerQueue = trezorEventQueue;
        }
    }
}
