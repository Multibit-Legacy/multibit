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

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;

import java.util.ArrayList;
import java.util.Collection;


/**
 * Class managing Trezor hardware devices that are plugged in to the computer on
 * which MultiBit is running.
 *
 * This class has responsibility for tracking the Trezor devices being plugged in and removed.
 * It is also responsible for message passing to the device(s).
 */
public enum TrezorManager {
    INSTANCE;

    private BitcoinController controller;
    private MultiBitFrame mainFrame;

    /**
     * The collection of trezor devices that are currently connected.
     */
    private Collection<Trezor> trezors;

    private Logger log = LoggerFactory.getLogger(TrezorManager.class);

    public void initialise(BitcoinController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        trezors = new ArrayList<Trezor>();
    }

    public Collection<Trezor> getTrezors() {
        return trezors;
    }

    public MockTrezor createMockTrezor() {
        MockTrezor mockTrezor = MockTrezorFactory.newMockTrezor();
        trezors.add(mockTrezor);
        return mockTrezor;
    }
}