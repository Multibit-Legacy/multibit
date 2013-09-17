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

import com.google.common.base.Preconditions;
import com.google.common.collect.Sets;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.TrezorListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;


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
     * The trezor device that is currently connected (may be null).
     */
    private Trezor trezor;


    /**
     * Listeners to the known trezors.
     */
    protected Set<TrezorListener> listeners;

    private Logger log = LoggerFactory.getLogger(TrezorManager.class);

    public void initialise(BitcoinController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        listeners = Sets.newLinkedHashSet();
    }

    public Trezor getTrezor() {
        return trezor;
    }

    /**
     * A utility method to create a MockTrezor device - this is a software emulation
     * of a real Trezor.
     *
     * @return a Trezor (will actually be a MockTrezor)
     */
    public Trezor createMockTrezor() {
        trezor = MockTrezorFactory.newMockTrezor();
        return trezor;
    }

    public void addListener(TrezorListener trezorListener) {
        if (trezor != null) {
            trezor.addListener(trezorListener);
        }
    }

    public void removeListener(TrezorListener trezorListener) {
        if (trezor != null) {
            trezor.removeListener(trezorListener);
        }
    }
}