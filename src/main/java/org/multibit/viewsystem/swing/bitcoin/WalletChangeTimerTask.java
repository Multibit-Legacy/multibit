/**
 * Copyright 2011 multibit.org
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
package org.multibit.viewsystem.swing.bitcoin;

import java.util.List;
import java.util.TimerTask;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.store.WalletVersionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * TimerTask to detect whether wallet files have been changed by some external
 * process and to save dirty files.
 * 
 * @see java.util.Timer
 * @see java.util.TimerTask
 */
public class WalletChangeTimerTask extends TimerTask {

    public static final int INITIAL_DELAY = 10000; // milliseconds
    public static final int DEFAULT_REPEAT_RATE = 120000; // milliseconds

    private static Logger log = LoggerFactory.getLogger(WalletChangeTimerTask.class);

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private boolean enable = true;

    /**
     * Constructs the object, sets the string to be output in function run()
     * @param str
     */
    public WalletChangeTimerTask(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
    }

    /**
     * When the timer executes, this code is run.
     */
    @Override
    public void run() {
        log.debug("Start of FileChangeTimerTask - run - enable = " + enable);
        if (enable) {
            List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();

            if (perWalletModelDataList != null) {
                for (WalletData loopModelData : perWalletModelDataList) {
                    if (this.bitcoinController.getFileHandler() != null) {
                        // See if the files have been changed by another process
                        // (non MultiBit).
                        boolean haveFilesChanged = this.bitcoinController.getFileHandler().haveFilesChanged(loopModelData);
                        if (haveFilesChanged) {
                            boolean previousFilesHaveBeenChanged = loopModelData.isFilesHaveBeenChangedByAnotherProcess();
                            loopModelData.setFilesHaveBeenChangedByAnotherProcess(true);
                            if (!previousFilesHaveBeenChanged) {
                                // only fire once, when change happens
                                this.bitcoinController.fireFilesHaveBeenChangedByAnotherProcess(loopModelData);
                                log.debug("Marking wallet " + loopModelData.getWalletFilename()
                                        + " as having been changed by another process.");
                            }
                        }

                        // See if they are dirty - write out if so.
                        if (loopModelData.isDirty()) {
                            log.debug("Saving dirty wallet '" + loopModelData.getWalletFilename() + "'...");
                            try {
                                this.bitcoinController.getFileHandler().savePerWalletModelData(loopModelData, false);
                                log.debug("... done.");
                            } catch (WalletSaveException e) {
                                String message = controller.getLocaliser().getString(
                                        "createNewWalletAction.walletCouldNotBeCreated",
                                        new Object[] { loopModelData.getWalletFilename(), e.getMessage() });
                                log.error(message);
                                MessageManager.INSTANCE.addMessage(new Message(message));
                            } catch (WalletVersionException e) {
                                String message = controller.getLocaliser().getString(
                                        "createNewWalletAction.walletCouldNotBeCreated",
                                        new Object[] { loopModelData.getWalletFilename(), e.getMessage() });
                                log.error(message);
                                MessageManager.INSTANCE.addMessage(new Message(message));
                            }
                        }
                    }
                }
            }
        }

        log.debug("End of FileChangeTimerTask - run");
    }
}