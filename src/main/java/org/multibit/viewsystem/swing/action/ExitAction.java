/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License
 * at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package org.multibit.viewsystem.swing.action;

import java.awt.Cursor;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.SwingUtilities;

import org.multibit.ApplicationInstanceManager;
import org.multibit.controller.Controller;
import org.multibit.controller.core.CoreController;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.FileChangeTimerTask;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;

/**
 * Exit the application.
 * 
 * @author jim
 * 
 */
public class ExitAction extends AbstractExitAction {

    private static final long serialVersionUID = 8784284740245520863L;
    
    private static final int MAXIMUM_TIME_TO_WAIT_FOR_FILE_CHANGE_TASK = 10000; // ms
    private static final int TIME_TO_WAIT = 200; // ms
    
    private final MultiBitFrame mainFrame;
    private static final Logger log = LoggerFactory.getLogger(ExitAction.class);

    private CoreController coreController = null;
    private BitcoinController bitcoinController = null;

    /**
     * Creates a new {@link ExitAction}.
     */
    public ExitAction(Controller controller, MultiBitFrame mainFrame) {
        super(controller);
        this.mainFrame = mainFrame;
    }

    public void setCoreController(CoreController coreController) {
        if (null == coreController) {
            this.coreController = coreController;
        }
    }

    public void setBitcoinController(BitcoinController bitcoinController) {
        if (null == this.bitcoinController) {
            this.bitcoinController = bitcoinController;
        }
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        // log.debug("exit 1");
        if (mainFrame != null) {
            if (EventQueue.isDispatchThread()) {
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    }
                });
            }

            // If the FileChangeTimerTask is running wait until it completes.
            FileChangeTimerTask fileChangeTimerTask = mainFrame.getFileChangeTimerTask();
            if (fileChangeTimerTask != null) {
                boolean breakout = false;
                int timeWaited = 0;
                
                while(fileChangeTimerTask.isRunning() && !breakout && timeWaited < MAXIMUM_TIME_TO_WAIT_FOR_FILE_CHANGE_TASK) {
                    try {
                        log.debug("Waiting for fileChangeTimerTask to complete (waited so far = " + timeWaited + "). . .");
                        Thread.sleep(TIME_TO_WAIT);
                        timeWaited = timeWaited + TIME_TO_WAIT;
                    } catch (InterruptedException e) {
                        breakout = true;
                        e.printStackTrace();
                    }
                }
            }
        }
        
        if (bitcoinController != null && bitcoinController.getMultiBitService() != null) {
            // Stop the peer group so that blocks are notified to wallets correctly.
            if (bitcoinController.getMultiBitService().getPeerGroup() != null) {
                log.debug("Closing Bitcoin network connection...");
                bitcoinController.getMultiBitService().getPeerGroup().stopAndWait();
                log.debug("PeerGroup is now stopped.");
            }

            // Close down the blockstore.
            BlockStore blockStore = bitcoinController.getMultiBitService().getBlockStore();
            if (blockStore != null) {
                try {
                    log.debug("Closing blockStore. . .");
                    blockStore.close();
                    blockStore = null;
                    log.debug("BlockStore closed successfully.");
                } catch (NullPointerException npe) {
                    log.error("NullPointerException on blockstore close");
                } catch (BlockStoreException e) {
                    log.error("BlockStoreException on blockstore close. Message was '" + e.getMessage() + "'");
                }
            }
        }

        if (bitcoinController != null) {
            // Save all the wallets and put their filenames in the user preferences.
            List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();
            if (perWalletModelDataList != null) {
                for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                    try {
                        // log.debug("exit 3a");
                        bitcoinController.getFileHandler().savePerWalletModelData(loopPerWalletModelData, false);
                        // log.debug("exit 3b");
                    } catch (WalletSaveException wse) {
                        log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                        MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));

                        // Save to backup.
                        try {
                            // log.debug("exit 4a");
                            bitcoinController.getFileHandler().backupPerWalletModelData(loopPerWalletModelData);
                            // log.debug("exit 4b");
                        } catch (WalletSaveException wse2) {
                            log.error(wse2.getClass().getCanonicalName() + " " + wse2.getMessage());
                            MessageManager.INSTANCE.addMessage(new Message(wse2.getClass().getCanonicalName() + " "
                                    + wse2.getMessage()));
                        }
                    } catch (WalletVersionException wve) {
                        log.error(wve.getClass().getCanonicalName() + " " + wve.getMessage());
                        MessageManager.INSTANCE.addMessage(new Message(wve.getClass().getCanonicalName() + " " + wve.getMessage()));
                    }
                }
            }

            // Write the user properties.
            log.debug("Saving user preferences ...");
            FileHandler.writeUserPreferences(bitcoinController);
            // log.debug("exit 6");
        }

        log.debug("Shutting down Bitcoin URI checker ...");
        ApplicationInstanceManager.shutdownSocket();
        // log.debug("exit 7");

        // Get rid of main display.
        if (mainFrame != null) {
            mainFrame.setVisible(false);
        }
        // log.debug("exit 8");

        if (mainFrame != null) {
            mainFrame.dispose();
        }
        // log.debug("exit 10");

        System.exit(0);
        // log.debug("exit 11");
    }
}
