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

import com.google.dogecoin.store.BlockStore;
import com.google.dogecoin.store.BlockStoreException;
import org.multibit.ApplicationInstanceManager;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.core.CoreController;
import org.multibit.file.BackupManager;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.core.CoreModel;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.HealthCheckTimerTask;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.List;

/**
 * Exit the application.
 */
public class ExitAction extends AbstractExitAction {

    private static final long serialVersionUID = 8784284740245520863L;
    
    private static final int MAXIMUM_TIME_TO_WAIT_FOR_HEALTH_CHECK_TASK = 10000; // ms
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
        String shuttingDownTitle = bitcoinController.getLocaliser().getString("multiBitFrame.title.shuttingDown");

        if (mainFrame != null) {
            mainFrame.setTitle(shuttingDownTitle);
               
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
               
            // If the HealthCheckTimerTask is running wait until it completes.
            HealthCheckTimerTask healthCheckTimerTask = mainFrame.getHealthCheckTimerTask();
            if (healthCheckTimerTask != null) {
                boolean breakout = false;
                int timeWaited = 0;
                
                while(healthCheckTimerTask.isRunning() && !breakout && timeWaited < MAXIMUM_TIME_TO_WAIT_FOR_HEALTH_CHECK_TASK) {
                    try {
                        log.debug("Waiting for healthCheckTimerTask to complete (waited so far = " + timeWaited + "). . .");
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

        if (bitcoinController != null && mainFrame != null) {
            // Save the current window state so we can restore it on next start
            bitcoinController.getModel().setUserPreference(CoreModel.PREVIOUS_WINDOW_SIZE_H, String.valueOf(mainFrame.getHeight()));
            bitcoinController.getModel().setUserPreference(CoreModel.PREVIOUS_WINDOW_SIZE_W, String.valueOf(mainFrame.getWidth()));
            bitcoinController.getModel().setUserPreference(CoreModel.PREVIOUS_WINDOW_POS_X, String.valueOf(mainFrame.getX()));
            bitcoinController.getModel().setUserPreference(CoreModel.PREVIOUS_WINDOW_POS_Y, String.valueOf(mainFrame.getY()));
            bitcoinController.getModel().setUserPreference(CoreModel.PREVIOUS_WINDOW_MAX, String.valueOf(mainFrame.getExtendedState()));
            bitcoinController.getModel().setUserPreference(CoreModel.PREVIOUS_WINDOW_TRAY, String.valueOf(mainFrame.isInTray()));
        }

        if (bitcoinController != null) {
            // Save all the wallets and put their filenames in the user preferences.
            List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();
            if (perWalletModelDataList != null) {
                for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                    try {
                        String titleText = shuttingDownTitle;
                        if (mainFrame != null) {
                            if (loopPerWalletModelData != null) {
                                titleText = bitcoinController.getLocaliser().getString("multiBitFrame.title.saving",
                                        new String[] { loopPerWalletModelData.getWalletDescription() });
                            }
                            if (EventQueue.isDispatchThread()) {
                                mainFrame.setTitle(titleText);
                            } else {
                                final String finalTitleText = titleText;
                                SwingUtilities.invokeLater(new Runnable() {
                                    @Override
                                    public void run() {
                                        mainFrame.setTitle(finalTitleText);
                                    }
                                });
                            }
                        }
                        bitcoinController.getFileHandler().savePerWalletModelData(loopPerWalletModelData, false);
                    } catch (WalletSaveException wse) {
                        log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                        MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));

                        // Save to backup.
                        try {
                            BackupManager.INSTANCE.backupPerWalletModelData(bitcoinController.getFileHandler(), loopPerWalletModelData);
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
        }

        log.debug("Shutting down Bitcoin URI checker ...");
        ApplicationInstanceManager.shutdownSocket();

        // Get rid of main display.
        if (mainFrame != null) {
            mainFrame.setVisible(false);
        }

        if (mainFrame != null) {
            mainFrame.dispose();
        }

        System.exit(0);
    }
}
