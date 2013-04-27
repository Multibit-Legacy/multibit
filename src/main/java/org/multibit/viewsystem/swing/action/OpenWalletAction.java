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
package org.multibit.viewsystem.swing.action;

import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.SwingWorker;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.network.MultiBitCheckpointManager;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.WalletFileFilter;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanel;
import org.multibit.viewsystem.swing.view.walletlist.WalletListPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionConfidence;
import com.google.bitcoin.core.Wallet;

/**
 * This {@link Action} opens a wallet from a file.
 */
public class OpenWalletAction extends AbstractAction {

    public Logger log = LoggerFactory.getLogger(OpenWalletAction.class.getName());

    private static final long serialVersionUID = 1913592460523457705L;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private MultiBitFrame mainFrame;

    private JFileChooser fileChooser;
    
    private Font adjustedFont;

    private String selectedWalletFilename;

    /**
     * Creates a new {@link OpenWalletAction}.
     */
    public OpenWalletAction(BitcoinController bitcoinController, ImageIcon icon, MultiBitFrame mainFrame) {
        super(bitcoinController.getLocaliser().getString("openWalletAction.text"), icon);
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        this.mainFrame = mainFrame;
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("openWalletAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("openWalletAction.mnemonicKey"));
    }

    /**
     * Show open file chooser and load wallet.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            if (fileChooser == null) {
                JFileChooser.setDefaultLocale(controller.getLocaliser().getLocale());
                fileChooser = new JFileChooser();
                fileChooser.setLocale(controller.getLocaliser().getLocale());
                fileChooser.setDialogTitle(controller.getLocaliser().getString("openWalletAction.tooltip"));
                adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
                if (adjustedFont != null) {
                    setFileChooserFont(new Container[] {fileChooser});
                }
                fileChooser.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

                if (controller.getModel() != null && this.bitcoinController.getModel().getActiveWalletFilename() != null) {
                    fileChooser.setCurrentDirectory(new File(this.bitcoinController.getModel().getActiveWalletFilename()));
                }
                fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                fileChooser.setFileFilter(new WalletFileFilter(controller));
            }

            fileChooser.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            int returnVal = fileChooser.showOpenDialog(mainFrame);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));


            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = fileChooser.getSelectedFile();
                if (file != null) {
                    if (!file.isDirectory()) {
                        selectedWalletFilename = file.getAbsolutePath();
                        
                        // See if the wallet is already open.
                        boolean walletIsAlreadyOpen = false;
                        if (controller != null && controller.getModel() != null) {
                            List<WalletData> perWalletDataModels = bitcoinController.getModel().getPerWalletModelDataList();
                            if (perWalletDataModels != null) {
                                Iterator<WalletData> iterator = perWalletDataModels.iterator();
                                if (iterator != null) {
                                    while(iterator.hasNext()) {
                                        WalletData perWalletModelData = iterator.next();
                                        if (perWalletModelData != null && perWalletModelData.getWalletFilename() != null) {
                                            if (perWalletModelData.getWalletFilename().equals(selectedWalletFilename)) {
                                                walletIsAlreadyOpen = true;
                                                bitcoinController.getModel().setActiveWalletByFilename(selectedWalletFilename);
                                                controller.fireDataChangedUpdateNow();
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        
                        if (!walletIsAlreadyOpen) {
                            Message openMessage = new Message(controller.getLocaliser().getString("multiBit.openingWallet", new Object[]{selectedWalletFilename}));
                            openMessage.setShowInStatusBar(false);
                            MessageManager.INSTANCE.addMessage(openMessage);
                            openWalletInBackground(selectedWalletFilename);
                        }
                    }
                } else {
                    selectedWalletFilename = null;
                    fileChooser = null;
                }
            } else {
                selectedWalletFilename = null;
                fileChooser = null;
            }
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    /**
     * Open a wallet in a background Swing worker thread.
     * @param selectedWalletFilename Filename of wallet to open
     */
    private void openWalletInBackground(String selectedWalletFilename) {
        final String selectedWalletFilenameFinal = selectedWalletFilename;

        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            
            private String message = null;
            
            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    log.debug("Opening wallet '" + selectedWalletFilenameFinal + "'.");
                    bitcoinController.addWalletFromFilename(selectedWalletFilenameFinal);
 
                    log.debug("Setting active wallet for file '" + selectedWalletFilenameFinal + "'.");
                    bitcoinController.getModel().setActiveWalletByFilename(selectedWalletFilenameFinal);

                    // Save the user properties to disk.
                    log.debug("Writing user preferences. . .");
                    FileHandler.writeUserPreferences(bitcoinController);
                    log.debug("User preferences with new wallet written successfully");
 
                    message = controller.getLocaliser().getString("multiBit.openingWalletIsDone", new Object[]{selectedWalletFilenameFinal}); 
                    
                    return Boolean.TRUE;
                } catch (WalletLoadException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (WalletVersionException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (IOException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                }  catch (WalletSaveException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (Exception e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                }
            }
            
            @Override
            protected void done() {
                try {
                    Boolean wasSuccessful = get();
                    if (wasSuccessful) {
                        log.debug(message);
                        Message messageMessage = new Message(message);
                        messageMessage.setShowInStatusBar(false);
                        MessageManager.INSTANCE.addMessage(messageMessage);  
                        
                        // Work out the late date/ block the wallet saw to see if it needs syncing.
                        WalletData perWalletModelData = bitcoinController.getModel().getActivePerWalletModelData();
                        Wallet wallet = perWalletModelData.getWallet();
                        int lastBlockSeenHeight = wallet.getLastBlockSeenHeight();
                        log.debug("For wallet '" + perWalletModelData.getWalletFilename() + " the lastBlockSeenHeight was " + lastBlockSeenHeight);
                        
                        // If there was no lastBlockSeenHeight, find the latest confirmed transaction height.
                        if (lastBlockSeenHeight <= 0) {
                            Set<Transaction> transactions = perWalletModelData.getWallet().getTransactions(true, false);
                            if (transactions != null) {
                                for (Transaction transaction : transactions) {
                                    TransactionConfidence confidence = transaction.getConfidence();
                                    if (confidence != null) {
                                        if (TransactionConfidence.ConfidenceType.BUILDING.equals(confidence.getConfidenceType())) {
                                            lastBlockSeenHeight = Math.max(lastBlockSeenHeight, confidence.getAppearedAtChainHeight());
                                        }
                                    }
                                }
                            }                            
                        }
                        log.debug("For wallet '" + perWalletModelData.getWalletFilename() + ", after transactions, the lastBlockSeenHeight was " + lastBlockSeenHeight);

                        int currentChainHeight = -1;
                        if (bitcoinController.getMultiBitService().getChain() != null) {
                            if (bitcoinController.getMultiBitService().getChain().getChainHead() != null) {
                                currentChainHeight = bitcoinController.getMultiBitService().getChain().getChainHead().getHeight();
                            }
                        }
                        log.debug("The current chain height is " + currentChainHeight);
                        
                        boolean needToSync = false; 
                        long requiredSyncTimeInSeconds = -1;
                        
                        // Check if we have both the lastBlockSeenHeight and the currentChainHeight.
                        if (lastBlockSeenHeight > 0 && currentChainHeight > 0) {
                            if (lastBlockSeenHeight >= currentChainHeight) {
                                // Wallet is at or ahead of current chain - but check there isnt a replay at the moment.
                                // If there is, use the actual last chain height to check.
                                // (The user might have opened a wallet whilst a replay was occurring).
                                if (ReplayManager.INSTANCE.getCurrentReplayTask() != null) {
                                    int actualLastChainHeight = ReplayManager.INSTANCE.getActualLastChainHeight();
                                    if (lastBlockSeenHeight < actualLastChainHeight) {
                                        needToSync = true;
                                    }
                                }
                            } else {
                                // Wallet is behind the current chain - need to sync.
                                needToSync = true;
                            }
                        } else {
                            // If we still dont have a sync date/height, use the key birth date of the wallet
                            // and sync from that. This might be expensive, but hould only be used for empty
                            // wallets.
                            requiredSyncTimeInSeconds = wallet.getEarliestKeyCreationTime();
                            needToSync = true;
                            log.debug("requiredSyncTimeInSeconds worked out from earliestKeyCreationTime = " + requiredSyncTimeInSeconds);
                        }
                        
                        log.debug("needToSync = " + needToSync);

                        if (needToSync) {
                            StoredBlock syncFromStoredBlock = null;

                            MultiBitCheckpointManager checkpointManager = bitcoinController.getMultiBitService().getCheckpointManager();
                            if (checkpointManager != null) {
                                if (lastBlockSeenHeight > 0) {
                                    syncFromStoredBlock = checkpointManager.getCheckpointBeforeOrAtHeight(lastBlockSeenHeight);
                                } else {
                                    if (requiredSyncTimeInSeconds >= 0) {
                                        syncFromStoredBlock = checkpointManager.getCheckpointBefore(requiredSyncTimeInSeconds);
                                    }
                                }
                            }
                            log.debug("syncFromStoredBlock =" + syncFromStoredBlock);
                            
                            // Initialise the message in the singleWalletPanel.
                            if (mainFrame != null) {
                                WalletListPanel walletListPanel = mainFrame.getWalletsView();
                                if (walletListPanel != null) {
                                    SingleWalletPanel singleWalletPanel = walletListPanel.findWalletPanelByFilename(selectedWalletFilenameFinal);
                                    if (singleWalletPanel != null) {
                                        singleWalletPanel.setSyncMessage(controller.getLocaliser().getString("multiBitDownloadListener.downloadingTextShort"), Message.NOT_RELEVANT_PERCENTAGE_COMPLETE);
                                    }
                                }
                            }
                            List<WalletData> perWalletModelDataList = new ArrayList<WalletData>();
                            perWalletModelDataList.add(perWalletModelData);
                            ReplayTask replayTask;
                            if (syncFromStoredBlock == null) {
                                // Sync from genesis block.
                                replayTask = new ReplayTask(perWalletModelDataList, null, 0);
                            } else {
                                Date syncDate = null;
                                if (syncFromStoredBlock.getHeader() != null) {
                                    syncDate = new Date(syncFromStoredBlock.getHeader().getTimeSeconds() * 1000);
                                }
                                replayTask = new ReplayTask(perWalletModelDataList, syncDate, syncFromStoredBlock.getHeight());
                            }
                            ReplayManager.INSTANCE.offerReplayTask(replayTask);
                        }
                        controller.fireRecreateAllViews(true);
                    } else {
                        log.error(message);
                        MessageManager.INSTANCE.addMessage(new Message(message));
                        WalletData loopData = bitcoinController.getModel().getPerWalletModelDataByWalletFilename(selectedWalletFilenameFinal);
                        if (loopData != null) {
                            // Clear the backup wallet filename - this prevents it being automatically overwritten.
                            if (loopData.getWalletInfo() != null) {
                                loopData.getWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, "");
                            }
                        }
                    }
                } catch (Exception e) {
                    // Not really used but caught so that SwingWorker shuts down cleanly.
                    log.error(e.getClass() + " " + e.getMessage());
                } finally {
                    setEnabled(true);
                    if (mainFrame != null) {
                        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    }
                }
            }
        };
        log.debug("Executing open of wallet '" + selectedWalletFilenameFinal + "'.");
        worker.execute();
    }
    
    private void setFileChooserFont(Component[] comp) {
        for (int x = 0; x < comp.length; x++) {
            if (comp[x] instanceof Container) {
                setFileChooserFont(((Container) comp[x]).getComponents());
            }
            try {
                comp[x].setFont(adjustedFont);
            } catch (Exception e) {
                // Do nothing.
            }
        }
    }
}