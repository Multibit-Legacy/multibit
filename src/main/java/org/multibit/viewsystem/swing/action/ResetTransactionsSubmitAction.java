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

import com.google.bitcoin.core.Transaction;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.utils.DateUtils;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanel;
import org.multibit.viewsystem.swing.view.walletlist.WalletListPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * This {@link Action} resets the blockchain and transactions.
 */
public class ResetTransactionsSubmitAction extends MultiBitSubmitAction {

    private static final Logger log = LoggerFactory.getLogger(ResetTransactionsSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private static final int NUMBER_OF_MILLISECOND_IN_A_SECOND = 1000;

    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link ResetTransactionsSubmitAction}.
     */
    public ResetTransactionsSubmitAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, Icon icon) {
        super(bitcoinController, "resetTransactionsSubmitAction.text", "resetTransactionsSubmitAction.tooltip",
                "resetTransactionsSubmitAction.mnemonicKey", icon);
        this.mainFrame = mainFrame;
    }


    /**
     * Reset the transactions and replay the blockchain.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        if (abort()) {
            return;
        }

        setEnabled(false);

        WalletData activePerWalletModelData = super.bitcoinController.getModel().getActivePerWalletModelData();

            // Work out the earliest transaction date and save it to the wallet.
            Date earliestTransactionDate = new Date(DateUtils.nowUtc().getMillis());
            Set<Transaction> allTransactions = activePerWalletModelData.getWallet().getTransactions(true);
            if (allTransactions != null) {
                for (Transaction transaction : allTransactions) {
                    if (transaction != null) {
                        Date updateTime = transaction.getUpdateTime();
                        if (updateTime != null && earliestTransactionDate.after(updateTime)) {
                            earliestTransactionDate = updateTime;
                        }
                    }
                }
            }
            Date actualResetDate = earliestTransactionDate;

            // Look at the earliest key creation time - this is
            // returned in seconds and is converted to milliseconds.
            long earliestKeyCreationTime = activePerWalletModelData.getWallet().getEarliestKeyCreationTime()
                    * NUMBER_OF_MILLISECOND_IN_A_SECOND;
            if (earliestKeyCreationTime != 0 && earliestKeyCreationTime < earliestTransactionDate.getTime()) {
                earliestTransactionDate = new Date(earliestKeyCreationTime);
                actualResetDate = earliestTransactionDate;
            }

        // Take an extra day off the reset date to ensure the wallet is cleared entirely
        actualResetDate = new Date (actualResetDate.getTime() - 3600 * 24 * NUMBER_OF_MILLISECOND_IN_A_SECOND);  // Number of milliseconds in a day

        // Remove the transactions from the wallet.
        activePerWalletModelData.getWallet().clearTransactions(actualResetDate);

        // Save the wallet without the transactions.
        try {
            super.bitcoinController.getFileHandler().savePerWalletModelData(activePerWalletModelData, true);

            super.bitcoinController.getModel().createWalletTableData(super.bitcoinController, super.bitcoinController.getModel().getActiveWalletFilename());
            controller.fireRecreateAllViews(false);
        } catch (WalletSaveException wse) {
            log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
            MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
        }
        
        // Double check wallet is not busy then declare that the active wallet
        // is busy with the task
        WalletData perWalletModelData = this.bitcoinController.getModel().getActivePerWalletModelData();

        if (!perWalletModelData.isBusy()) {
            perWalletModelData.setBusy(true);
            perWalletModelData.setBusyTaskKey("resetTransactionsSubmitAction.text");
            perWalletModelData.setBusyTaskVerbKey("resetTransactionsSubmitAction.verb");

            super.bitcoinController.fireWalletBusyChange(true);

            resetTransactionsInBackground(actualResetDate, activePerWalletModelData.getWalletFilename());
        }

    }

    /**
     * Reset the transaction in a background Swing worker thread.
     */
    private void resetTransactionsInBackground(final Date resetDate, final String walletFilename) {
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {

            private String message = "";

            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure;

                log.debug("Starting replay from date = " + resetDate);
                List<WalletData> perWalletModelDataList = new ArrayList<WalletData>();
                perWalletModelDataList.add(bitcoinController.getModel().getActivePerWalletModelData());

                // Initialise the message in the SingleWalletPanel.
                if (mainFrame != null) {
                    WalletListPanel walletListPanel = mainFrame.getWalletsView();
                    if (walletListPanel != null) {
                        SingleWalletPanel singleWalletPanel = walletListPanel.findWalletPanelByFilename(walletFilename);
                        if (singleWalletPanel != null) {
                            singleWalletPanel.setSyncMessage(controller.getLocaliser().getString("resetTransactionsSubmitAction.verb"), Message.NOT_RELEVANT_PERCENTAGE_COMPLETE);
                        }
                    }
                }

                ReplayTask replayTask = new ReplayTask(perWalletModelDataList, resetDate, ReplayTask.UNKNOWN_START_HEIGHT);
                ReplayManager.INSTANCE.offerReplayTask(replayTask);

                successMeasure = Boolean.TRUE;

                return successMeasure;
            }

            @Override
            protected void done() {
                try {
                    Boolean wasSuccessful = get();
                    if (wasSuccessful != null && wasSuccessful) {
                        log.debug(message);
                    } else {
                        log.error(message);
                    }
                    if (!message.equals("")) {
                        MessageManager.INSTANCE.addMessage(new Message(message));
                    }
                } catch (Exception e) {
                    // Not really used but caught so that SwingWorker shuts down cleanly.
                    log.error(e.getClass() + " " + e.getMessage());
                }
            }
        };
        log.debug("Resetting transactions in background SwingWorker thread");
        worker.execute();
    }
}