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

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.SwingWorker;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.utils.DateUtils;
import org.multibit.viewsystem.dataproviders.ResetTransactionsDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanel;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanelDownloadListener;
import org.multibit.viewsystem.swing.view.walletlist.WalletListPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Transaction;

/**
 * This {@link Action} resets the blockchain and transactions.
 */
public class ResetTransactionsSubmitAction extends MultiBitSubmitAction {

    private static final Logger log = LoggerFactory.getLogger(ResetTransactionsSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private static final int NUMBER_OF_MILLISECOND_IN_A_SECOND = 1000;

    private ResetTransactionsDataProvider resetTransactionsDataProvider;
    
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link ResetTransactionsSubmitAction}.
     */
    public ResetTransactionsSubmitAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, Icon icon,
            ResetTransactionsDataProvider resetTransactionsDataProvider) {
        super(bitcoinController, "resetTransactionsSubmitAction.text", "resetTransactionsSubmitAction.tooltip",
                "resetTransactionsSubmitAction.mnemonicKey", icon);
        this.mainFrame = mainFrame;
        this.resetTransactionsDataProvider = resetTransactionsDataProvider;
    }


    /**
     * Reset the transactions and replay the blockchain.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        log.debug("RT Ping 1");
        if (abort()) {
            return;
        }
        log.debug("RT Ping 2");

        final ResetTransactionsSubmitAction thisAction = this;
        setEnabled(false);

        boolean resetFromFirstTransaction = resetTransactionsDataProvider.isResetFromFirstTransaction();
        Date resetDate = resetTransactionsDataProvider.getResetDate();

        PerWalletModelData activePerWalletModelData = super.bitcoinController.getModel().getActivePerWalletModelData();
        log.debug("RT Ping 3");

        Date actualResetDate = null;

        if (resetFromFirstTransaction) {
            // Work out the earliest transaction date and save it to the wallet.
            log.debug("RT Ping 4");

            Date earliestTransactionDate = new Date(DateUtils.nowUtc().getMillis());
            Set<Transaction> allTransactions = activePerWalletModelData.getWallet().getTransactions(true, true);
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
            actualResetDate = earliestTransactionDate;
            log.debug("RT Ping 5");

            // Look at the earliest key creation time - this is
            // returned in seconds and is converted to milliseconds.
            long earliestKeyCreationTime = activePerWalletModelData.getWallet().getEarliestKeyCreationTime()
                    * NUMBER_OF_MILLISECOND_IN_A_SECOND;
            if (earliestKeyCreationTime != 0 && earliestKeyCreationTime < earliestTransactionDate.getTime()) {
                earliestTransactionDate = new Date(earliestKeyCreationTime);
                actualResetDate = earliestTransactionDate;
            }
            log.debug("RT Ping 6");
        } else {
            log.debug("RT Ping 7");
            actualResetDate = resetDate;
        }
        log.debug("RT Ping 8");

        // Remove the transactions from the wallet.
        activePerWalletModelData.getWallet().clearTransactions(actualResetDate);
        log.debug("RT Ping 9");

        // Save the wallet without the transactions.
        try {
            super.bitcoinController.getFileHandler().savePerWalletModelData(activePerWalletModelData, true);
            log.debug("RT Ping 10");

            super.bitcoinController.getModel().createWalletData(super.bitcoinController, super.bitcoinController.getModel().getActiveWalletFilename());
            log.debug("RT Ping 11");

            controller.fireRecreateAllViews(false);
            log.debug("RT Ping 12");

        } catch (WalletSaveException wse) {
            log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
            MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
        }
        log.debug("RT Ping 13");
        
        // Double check wallet is not busy then declare that the active wallet
        // is busy with the task
        PerWalletModelData perWalletModelData = this.bitcoinController.getModel().getActivePerWalletModelData();

        if (!perWalletModelData.isBusy()) {
            perWalletModelData.setBusy(true);
            perWalletModelData.setBusyTaskKey("resetTransactionsSubmitAction.text");
            perWalletModelData.setBusyTaskVerbKey("resetTransactionsSubmitAction.verb");

            super.bitcoinController.fireWalletBusyChange(true);

            resetTransactionsInBackground(resetFromFirstTransaction, actualResetDate, activePerWalletModelData.getWalletFilename());
            log.debug("RT Ping 14");
        }

    }

    /**
     * Reset the transaction in a background Swing worker thread.
     */
    private void resetTransactionsInBackground(final boolean resetFromFirstTransaction, final Date resetDate, final String walletFilename) {
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {

            private String message = "";

            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure = Boolean.FALSE;

                log.debug("Starting replay from date = " + resetDate);
                List<PerWalletModelData> perWalletModelDataList = new ArrayList<PerWalletModelData>();
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
                    if (wasSuccessful != null && wasSuccessful.booleanValue()) {
                        log.debug(message);
                    } else {
                        log.error(message);
                    }
                    if (message != "") {
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