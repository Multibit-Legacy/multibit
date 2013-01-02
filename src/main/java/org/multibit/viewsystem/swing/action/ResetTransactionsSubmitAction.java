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
import java.util.Date;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.SwingWorker;

import org.multibit.controller.MultiBitController;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.utils.DateUtils;
import org.multibit.viewsystem.dataproviders.ResetTransactionsDataProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.store.BlockStoreException;

/**
 * This {@link Action} resets the blockchain and transactions.
 */
public class ResetTransactionsSubmitAction extends MultiBitSubmitAction {

    private static final Logger log = LoggerFactory.getLogger(ResetTransactionsSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private static final int NUMBER_OF_MILLISECOND_IN_A_SECOND = 1000;

    private static final long BUTTON_DOWNCLICK_TIME = 400;

    private ResetTransactionsDataProvider resetTransactionsDataProvider;

    /**
     * Creates a new {@link ResetTransactionsSubmitAction}.
     */
    public ResetTransactionsSubmitAction(MultiBitController controller, Icon icon,
            ResetTransactionsDataProvider resetTransactionsDataProvider) {
        super(controller, "resetTransactionsSubmitAction.text", "resetTransactionsSubmitAction.tooltip",
                "resetTransactionsSubmitAction.mnemonicKey", icon);
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

        PerWalletModelData activePerWalletModelData = controller.getModel().getActivePerWalletModelData();
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
            controller.getFileHandler().savePerWalletModelData(activePerWalletModelData, true);
            log.debug("RT Ping 10");

            controller.getModel().createWalletData(controller.getModel().getActiveWalletFilename());
            log.debug("RT Ping 11");

            controller.fireRecreateAllViews(false);
            log.debug("RT Ping 12");

        } catch (WalletSaveException wse) {
            log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
            MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
        }
        log.debug("RT Ping 13");

        resetTransactionsInBackground(resetFromFirstTransaction, actualResetDate);
        log.debug("RT Ping 14");

        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                thisAction.setEnabled(true);
            }
        }, BUTTON_DOWNCLICK_TIME);
    }

    /**
     * Reset the transaction in a background Swing worker thread.
     */
    private void resetTransactionsInBackground(final boolean resetFromFirstTransaction, final Date resetDate) {
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {

            private String message = "";

            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure = Boolean.FALSE;

                try {
                    controller.getMultiBitService().replayBlockChain(resetDate);

                    successMeasure = Boolean.TRUE;
                } catch (BlockStoreException e) {
                    message = controller.getLocaliser().getString("resetTransactionsSubmitAction.replayUnsuccessful",
                            new Object[] { e.getMessage() });
                }

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