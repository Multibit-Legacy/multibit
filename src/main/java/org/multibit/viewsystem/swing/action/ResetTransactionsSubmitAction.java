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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.SwingWorker;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.store.BlockStoreException;

/**
 * This {@link Action} views the BitCoinJ preferences
 */
public class ResetTransactionsSubmitAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(ResetTransactionsSubmitAction.class);
    
    private static final long serialVersionUID = 1923492460523457765L;

    private static final int NUMBER_OF_MILLISECOND_IN_A_SECOND = 1000;

    private static final long BUTTON_DOWNCLICK_TIME = 400;
    
    private MultiBitController controller;

    /**
     * Creates a new {@link ResetTransactionsSubmitAction}.
     */
    public ResetTransactionsSubmitAction(MultiBitController controller) {
        super(controller.getLocaliser().getString("resetTransactionsSubmitAction.text"));
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("resetTransactionsSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("resetTransactionsSubmitAction.mnemonicKey"));
    }

    /**
     * reset the transactions and replay the blockchain
     */
    public void actionPerformed(ActionEvent event) {
        // check to see if another process has changed the active wallet
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            final ResetTransactionsSubmitAction thisAction = this;
            setEnabled(false);
            
            resetTransactionsInBackground();
            Timer timer = new Timer();
            timer.schedule(new TimerTask() {
                @Override
                public void run() {
                    thisAction.setEnabled(true);
                }}, BUTTON_DOWNCLICK_TIME);
            
        }
    }
    
    /**
     * reset the transaction in a background Swing worker thread
     */
    private void resetTransactionsInBackground() {
        final PerWalletModelData finalPerWalletModelData = controller.getModel().getActivePerWalletModelData();
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            
            private String message = null;
            
            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure = Boolean.FALSE;
                
                // work out the earliest transaction date and save it to the wallet
                PerWalletModelData activePerWalletModelData = controller.getModel().getActivePerWalletModelData();

                Date earliestTransactionDate = new Date();
                Set<Transaction> allTransactions = activePerWalletModelData.getWallet().getTransactions(true, true);
                if (allTransactions != null) {
                    for (Transaction transaction : allTransactions) {
                        if (transaction != null) {
                            Date updateTime = transaction.getUpdateTime();
                            if (updateTime != null && earliestTransactionDate.after(updateTime)) {
                                earliestTransactionDate = updateTime;
                            }
                            Date updateDate = transaction.getUpdatedAt();
                            if (updateDate != null && earliestTransactionDate.after(updateDate)) {
                                earliestTransactionDate = updateDate;
                            }
                        }
                    }
                }

                // also look at the earliest key creation time - this is returned in
                // seconds and is converted to milliseconds
                long earliestKeyCreationTime = activePerWalletModelData.getWallet().getEarliestKeyCreationTime()
                        * NUMBER_OF_MILLISECOND_IN_A_SECOND;
                if (earliestKeyCreationTime != 0 && earliestKeyCreationTime < earliestTransactionDate.getTime()) {
                    earliestTransactionDate = new Date(earliestKeyCreationTime);
                }

                 // remove the transactions from the wallet
                activePerWalletModelData.getWallet().clearTransactions(0);

                // save the wallet without the transactions
                controller.getFileHandler().savePerWalletModelData(finalPerWalletModelData, true);

                try {
                    controller.getMultiBitService().replayBlockChain(earliestTransactionDate);
                    successMeasure = Boolean.TRUE;
                    message = controller.getLocaliser().getString("resetTransactionsSubmitAction.startReplay");
                } catch (BlockStoreException e) {
                    message = controller.getLocaliser().getString("resetTransactionsSubmitAction.replayUnsuccessful", new Object[]{e.getMessage()});;
                }
                return successMeasure;
            }
            
            protected void done() {
                try {
                    Boolean wasSuccessful = get();
                    if (wasSuccessful) {
                        log.debug(message);
                        controller.updateStatusLabel(message);  
                    } else {
                        log.error(message);
                        controller.updateStatusLabel(message);
                    }
                } catch (Exception e) {
                    // not really used but caught so that SwingWorker shuts down cleanly
                    log.error(e.getClass() + " " + e.getMessage());
                } 
            }
        };
        log.debug("Resetting transactions in background SwingWorker thread");
        worker.execute();
    }
}