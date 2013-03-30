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

package org.multibit.network;

import java.io.IOException;
import java.text.DateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.store.BlockStoreException;

/**
 * ReplayManager is responsible for updating Wallets that are not updated to
 * MultiBit's main BlockStore. This happens when: 1) The user imports some
 * private keys 2) They do a 'Reset blockchain and transactions' 3) An out of
 * date wallet is opened 4) Encrypted wallets are opened when the user has used
 * an older version of MultiBit that does not understand them (they then get out
 * of date).
 */
public enum ReplayManager {
    INSTANCE;

    private static final Logger log = LoggerFactory.getLogger(ReplayManager.class);

    private MultiBitController controller;

    public void initialise(MultiBitController controller) {
        this.controller = controller;
    }

    private Queue<ReplayTask> replayTaskQueue = new LinkedList<ReplayTask>();

    /**
     * Synchronise a wallet with the blockchain. Assumes MultiBitService has
     * been initialised Always uses SPVBlockStore
     */
    public void syncWallet(final ReplayTask replayTask) throws IOException,
            BlockStoreException {
        log.info("Starting replay task : " + replayTask.toString());

        // Mark the wallets as busy.
        List<PerWalletModelData> perWalletModelDataList = replayTask.getPerWalletModelDataToReplay();
        if (perWalletModelDataList != null) {
            for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                perWalletModelData.setBusy(true);
                perWalletModelData.setBusyTask(controller.getLocaliser().getString("multiBitDownloadListener.downloadingText"));
                perWalletModelData.setBusyTaskVerb(controller.getLocaliser().getString(
                        "multiBitDownloadListener.downloadingTextShort"));
            }
            controller.fireWalletBusyChange(true);
        }
        Date dateToReplayFrom = replayTask.getStartDate();

        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString(
                "resetTransactionsSubmitAction.startReplay")));

        log.debug("Starting replay of blockchain from date = '" + dateToReplayFrom);

        // Reset UI to zero peers.
        controller.getPeerEventListener().onPeerDisconnected(null, 0);

        // Restart peerGroup and download rest of blockchain.
        Message message;
        if (dateToReplayFrom != null) {
            message = new Message(controller.getLocaliser().getString(
                    "resetTransactionSubmitAction.replayingBlockchain",
                    new Object[] { DateFormat.getDateInstance(DateFormat.MEDIUM, controller.getLocaliser().getLocale()).format(
                            dateToReplayFrom) }), false);
        } else {
            message = new Message(controller.getLocaliser().getString(
                    "resetTransactionSubmitAction.replayingBlockchain",
                    new Object[] { DateFormat.getDateInstance(DateFormat.MEDIUM, controller.getLocaliser().getLocale()).format(
                            MultiBitService.genesisBlockCreationDate) }), false);
        }
        MessageManager.INSTANCE.addMessage(message);

        log.debug("About to restart PeerGroup.");       
        message = new Message(controller.getLocaliser().getString("multiBitService.stoppingBitcoinNetworkConnection"),
                false, 0);
        MessageManager.INSTANCE.addMessage(message);

        controller.getMultiBitService().getPeerGroup().stopAndWait();
        log.debug("PeerGroup is now stopped.");
 
        // Reset UI to zero peers.
        controller.getPeerEventListener().onPeerDisconnected(null, 0);

        // TODO Save wallets.
        
        // Close the blockstore and recreate a new one.
        controller.getMultiBitService().createNewBlockStoreForReplay(dateToReplayFrom);

        // Create a new PeerGroup.
        controller.getMultiBitService().createNewPeerGroup();
        log.debug("Recreated PeerGroup.");
        
        PeerGroup peerGroup = controller.getMultiBitService().getPeerGroup();
        if (peerGroup instanceof MultiBitPeerGroup && replayTask.getSingleWalletPanelDownloadListener() != null) {
            ((MultiBitPeerGroup)peerGroup).getMultiBitDownloadListener().addSingleWalletPanelDownloadListener(replayTask.getSingleWalletPanelDownloadListener());
        }

        // Start up the PeerGroup.
        peerGroup.start();
        log.debug("Restarted PeerGroup.");
        
        log.debug("About to start  blockchain download.");
        controller.getMultiBitService().downloadBlockChain();
        log.debug("Blockchain download started.");
    }

    /**
     * Add a ReplayTask to the ReplayManager's list of tasks to do.
     * 
     * @param replayTask
     */
    public boolean offerReplayTask(ReplayTask replayTask) {
        replayTaskQueue.offer(replayTask);

        // TODO - put in worker thread.
        try {
            syncWallet(replayTask);
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (BlockStoreException bse) {
            bse.printStackTrace();
        }
        return true;
    }
    
    public void currentTaskHasCompleted() {
        // The downloadlistener listening to the replay reports that the current task has completed.
        ReplayTask currentTask = replayTaskQueue.peek();
        if (currentTask != null) {
            // This task is complete.
            List<PerWalletModelData> perWalletModelDataList = currentTask.getPerWalletModelDataToReplay();
            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    perWalletModelData.setBusyTaskVerb(null);
                    perWalletModelData.setBusyTask(null);
                    perWalletModelData.setBusy(false);
                }
            }
            // TODO - does not look quite right.
            controller.fireWalletBusyChange(false);
            
            // Remove that task from the queue.
            log.debug("ReplayTask " + currentTask.toString() + " has completed.");
            replayTaskQueue.poll();
            
            // Start the next task.
            // TODO start the next replayTask in the queue.
        }
    }
}