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
import java.util.Timer;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.multibit.viewsystem.swing.UpdateTransactionsTimerTask;
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
    private ReplayManagerTimerTask replayManagerTimerTask;
    private Timer replayManagerTimer;
    
    private static final int REPLAY_MANAGER_DELAY_TIME = 0; // ms
    private static final int REPLAY_MANAGER_REPEAT_TIME = 333; // ms
    
    private MultiBitController controller;

    final private Queue<ReplayTask> replayTaskQueue = new LinkedList<ReplayTask>();

    public void initialise(MultiBitController controller) {
        this.controller = controller;
        replayManagerTimerTask = new ReplayManagerTimerTask(controller, replayTaskQueue);
        replayManagerTimer = new Timer();
        replayManagerTimer.scheduleAtFixedRate(replayManagerTimerTask, REPLAY_MANAGER_DELAY_TIME, REPLAY_MANAGER_REPEAT_TIME);
    }

    /**
     * Synchronise one or more wallets with the blockchain.
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
        log.debug("Restarted PeerGroup = " + peerGroup.toString());
              
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
        synchronized(replayTaskQueue) {
            replayTaskQueue.offer(replayTask);
        }
        return true;
    }
    
    /**
     * Called by the downloadlistener when the synchronise completes.
     */
    public void currentTaskHasCompleted() {
        // Tell the ReplayTimerTask that we are cleaning up.
        replayManagerTimerTask.currentTaskIsTidyingUp(true);
        
        try {
            ReplayTask currentTask = replayTaskQueue.peek();
            if (currentTask != null) {
                // This task is complete. Inform the UI.
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
            }
        } finally {
            // No longer tidying up.
            replayManagerTimerTask.currentTaskIsTidyingUp(false);

            // Everything is completed - clear to start the next task.
            replayManagerTimerTask.currentTaskHasCompleted();
        }
    }
}