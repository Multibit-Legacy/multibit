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
import java.util.UUID;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.store.BlockStore;
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
    
    /**
     * The actual chain height prior to any replay
     * (as opposed to the current height whilst replaying).
     */
    private int actualLastChainHeight;
    
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
        
        // Remember the chain height.
        if (controller.getMultiBitService().getChain() != null) {
            actualLastChainHeight = controller.getMultiBitService().getChain().getBestChainHeight();
        }

        // Mark the wallets as busy and set the replay task uuid into the model
        List<PerWalletModelData> perWalletModelDataList = replayTask.getPerWalletModelDataToReplay();
        if (perWalletModelDataList != null) {
            for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                perWalletModelData.setBusy(true);
                perWalletModelData.setBusyTask(controller.getLocaliser().getString("multiBitDownloadListener.downloadingText"));
                perWalletModelData.setBusyTaskVerb(controller.getLocaliser().getString(
                        "multiBitDownloadListener.downloadingTextShort"));
                perWalletModelData.setReplayTaskUUID(replayTask.getUuid());
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
        int newChainHeightAfterTruncate = controller.getMultiBitService().createNewBlockStoreForReplay(dateToReplayFrom);
        if (replayTask.getStartHeight() == ReplayTask.UNKNOWN_START_HEIGHT) {
            replayTask.setStartHeight(newChainHeightAfterTruncate);
        }
        
        // Create a new PeerGroup.
        controller.getMultiBitService().createNewPeerGroup();
        log.debug("Recreated PeerGroup.");
        
        // Hook up the download listeners.
        addDownloadListeners(perWalletModelDataList);

        // Start up the PeerGroup.
        PeerGroup peerGroup = controller.getMultiBitService().getPeerGroup();
        peerGroup.start();
        log.debug("Restarted PeerGroup = " + peerGroup.toString());
              
        log.debug("About to start  blockchain download.");
        controller.getMultiBitService().downloadBlockChain();
        log.debug("Blockchain download started.");
    }
    
    public void addDownloadListeners(List<PerWalletModelData> perWalletModelDataList) {
        PeerGroup peerGroup = controller.getMultiBitService().getPeerGroup();
        if (peerGroup instanceof MultiBitPeerGroup) {
            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    if (perWalletModelData.getSingleWalletDownloadListener() != null) {
                        ((MultiBitPeerGroup) peerGroup).getMultiBitDownloadListener().addSingleWalletPanelDownloadListener(
                                perWalletModelData.getSingleWalletDownloadListener());
                    }
                }
            }
        }
    }

    public void removeDownloadListeners(List<PerWalletModelData> perWalletModelDataList) {
        PeerGroup peerGroup = controller.getMultiBitService().getPeerGroup();
        if (peerGroup instanceof MultiBitPeerGroup) {
            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    if (perWalletModelData.getSingleWalletDownloadListener() != null) {
                        ((MultiBitPeerGroup) peerGroup).getMultiBitDownloadListener().removeDownloadListener(
                                perWalletModelData.getSingleWalletDownloadListener());
                    }
                }
            }
        }
    }

    /**
     * Add a ReplayTask to the ReplayManager's list of tasks to do.
     * 
     * @param replayTask
     */
    public boolean offerReplayTask(ReplayTask replayTask) {
        if (replayTask == null) {
            return false;
        }
        
        synchronized(replayTaskQueue) {
            replayTaskQueue.offer(replayTask);
            String waitingText = controller.getLocaliser().getString("singleWalletPanel.waiting.text");
            String waitingVerb = controller.getLocaliser().getString("singleWalletPanel.waiting.verb");
            
            for (PerWalletModelData perWalletModelData : replayTask.getPerWalletModelDataToReplay()) {
                if (perWalletModelData != null) {
                    perWalletModelData.setBusy(true);
                    perWalletModelData.setBusyTaskVerb(waitingVerb);
                    perWalletModelData.setBusyTask(waitingText);
                }
            }
        }
        return true;
    }
     
    /**
     * Called by the downloadlistener when the synchronise completes.
     * @param replayTaskUUID TODO
     */
    public void taskHasCompleted(UUID replayTaskUUID) {
        log.debug("ReplayTask with UUID " + replayTaskUUID + " has completed.");
        // Check the UUID matches the current task.
        ReplayTask currentTask = replayTaskQueue.peek();
        if (currentTask == null) {
            return;
        } else {
            // Not relevant - ignore.
            if (!currentTask.getUuid().equals(replayTaskUUID)) {
                return;
            }
        }
        
        // Tell the ReplayTimerTask that we are cleaning up.
        replayManagerTimerTask.currentTaskIsTidyingUp(true);
        
        try {
            if (currentTask != null) {
                // This task is complete. Inform the UI.
                List<PerWalletModelData> perWalletModelDataList = currentTask.getPerWalletModelDataToReplay();
                if (perWalletModelDataList != null) {
                    for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                        perWalletModelData.setBusyTaskVerb(null);
                        perWalletModelData.setBusyTask(null);
                        perWalletModelData.setBusy(false);
                        perWalletModelData.setReplayTaskUUID(null);
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
    
    public ReplayTask getCurrentReplayTask() {
        synchronized (replayTaskQueue) {
            if (replayTaskQueue.isEmpty()) {
                return null;
            } else {
                return replayTaskQueue.peek();
            }
        }
    }
    
    /**
     * See if there is a waiting replay task for a perWalletModelData
     * @param perModelWalletData
     * @return the waiting ReplayTask or null if there is not one.
     */
    @SuppressWarnings("unchecked")
    public ReplayTask getWaitingReplayTask(PerWalletModelData perWalletModelData) {
        synchronized (replayTaskQueue) {
            if (replayTaskQueue.isEmpty()) {
                return null;
            } else {
                for (ReplayTask replayTask : (List<ReplayTask>)replayTaskQueue) {
                    List<PerWalletModelData> list = replayTask.getPerWalletModelDataToReplay();
                    if (list != null) {
                        for (PerWalletModelData item : list) {
                            if (perWalletModelData.getWalletFilename().equals(item.getWalletFilename())) {
                                return replayTask;
                            }
                        }
                    }
                    
                }
                return null;
            }
        }
    }
    
    /**
     * The actual height of the block chain just prior to a replay being started.
     * @return
     */
    public int getActualLastChainHeight() {
        return actualLastChainHeight;   
    }
}