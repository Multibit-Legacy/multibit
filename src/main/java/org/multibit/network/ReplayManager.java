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

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Queue;
import java.util.TimeZone;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.GetDataMessage;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.store.BlockStoreException;

/**
 * ReplayManager is responsible for updating Wallets that are not updated to
 * MultiBit's main BlockStore. This happens when: 1) The user imports some
 * private keys 2) They do a 'Reset blockchain and transactions' 3) An out of
 * date wallet is opened 4) Encrypted wallets are opened when the user has used
 * an older version of MultiBit that does not understand them (they then get out
 * of date).
 * 
 * MemoryBlockStore is used where necessary with a separate PeerGroup.
 */
public enum ReplayManager {
    INSTANCE;

    private static final Logger log = LoggerFactory.getLogger(ReplayManager.class);

    private MultiBitController controller;

    private SimpleDateFormat formatter;

    public void initialise(MultiBitController controller) {
        this.controller = controller;

        // Date format is UTC with century, T time separator and Z for UTC
        // timezone.
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    private Queue<ReplayTask> replayTaskQueue = new LinkedList<ReplayTask>();

    /**
     * Synchronise a wallet with the blockchain. Assumes MultiBitService has
     * been initialised Always uses SPVBlockStore
     */
    public void syncWallet(final PerWalletModelData perWalletModelData, ReplayTask replayTask) throws IOException,
            BlockStoreException {
        log.info("Starting replay task : " + replayTask.toString());

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

        controller.getMultiBitService().createNewBlockStoreForReplay(dateToReplayFrom);

        log.debug("About to restart PeerGroup.");
        controller.getMultiBitService().restartPeerGroup();
        log.debug("Restarted PeerGroup.");
        
        PeerGroup peerGroup = controller.getMultiBitService().getPeerGroup();
        if (peerGroup instanceof MultiBitPeerGroup && replayTask.getSingleWalletPanelDownloadListener() != null) {
            ((MultiBitPeerGroup)peerGroup).getMultiBitDownloadListener().addSingleWalletPanelDownloadListener(replayTask.getSingleWalletPanelDownloadListener());
        }

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

        // TODO This would be done in a different thread and hook up listeners
        // for all the wallets to sync.
        try {
            syncWallet(replayTask.getPerWalletModelDataToReplay().get(0), replayTask);
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (BlockStoreException bse) {
            bse.printStackTrace();
        }
        return true;
    }

    class ReplayPeerEventListener implements PeerEventListener {
        public Block lastBlock = null;
        public int blocksLeft = -1;

        @Override
        public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft) {
            this.lastBlock = block;
            this.blocksLeft = blocksLeft;
            log.debug("onBlocksDownloaded blocksLeft = " + blocksLeft);
        }

        @Override
        public void onChainDownloadStarted(Peer peer, int blocksLeft) {
            log.debug("onChainDownloadStarted blocksLeft = " + blocksLeft);
        }

        @Override
        public void onPeerConnected(Peer peer, int peerCount) {
            log.debug("peerConnected, peerCount = " + peerCount);
        }

        @Override
        public void onPeerDisconnected(Peer peer, int peerCount) {
            log.debug("peerDisconnected, peerCount = " + peerCount);
        }

        @Override
        public com.google.bitcoin.core.Message onPreMessageReceived(Peer peer, com.google.bitcoin.core.Message m) {
            return null;
        }

        @Override
        public void onTransaction(Peer peer, Transaction t) {
        }

        @Override
        public List<com.google.bitcoin.core.Message> getData(Peer peer, GetDataMessage m) {
            return null;
        }
    }
}