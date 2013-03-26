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
import java.io.FileInputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Queue;
import java.util.TimeZone;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.GetDataMessage;
import com.google.bitcoin.core.Message;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
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

    private MultiBitController multiBitController;

    private SimpleDateFormat formatter;

    public void initialise(MultiBitController multiBitController) {
        this.multiBitController = multiBitController;
        
        // Date format is UTC with century, T time separator and Z for UTC timezone.
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    private Queue<ReplayTask> replayTaskQueue = new LinkedList<ReplayTask>();

    /**
     * Synchronise a wallet with the blockchain. Assumes MultiBitService has been initialised
     */
    public void syncWallet(final PerWalletModelData perWalletModelData, ReplayTask replayTask) throws IOException,
            BlockStoreException {
        log.info("Starting replay task : " + replayTask.toString());

        final Wallet wallet = perWalletModelData.getWallet();
        final NetworkParameters params = multiBitController.getModel().getNetworkParameters();
        final String checkpointsFilename;
        if ("".equals(multiBitController.getApplicationDataDirectoryLocator().getApplicationDataDirectory())) {
            checkpointsFilename = MultiBitService.getFilePrefix() + MultiBitService.CHECKPOINTS_SUFFIX;
        } else {
            checkpointsFilename = multiBitController.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
            + MultiBitService.getFilePrefix() + MultiBitService.CHECKPOINTS_SUFFIX;  
        }

//        long earliestKeyCreationTime = wallet.getEarliestKeyCreationTime();
//        log.debug("Earliest key creation time = " + new Date(earliestKeyCreationTime * 1000));

//        Sha256Hash lastBlockSeenHash = wallet.getLastBlockSeenHash();
//        int lastBlockSeenHeight = wallet.getLastBlockSeenHeight();

        Date checkpointDate = null;
        if (replayTask.getStartDate() != null) {
            checkpointDate = replayTask.getStartDate();
        } else {
            throw new IllegalArgumentException("No replay start time specified");
        }

        // Create a separate MultiBit runtime just for the replay.
        File multiBitDirectory = createMultiBitRuntime();

        // Set the application data directory to be the one we just created.
        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);

        // Create the replayController.
        final MultiBitController replayController = new MultiBitController(applicationDataDirectoryLocator);

        // Create the model - gets hooked up to controller automatically.
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(replayController);

        log.debug("Creating Bitcoin service");
        // Create the MultiBitService that connects to the bitcoin network.
        MultiBitService multiBitService = new MultiBitService(replayController);
        replayController.setMultiBitService(multiBitService);

        // Add the simple view system (no Swing).
        SimpleViewSystem simpleViewSystem = new SimpleViewSystem();
        replayController.registerViewSystem(simpleViewSystem);
        
        File checkpointsFile = new File(checkpointsFilename);
        if (checkpointsFile.exists()) {
            FileInputStream stream = new FileInputStream(checkpointsFile);

            // Checkpoint by an absolute time.
            log.info("Checkpointing by date : " + checkpointDate.toString());
            MultiBitCheckpointManager.checkpoint(params, stream, replayController.getMultiBitService().getBlockStore(), checkpointDate.getTime() / 1000);
        }
        
        replayController.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Get the multibitService to load it up and hook it up to the blockchain.
        replayController.getMultiBitService().addWalletFromFilename(perWalletModelData.getWalletFilename());
        replayController.getModel().setActiveWalletByFilename(perWalletModelData.getWalletFilename());

        // Wait for a peer connection.
        log.debug("Waiting for peer connection. . . ");
        while (!simpleViewSystem.isOnline()) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        log.debug("Now online.");
        
        // Now download and process the block chain.
        ReplayPeerEventListener peerEventListener = new ReplayPeerEventListener();
        replayController.getMultiBitService().getPeerGroup().startBlockChainDownload(peerEventListener);

        log.debug("Waiting for blockchain replay to reach a height of " + replayTask.getStopBlockHeight() + " or date "
                + replayTask.getStopDate().toString());
        boolean continueDownload = true;
        while (continueDownload) {
            try {
                Thread.sleep(1000);
                log.debug("Blocks downloaded =  " + simpleViewSystem.getNumberOfBlocksDownloaded());
                log.debug("Last block = " + peerEventListener.lastBlock + ", blocksLeft = " + peerEventListener.blocksLeft);
                if (peerEventListener.lastBlock != null) {
                    if (replayTask.getStopDate() != null && peerEventListener.lastBlock.getTime().after(replayTask.getStopDate())) {
                        continueDownload = false;
                    }
                }
                if (peerEventListener.blocksLeft == 0) {
                    continueDownload = false;
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        replayController.getMultiBitService().getPeerGroup().removeWallet(wallet);
        replayController.getMultiBitService().getPeerGroup().stop();

        log.info("Completed replay task : " + replayTask.toString());
        log.debug(wallet.toString());
        
        // TODO - just save the wallet.
    }

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
    
    /**
     * Create a working, portable runtime of MultiBit in a temporary directory.
     * 
     * @return the temporary directory the multibit runtime has been created in.
     */
    private File createMultiBitRuntime() throws IOException {
        File multiBitDirectory = FileHandler.createTempDirectory("multibit");
        multiBitDirectory.deleteOnExit();
        String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

        System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

        // create an empty multibit.properties
        File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multibit.properties");
        multibitProperties.createNewFile();
        multibitProperties.deleteOnExit();

        return multiBitDirectory;
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
        public Message onPreMessageReceived(Peer peer, Message m) {
            return null;
        }

        @Override
        public void onTransaction(Peer peer, Transaction t) {
        }

        @Override
        public List<Message> getData(Peer peer, GetDataMessage m) {
            return null;
        }
    }
}