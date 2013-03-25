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

import com.google.bitcoin.core.*;
import com.google.bitcoin.discovery.DnsDiscovery;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.MemoryBlockStore;

import java.io.File;
import java.io.FileInputStream;
import java.math.BigInteger;
import java.net.InetAddress;
import java.util.Date;
import java.util.Map;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * ReplayManager is responsible for updating Wallets that are not updated to MultiBit's
 * main BlockStore. This happens when:
 * 1) The user imports some private keys
 * 2) They do a 'Reset blockchain and transactions'
 * 3) An out of date wallet is opened
 * 4) Encrypted wallets are opened when the user has used an older version of MultiBit that does not 
 *    understand them (they then get out of date).
 *    
 *  MemoryBlockStore is used where necessary with a separate PeerGroup.
 */
public enum ReplayManager {
    INSTANCE;
    
    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    public void initialise(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }
    
    /**
     * Synchronise a wallet with the blockchain.
     * Assumes MultiBitService has been initialised
     */
    public void syncWallet(final PerWalletModelData perWalletModelData) throws Exception {
        final Wallet wallet = perWalletModelData.getWallet();
        final NetworkParameters params = controller.getModel().getNetworkParameters();
        
        long earliestKeyCreationTime = wallet.getEarliestKeyCreationTime();
        System.out.println("Earliest key creation time = " + new Date(earliestKeyCreationTime));
        System.out.println(wallet.toString());
        
        Sha256Hash lastBlockSeenHash = wallet.getLastBlockSeenHash();
        int lastBlockSeenHeight = wallet.getLastBlockSeenHeight();
        
        // Replay from the earlier of the lastBlockSeenHeight (may be -1) and lastBlockSeenHash.
        BlockStore blockStore = new MemoryBlockStore(params);
        
        File checkpointsFile = new File(controller.getMultiBitService().getCheckpointsFilename());
        
        if (checkpointsFile.exists()) {
            FileInputStream stream = new FileInputStream(checkpointsFile);
            
            if (lastBlockSeenHeight > -1) {
                MultiBitCheckpointManager checkpointManager = new MultiBitCheckpointManager(params, stream);
                // If we have a last seen height, use that to checkpoint
                StoredBlock checkpoint = checkpointManager.getCheckpointBeforeHeight(lastBlockSeenHeight);
                
                stream.close();
                stream = new FileInputStream(checkpointsFile);
                MultiBitCheckpointManager.checkpoint(params, stream, blockStore, checkpoint.getHeader().getTimeSeconds());
            } else {
                // Checkpoint by the wallet birthday.
                MultiBitCheckpointManager.checkpoint(params, stream, blockStore, earliestKeyCreationTime);
            }
        }

        BlockChain chain = new BlockChain(params, wallet, blockStore);

        final PeerGroup peerGroup = new PeerGroup(params, chain);
        peerGroup.addPeerDiscovery(new DnsDiscovery(params));
        peerGroup.setMaxConnections(4);
        peerGroup.setFastCatchupTimeSecs(earliestKeyCreationTime);
        System.out.println("Using FastCatchup for blockchain sync with time of " + (new Date(earliestKeyCreationTime)).toString());

        peerGroup.start();

        wallet.addEventListener(new AbstractWalletEventListener() {
            @Override
            public synchronized void onCoinsReceived(Wallet w, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
                System.out.println("\nReceived tx " + tx.getHashAsString());
                System.out.println(tx.toString());
                perWalletModelData.setDirty(true);
            }
        });

        // Now download and process the block chain.
        peerGroup.downloadBlockChain();
        peerGroup.stop();
       
        System.out.println("\nDone!\n");
        System.out.println(wallet.toString());
    }
}