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
package org.multibit.network;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;

import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerGroup;


public class MultiBitPeerGroup extends PeerGroup {
    private final Controller controller;
    private final BitcoinController bitcoinController;
    MultiBitDownloadListener multiBitDownloadListener = null;

    public static final int MAXIMUM_NUMBER_OF_PEERS = 6;

        
    public MultiBitPeerGroup(BitcoinController bitcoinController, NetworkParameters params, BlockChain chain) {
        super(params, chain);
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        multiBitDownloadListener = new MultiBitDownloadListener(this.bitcoinController);

        setMaxConnections(MAXIMUM_NUMBER_OF_PEERS);
    }
    
    /**
     * Download the blockchain from peers.
     * 
     * <p>This method wait until the download is complete.  "Complete" is defined as downloading
     * from at least one peer all the blocks that are in that peer's inventory.
     */
    @Override
    public void downloadBlockChain() {
        startBlockChainDownload(multiBitDownloadListener);
    }

    public MultiBitDownloadListener getMultiBitDownloadListener() {
        return multiBitDownloadListener;
    }
}
