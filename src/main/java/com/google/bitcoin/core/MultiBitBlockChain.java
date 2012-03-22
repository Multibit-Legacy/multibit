/**
 * Copyright 2012 multibit.org
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
package com.google.bitcoin.core;

import java.util.List;

import org.multibit.store.ReplayableBlockStore;

import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;

/**
 * Extension of bitcoinj BlockChain for use with block chain replay
 */
public class MultiBitBlockChain extends BlockChain {

    public MultiBitBlockChain(NetworkParameters params, Wallet wallet, BlockStore blockStore) throws BlockStoreException {
        super(params, wallet, blockStore);
    }

    public MultiBitBlockChain(NetworkParameters params, BlockStore blockStore) throws BlockStoreException {
        super(params, blockStore);
    }

    public MultiBitBlockChain(NetworkParameters params, List<Wallet> wallets, BlockStore blockStore) throws BlockStoreException {
        super(params, wallets, blockStore);
    }
    
    /**
     * set the chainhead, clear any cached blocks and truncate the blockchain 
     * (used for blockchain replay)
     * @param chainHead
     * @throws BlockStoreException
     */
    public void setChainHeadClearCachesAndTruncateBlockStore(StoredBlock chainHead) throws BlockStoreException {
        if (blockStore instanceof ReplayableBlockStore) {
            ((ReplayableBlockStore) blockStore).setChainHeadAndTruncate(chainHead);
        } else {
            blockStore.setChainHead(chainHead);

        }
        
        // this synchronized probably needs to enclude the truncate
        synchronized (chainHeadLock) {
            this.chainHead = chainHead;
            //unconnectedBlocks.clear();
        }
    }

}
