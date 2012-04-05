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
package org.multibit.store;

import static org.junit.Assert.assertEquals;

import java.io.File;

import org.junit.Test;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.StoredBlock;

public class ReplayableBlockStoreTest {
    
    @Test
    public void testBasicStorage() throws Exception {
        File temporaryBlockStore = File.createTempFile("ReplayableBlockStore-testBasicStorage", null, null);
        System.out.println(temporaryBlockStore.getAbsolutePath());
        temporaryBlockStore.deleteOnExit();

        NetworkParameters networkParameters = NetworkParameters.unitTests();
        Address toAddress1 = new ECKey().toAddress(networkParameters);
        Address toAddress2 = new ECKey().toAddress(networkParameters);

        ReplayableBlockStore store = new ReplayableBlockStore(networkParameters, temporaryBlockStore, true);
        
        // Check the first block in a new store is the genesis block.
        StoredBlock genesis = store.getChainHead();
        assertEquals(networkParameters.genesisBlock, genesis.getHeader());

        // Build a new block.
        StoredBlock block1 = genesis.build(genesis.getHeader().createNextBlock(toAddress1).cloneAsHeader());
        store.put(block1);
        store.setChainHead(block1);

        // Check we can get it back out again if we rebuild the store object.
        store = new ReplayableBlockStore(networkParameters, temporaryBlockStore, false);
        StoredBlock block1reborn = store.get(block1.getHeader().getHash());
        assertEquals(block1, block1reborn);

        // Check the chain head was stored correctly also.
        assertEquals(block1, store.getChainHead());
        
        
        // Build another block.
        StoredBlock block2 = block1.build(block1.getHeader().createNextBlock(toAddress2).cloneAsHeader());
        store.put(block2);
        store.setChainHead(block2);

        // Check we can get it back out again if we rebuild the store object.
        store = new ReplayableBlockStore(networkParameters, temporaryBlockStore, false);
        StoredBlock block2reborn = store.get(block2.getHeader().getHash());
        assertEquals(block2, block2reborn);

        // Check the chain head was stored correctly also.
        assertEquals(block2, store.getChainHead());
    }
    
    @Test
    public void testChainheadToPastTruncates() throws Exception {
        // this functionality is used in blockchain replay
        
        File temporaryBlockStore = File.createTempFile("ReplayableBlockStore-testReplay", null, null);
        System.out.println(temporaryBlockStore.getAbsolutePath());
        temporaryBlockStore.deleteOnExit();

        NetworkParameters networkParameters = NetworkParameters.unitTests();
        Address toAddress1 = new ECKey().toAddress(networkParameters);
        Address toAddress2 = new ECKey().toAddress(networkParameters);

        ReplayableBlockStore store = new ReplayableBlockStore(networkParameters, temporaryBlockStore, true);
        
        // Check the first block in a new store is the genesis block.
        StoredBlock genesis = store.getChainHead();
        assertEquals(networkParameters.genesisBlock, genesis.getHeader());

        // Build a new block.
        StoredBlock block1 = genesis.build(genesis.getHeader().createNextBlock(toAddress1).cloneAsHeader());
        store.put(block1);
        store.setChainHead(block1);

        // remember the size of the blockstore after adding the first block
        long blockSizeAfterFirstBlockAdded = temporaryBlockStore.length();
        System.out.println("blockSizeAfterFirstBlockAdded = " + blockSizeAfterFirstBlockAdded);
        
        
        // Build another block.
        StoredBlock block2 = block1.build(block1.getHeader().createNextBlock(toAddress2).cloneAsHeader());
        store.put(block2);
        store.setChainHead(block2);
       
        long blockSizeAfterSecondBlockAdded = temporaryBlockStore.length();
        System.out.println("blockSizeAfterSecondBlockAdded = " + blockSizeAfterSecondBlockAdded);
               
        store.setChainHeadAndTruncate(block1);
        
        long blockSizeAfterSetChainHeadAndTruncate = temporaryBlockStore.length();
        System.out.println("blockSizeAfterSetChainHeadAndTruncate = " + blockSizeAfterSetChainHeadAndTruncate);
        
        assertEquals("setChainHeadAndTruncate did not roll back blockstore", blockSizeAfterFirstBlockAdded, blockSizeAfterSetChainHeadAndTruncate);
    }
}
