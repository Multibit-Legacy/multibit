package org.multibit.network;

import org.multibit.controller.MultiBitController;

import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.DownloadListener;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.store.BlockStore;

public class MultiBitPeerGroup extends PeerGroup {
    MultiBitController controller;
    
    public MultiBitPeerGroup(MultiBitController controller, BlockStore blockStore, NetworkParameters params, BlockChain chain, Wallet wallet) {
        super(blockStore, params, chain, wallet);
        this.controller = controller;
    }
    
    /**
     * Download the blockchain from peers.
     * 
     * <p>This method wait until the download is complete.  "Complete" is defined as downloading
     * from at least one peer all the blocks that are in that peer's inventory.
     */
    @Override
    public void downloadBlockChain() {
        DownloadListener listener = new MultiBitDownloadListener(controller);
        startBlockChainDownload(listener);
        try {
            listener.await();
        } catch (InterruptedException e) {
            throw new IllegalStateException(e);
        }
    }

}
