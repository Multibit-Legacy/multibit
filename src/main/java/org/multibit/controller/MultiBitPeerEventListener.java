package org.multibit.controller;

import java.util.List;

import org.multibit.model.StatusEnum;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.GetDataMessage;
import com.google.bitcoin.core.Message;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.Transaction;

public class MultiBitPeerEventListener implements PeerEventListener {

    private Logger log = LoggerFactory.getLogger(MultiBitPeerEventListener.class);

    private MultiBitController controller;
    
    public MultiBitPeerEventListener(MultiBitController controller) {
        this.controller = controller;
    }
    
    @Override
    public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft) {
        controller.fireBlockDownloaded();
    }

    @Override
    public void onChainDownloadStarted(Peer peer, int blocksLeft) {
        controller.fireBlockDownloaded();
    }

    @Override
    public void onPeerConnected(Peer peer, int peerCount) {
         if (peerCount >= 1) {
            controller.setOnlineStatus(StatusEnum.ONLINE);
        }
        if (controller.getModel() != null) {
            controller.getModel().setNumberOfConnectedPeers(peerCount);
        }   
        SendBitcoinConfirmPanel.updatePanel(null); 
    }

    @Override
    public void onPeerDisconnected(Peer peer, int peerCount) {
        //log.debug("Peer '" + peer.toString() + "' disconnected . PeerCount = " + peerCount);
        if (peerCount == 0) {
           controller.setOnlineStatus(StatusEnum.CONNECTING);
        }
        if (controller.getModel() != null) {
            controller.getModel().setNumberOfConnectedPeers(peerCount);
        } 
        SendBitcoinConfirmPanel.updatePanel(null);    
    }

    @Override
    public Message onPreMessageReceived(Peer peer, Message message) {
        return message;
    }

    @Override
    public void onTransaction(Peer peer, Transaction transaction) { 
    }

    @Override
    public List<Message> getData(Peer peer, GetDataMessage m) {
        return null;
    }
}
