package org.multibit.controller.bitcoin;

import java.util.List;

import org.multibit.controller.Controller;
import org.multibit.model.core.StatusEnum;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.GetDataMessage;
import com.google.bitcoin.core.Message;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.Transaction;

public class BitcoinPeerEventListener implements PeerEventListener {

    private Logger log = LoggerFactory.getLogger(BitcoinPeerEventListener.class);

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    public BitcoinPeerEventListener(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
    }
    
    @Override
    public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft) {
        this.bitcoinController.fireBlockDownloaded();
    }

    @Override
    public void onChainDownloadStarted(Peer peer, int blocksLeft) {
        this.bitcoinController.fireBlockDownloaded();
    }

    @Override
    public void onPeerConnected(Peer peer, int peerCount) {
        if (peer != null) {
            log.debug("Connected to peer:" + peer.getPeerVersionMessage());
        }
        if (peerCount >= 1) {
            controller.setOnlineStatus(StatusEnum.ONLINE);
        }
        if (controller.getModel() != null) {
            this.bitcoinController.getModel().setNumberOfConnectedPeers(peerCount);
        }   
        SendBitcoinConfirmPanel.updatePanel(); 
    }

    @Override
    public void onPeerDisconnected(Peer peer, int peerCount) {
        if (peer != null) {
            log.debug("Disconnected from peer, address : " + peer.getAddress());
        }
        if (peerCount == 0) {
           controller.setOnlineStatus(StatusEnum.CONNECTING);
        }
        if (controller.getModel() != null) {
            this.bitcoinController.getModel().setNumberOfConnectedPeers(peerCount);
        } 
        SendBitcoinConfirmPanel.updatePanel();    
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
