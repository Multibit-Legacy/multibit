package org.multibit.controller;

import java.util.List;

import org.multibit.model.StatusEnum;
import org.multibit.viewsystem.swing.view.dialogs.SendBitcoinConfirmDialog;
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

    private final Controller controller;
    private final MultiBitController multiBitController;
    
    public MultiBitPeerEventListener(MultiBitController multiBitController) {
        this.multiBitController = multiBitController;
        this.controller = this.multiBitController;
    }
    
    @Override
    public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft) {
        this.multiBitController.fireBlockDownloaded();
    }

    @Override
    public void onChainDownloadStarted(Peer peer, int blocksLeft) {
        this.multiBitController.fireBlockDownloaded();
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
            controller.getModel().setNumberOfConnectedPeers(peerCount);
        }   
        SendBitcoinConfirmDialog.updateDialog(null); 
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
            controller.getModel().setNumberOfConnectedPeers(peerCount);
        } 
        SendBitcoinConfirmDialog.updateDialog(null);    
    }

    @Override
    public Message onPreMessageReceived(Peer peer, Message message) {
        return message;
    }

    @Override
    public void onTransaction(Peer peer, Transaction transaction) { 
        // Loop through all the wallets, seeing if the transaction is relevant
        // and adding them as pending if so.
        // (As of 30 Jan 2013, incoming zero confirmation tx are not seen if this code is removed)
//        if (transaction != null) {
//            try {
//                java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
//
//                if (perWalletModelDataList != null) {
//                    for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
//                        Wallet loopWallet = perWalletModelData.getWallet();
//                        if (loopWallet != null) {
//                            if (loopWallet.isPendingTransactionRelevant(transaction)) {
//                                // The perWalletModelData is marked as dirty.
//                                if (perWalletModelData.getWalletInfo() != null) {
//                                    synchronized(perWalletModelData.getWalletInfo()) {
//                                        perWalletModelData.setDirty(true);
//                                    }
//                                } else {
//                                    perWalletModelData.setDirty(true);
//                                }
//                                if (loopWallet.getTransaction(transaction.getHash()) == null) {
//                                    log.debug("MultiBit adding a new pending transaction for the wallet '"
//                                            + perWalletModelData.getWalletDescription() + "'\n" + transaction.toString());
//                                    loopWallet.receivePending(transaction, null);
//                                }
//                            }
//                        }
//                    }
//                }
//            } catch (ScriptException e) {
//                log.error(e.getMessage(), e);
//            } catch (VerificationException e) {
//                log.error(e.getMessage(), e);
//            }
//        }
    }

    @Override
    public List<Message> getData(Peer peer, GetDataMessage m) {
        return null;
    }
}
