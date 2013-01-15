/**
 * Copyright 2012 multibit.org
 *
 * Licensed under the MIT license (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License
 * at
 *
 * http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package org.multibit.controller;

import java.io.IOException;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.ArrayList;

import org.multibit.file.FileHandler;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.StatusEnum;
import org.multibit.model.WalletBusyListener;
import org.multibit.network.MultiBitService;
import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitResponse;
import org.multibit.viewsystem.MultiBitViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.GetDataMessage;
import com.google.bitcoin.core.Message;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionConfidence.ConfidenceType;
import com.google.bitcoin.core.VerificationException;
import com.google.bitcoin.core.Wallet;
import java.util.Locale;
import org.multibit.model.MultiBitModel;

/**
 * The MVC controller for MultiBit.
 *
 * @author jim
 */
public class MultiBitController extends AbstractCoreController implements PeerEventListener {

    private Logger log = LoggerFactory.getLogger(MultiBitController.class);

    
    /**
     * The view systems under control of the CoreController.
     */
    private Collection<MultiBitViewSystem> viewSystems;
    
    /**
     * The WalletBusy listeners
     */
    private Collection<WalletBusyListener> walletBusyListeners = null;
    /**
     * The data model backing the views.
     */
    private MultiBitService multiBitService = null;
    /**
     * Class encapsulating File IO.
     */
    private FileHandler fileHandler = null;
    /**
     * Class encapsulating the location of the Application Data Directory.
     */
    
    

    public MultiBitController(Controller coreController) {
        super(coreController);
        viewSystems = new ArrayList<MultiBitViewSystem>();
        walletBusyListeners = new ArrayList<WalletBusyListener>();
        fileHandler = new FileHandler(this);
    }

    public void registerWalletBusyListener(WalletBusyListener walletBusyListener) {
        walletBusyListeners.add(walletBusyListener);
    }

    /**
     * Add a wallet to multibit from a filename.
     *
     * @param walletFilename The wallet filename
     *
     * @return The model data
     */
    public PerWalletModelData addWalletFromFilename(String walletFilename) throws IOException {
        PerWalletModelData perWalletModelDataToReturn = null;
        if (multiBitService != null) {
            perWalletModelDataToReturn = multiBitService.addWalletFromFilename(walletFilename);
        }
        return perWalletModelDataToReturn;
    }
    

    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        }

        super.fireDataChanged();
    }

    /**
     * Fire that a wallet has changed its busy state.
     */
    public void fireWalletBusyChange(boolean newWalletIsBusy) {
        for (WalletBusyListener walletBusyListener : walletBusyListeners) {
            walletBusyListener.walletBusyChange(newWalletIsBusy);
        }
    }

    /**
     * The controller listens for PeerGroup events and notifies interested
     * parties
     */
    @Override
    public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft) {
        onBlocksDownloaded(peer, block, blocksLeft, true);
    }

    public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft, boolean checkIfBlockNeedsWriting) {
        fireBlockDownloaded();
    }

    @Override
    public void onChainDownloadStarted(Peer peer, int blocksLeft) {
        // log.debug("onChainDownloadStarted called");
        fireBlockDownloaded();
    }

    @Override
    public void onPeerConnected(Peer peer, int peerCount) {
        //log.debug("Peer '" + peer.toString() + "' connected . PeerCount = " + peerCount);
        if (peerCount >= 1) {
            setOnlineStatus(StatusEnum.ONLINE);
        }
        if (getModel() != null) {
            getModel().setNumberOfConnectedPeers(peerCount);
        }
        SendBitcoinConfirmPanel.updatePanel(null);
    }

    @Override
    public void onPeerDisconnected(Peer peer, int peerCount) {
        //log.debug("Peer '" + peer.toString() + "' disconnected . PeerCount = " + peerCount);
        if (peerCount == 0) {
            setOnlineStatus(StatusEnum.CONNECTING);
        }
        if (getModel() != null) {
            getModel().setNumberOfConnectedPeers(peerCount);
        }
        SendBitcoinConfirmPanel.updatePanel(null);
    }

    public void setOnlineStatus(StatusEnum statusEnum) {
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.setOnlineStatus(statusEnum);
        }
    }

    /**
     * Method called by downloadListener whenever a block is downloaded.
     */
    public void fireBlockDownloaded() {
        // log.debug("Fire blockdownloaded");
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.blockDownloaded();
        }
    }

    private void checkForDirtyWallets(Transaction transaction) {
        List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();
        for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
            try {
                if (loopPerWalletModelData.getWallet().isTransactionRelevant(transaction, true)) {
                    loopPerWalletModelData.setDirty(true);
                    //log.debug("Marking wallet '" + loopPerWalletModelData.getWalletFilename() + "' as dirty.");
                }
            } catch (ScriptException e) {
                log.debug(e.getMessage());
            }
        }
    }

    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.onCoinsSent(wallet, transaction, prevBalance, newBalance);
        }
    }

    public void onWalletChanged(Wallet wallet) {
        // TODO
    }

    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
        //log.debug("Firing confidence change in onTransactionConfidenceChanged.");

        // Set the depth in blocks as this does not seem to get updated anywhere.
        if (getMultiBitService().getChain() != null && transaction.getConfidence().getConfidenceType() == ConfidenceType.BUILDING) {
            transaction.getConfidence().setDepthInBlocks(getMultiBitService().getChain().getBestChainHeight() - transaction.getConfidence().getAppearedAtChainHeight() + 1);
        }
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.onTransactionConfidenceChanged(wallet, transaction);
        }
        checkForDirtyWallets(transaction);
    }

    public void onKeyAdded(ECKey ecKey) {
        log.debug("Key added : " + ecKey.toString());
    }

    public void onReorganise(Wallet wallet) {
        List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();
        for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
            if (loopPerWalletModelData.getWallet().equals(wallet)) {
                loopPerWalletModelData.setDirty(true);
                log.debug("Marking wallet '" + loopPerWalletModelData.getWalletFilename() + "' as dirty.");
            }
        }
        for (MultiBitViewSystem viewSystem : this.viewSystems) {
            viewSystem.onReorganize(wallet);
        }
    }

    public MultiBitService getMultiBitService() {
        return multiBitService;
    }

    public void setMultiBitService(MultiBitService multiBitService) {
        this.multiBitService = multiBitService;
    }

    public FileHandler getFileHandler() {
        return fileHandler;
    }

    @Override
    public void onQuitEvent(GenericQuitEvent event, GenericQuitResponse response) {
        if (super.isOKToQuit()) {
            ExitAction exitAction = new ExitAction(this, null);
            exitAction.actionPerformed(null);
            response.performQuit();
        } else {
            response.cancelQuit();
        }
    }

    @Override
    public Message onPreMessageReceived(Peer peer, Message message) {
        return message;
    }

    @Override
    public void onTransaction(Peer peer, Transaction transaction) {
        // Loop through all the wallets, seeing if the transaction is relevant
        // and adding them as pending if so.
        // (As of 25 Oct 2012, intrawallet zero confirmation tx are not seen if this code is removed)
        if (transaction != null) {
            try {
                java.util.List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();

                if (perWalletModelDataList != null) {
                    for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                        Wallet loopWallet = perWalletModelData.getWallet();
                        if (loopWallet != null) {
                            if (loopWallet.isTransactionRelevant(transaction, true)) {
                                // The perWalletModelData is marked as dirty.
                                if (perWalletModelData.getWalletInfo() != null) {
                                    synchronized (perWalletModelData.getWalletInfo()) {
                                        perWalletModelData.setDirty(true);
                                    }
                                } else {
                                    perWalletModelData.setDirty(true);
                                }
                                if (loopWallet.getTransaction(transaction.getHash()) == null) {
                                    log.debug("MultiBit adding a new pending transaction for the wallet '"
                                            + perWalletModelData.getWalletDescription() + "'\n" + transaction.toString());
                                    loopWallet.receivePending(transaction);
                                }
                            }
                        }
                    }
                }
            } catch (ScriptException e) {
                log.error(e.getMessage(), e);
            } catch (VerificationException e) {
                log.error(e.getMessage(), e);
            }
        }
    }

    @Override
    public List<Message> getData(Peer peer, GetDataMessage m) {
        return null;
    }

    /**
     * Called from replay when a block is replayed from cache.
     *
     * @param block
     */
    public void onBlock(StoredBlock storedBlock, Block block) {
        // Loop through the transactions in the block.
        List<Transaction> transactions = block.getTransactions();
        if (transactions != null) {
            for (Transaction transaction : transactions) {
                // loop through all the wallets, seeing if the transaction is relevant
                if (transaction != null) {
                    try {
                        java.util.List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();

                        if (perWalletModelDataList != null) {
                            for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                                Wallet loopWallet = perWalletModelData.getWallet();
                                if (loopWallet != null) {
                                    if (loopWallet.isTransactionRelevant(transaction, true)) {
                                        // the perWalletModelData is marked as dirty
                                        if (perWalletModelData.getWalletInfo() != null) {
                                            synchronized (perWalletModelData.getWalletInfo()) {
                                                perWalletModelData.setDirty(true);
                                            }
                                        } else {
                                            perWalletModelData.setDirty(true);
                                        }
                                        if (loopWallet.getTransaction(transaction.getHash()) == null) {
                                            log.debug("MultiBit adding a new transaction from a block for the wallet '"
                                                    + perWalletModelData.getWalletDescription() + "'\n" + transaction.toString());
                                            loopWallet.receiveFromBlock(transaction, storedBlock, com.google.bitcoin.core.BlockChain.NewBlockType.BEST_CHAIN);
                                        }
                                    }
                                }
                            }
                        }
                    } catch (ScriptException e) {
                        log.error(e.getMessage(), e);
                    } catch (VerificationException e) {
                        log.error(e.getMessage(), e);
                    }
                }
            }
        }
    }
}
