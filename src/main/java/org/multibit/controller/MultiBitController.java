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
package org.multibit.controller;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Localiser;
import org.multibit.file.FileHandler;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.StatusEnum;
import org.multibit.model.WalletBusyListener;
import org.multibit.network.MultiBitService;
import org.multibit.platform.listener.GenericAboutEvent;
import org.multibit.platform.listener.GenericAboutEventListener;
import org.multibit.platform.listener.GenericOpenURIEvent;
import org.multibit.platform.listener.GenericOpenURIEventListener;
import org.multibit.platform.listener.GenericPreferencesEvent;
import org.multibit.platform.listener.GenericPreferencesEventListener;
import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitEventListener;
import org.multibit.platform.listener.GenericQuitResponse;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
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
import com.google.bitcoin.uri.BitcoinURI;
import com.google.bitcoin.uri.BitcoinURIParseException;

/**
 * The MVC controller for MultiBit.
 * 
 * @author jim
 */
public class MultiBitController implements PeerEventListener, GenericOpenURIEventListener, GenericPreferencesEventListener,
        GenericAboutEventListener, GenericQuitEventListener {

    public static final String ENCODED_SPACE_CHARACTER = "%20";

    private Logger log = LoggerFactory.getLogger(MultiBitController.class);

    /**
     * The view systems under control of the MultiBitController.
     */
    private Collection<ViewSystem> viewSystems;

    /**
     * The WalletBusy listeners
     */
    private Collection<WalletBusyListener> walletBusyListeners;
    
    /**
     * The data model backing the views.
     */
    private MultiBitModel model;

    /**
     * The localiser used to localise everything.
     */
    private Localiser localiser;

    /**
     * The bitcoinj network interface.
     */
    private MultiBitService multiBitService;

    /**
     * Class encapsulating File IO.
     */
    private FileHandler fileHandler;

    /**
     * Class encapsulating the location of the Application Data Directory.
     */
    private ApplicationDataDirectoryLocator applicationDataDirectoryLocator;

    /**
     * Multiple threads will write to this variable so require it to be volatile
     * to ensure that latest write is what gets read
     */
    private volatile URI rawBitcoinURI = null;

    private volatile boolean applicationStarting = true;
    
    /**
     * Used for testing only.
     */
    public MultiBitController() {
        this(null);
    }

    public MultiBitController(ApplicationDataDirectoryLocator applicationDataDirectoryLocator) {
        this.applicationDataDirectoryLocator = applicationDataDirectoryLocator;

        viewSystems = new ArrayList<ViewSystem>();
        
        walletBusyListeners = new ArrayList<WalletBusyListener>();

        fileHandler = new FileHandler(this);

        // by default localise to English
        localiser = new Localiser(Locale.ENGLISH);
    }

    /**
     * Display the view specified.
     * 
     * @param viewToDisplay
     *            View to display. Must be one of the View constants
     */
    public void displayView(int viewToDisplay) {
        // log.debug("Displaying view '" + viewToDisplay + "'");

        // tell all views to close the current view
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(viewToDisplay);

        // tell all views which view to display
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.displayView(getCurrentView());
        }
    }

    /**
     * Display the help context specified.
     * 
     * @param helpContextToDisplay
     *            The help context to display. A path in the help
     */
    public void displayHelpContext(String helpContextToDisplay) {
        log.debug("Displaying help context '" + helpContextToDisplay + "'");
        // tell all views to close the current view
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(View.HELP_CONTENTS_VIEW);

        // tell all views which view to display
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.setHelpContext(helpContextToDisplay);
            viewSystem.displayView(View.HELP_CONTENTS_VIEW);
        }
    }

    /**
     * Register a new MultiBitViewSystem from the list of views that are managed.
     * 
     * @param viewSystem
     *            system
     */
    public void registerViewSystem(ViewSystem viewSystem) {
        viewSystems.add(viewSystem);
    }
    
    /**
     * Register a new WalletBusyListener
     */
    public void registerWalletBusyListener(WalletBusyListener walletBusyListener) {
        walletBusyListeners.add(walletBusyListener);
    }

    public MultiBitModel getModel() {
        return model;
    }

    public void setModel(MultiBitModel model) {
        this.model = model;
    }

    /**
     * Add a wallet to multibit from a filename.
     * 
     * @param walletFilename
     *            The wallet filename
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

    /**
     * The language has been changed.
     */
    public void fireDataStructureChanged() {
        Locale newLocale = new Locale(model.getUserPreference(MultiBitModel.USER_LANGUAGE_CODE));
        localiser.setLocale(newLocale);

        int viewToDisplay = getCurrentView();

        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews(true);
        }

        setCurrentView(viewToDisplay);
        fireDataChanged();
    }

    /**
     * Fire that all the views need recreating.
     */
    public void fireRecreateAllViews(boolean initUI) {
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews(initUI);
        }
    }

    /**
     * Fire that the model data has changed.
     */
    public void fireDataChanged() {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.fireDataChanged();
        }
    }

    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        }

        fireDataChanged();
    }
    
    
    /**
     * Fire that a wallet has changed its busy state.
     */
    public void fireWalletBusyChange(boolean newWalletIsBusy) {
        for (WalletBusyListener walletBusyListener : walletBusyListeners) {
            walletBusyListener.walletBusyChange(newWalletIsBusy);
        }
    }

    public Localiser getLocaliser() {
        return localiser;
    }

    public void setLocaliser(Localiser localiser) {
        this.localiser = localiser;
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
        SendBitcoinConfirmPanel.updatePanel(null);    }

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
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.setOnlineStatus(statusEnum);
        }
    }

    /**
     * Method called by downloadListener whenever a block is downloaded.
     */
    public void fireBlockDownloaded() {
        // log.debug("Fire blockdownloaded");
        for (ViewSystem viewSystem : viewSystems) {
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
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (ViewSystem viewSystem : viewSystems) {
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
        for (ViewSystem viewSystem : viewSystems) {
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
        for (ViewSystem viewSystem : viewSystems) {
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

    public ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator() {
        return applicationDataDirectoryLocator;
    }

    public int getCurrentView() {
        if (getModel() != null) {
            return getModel().getCurrentView();
        } else {
            return View.DEFAULT_VIEW;
        }
    }

    public void setCurrentView(int view) {
        // log.debug("setCurrentView = " + view);
        if (getModel() != null) {
            getModel().setCurrentView(view);
        }
    }

    public void setApplicationStarting(boolean applicationStarting) {
        this.applicationStarting = applicationStarting;
    }

    @Override
    public synchronized void onOpenURIEvent(GenericOpenURIEvent event) {
        rawBitcoinURI = event.getURI();
        log.debug("Controller received open URI event with URI='{}'", rawBitcoinURI.toASCIIString());
        if (!applicationStarting) {
            log.debug("Open URI event handled immediately");
            handleOpenURI();
        } else {
            log.debug("Open URI event not handled immediately because application is still starting");
        }
    }

    public synchronized void handleOpenURI() {
        log.debug("handleOpenURI called and rawBitcoinURI ='" + rawBitcoinURI + "'");

        // get the open URI configuration information
        String showOpenUriDialogText = getModel().getUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        String useUriText = getModel().getUserPreference(MultiBitModel.OPEN_URI_USE_URI);

        if (Boolean.FALSE.toString().equalsIgnoreCase(useUriText)
                && Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // ignore open URI request
            log.debug("Bitcoin URI ignored because useUriText = '" + useUriText + "', showOpenUriDialogText = '"
                    + showOpenUriDialogText + "'");
            org.multibit.message.Message message = new org.multibit.message.Message(localiser.getString("showOpenUriView.paymentRequestIgnored"));
            MessageManager.INSTANCE.addMessage(message);
            
            return;
        }
        if (rawBitcoinURI == null || rawBitcoinURI.equals("")) {
            log.debug("No Bitcoin URI found to handle");
            // displayView(getCurrentView());
            return;
        }
        // Process the URI
        // TODO Consider handling the possible runtime exception at a suitable
        // level for recovery.

        // Early MultiBit versions did not URL encode the label hence may
        // have illegal embedded spaces - convert to ENCODED_SPACE_CHARACTER i.e
        // be lenient
        String uriString = rawBitcoinURI.toString().replace(" ", ENCODED_SPACE_CHARACTER);
        BitcoinURI bitcoinURI = null;
        try {
            bitcoinURI = new BitcoinURI(this.getModel().getNetworkParameters(), uriString);
        } catch (BitcoinURIParseException pe) {
            log.error("Could not parse the uriString '" + uriString + "', aborting");
            return;
        }

        // Convert the URI data into suitably formatted view data.
        String address = bitcoinURI.getAddress().toString();
        String label = "";
        try {
            // No label? Set it to a blank String otherwise perform a URL decode
            // on it just to be sure.
            label = null == bitcoinURI.getLabel() ? "" : URLDecoder.decode(bitcoinURI.getLabel(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            log.error("Could not decode the label in UTF-8. Unusual URI entry or platform.");
        }
        // No amount? Set it to zero.
        BigInteger numericAmount = null == bitcoinURI.getAmount() ? BigInteger.ZERO : bitcoinURI.getAmount();
        String amount = getLocaliser().bitcoinValueToStringNotLocalised(numericAmount, false, false);

        if (Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // Do not show confirm dialog - go straight to send view.
            // Populate the model with the URI data.
            getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, address);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, label);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, amount);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW, "true");
            log.debug("Routing straight to send view for address = " + address);

            getModel().setUserPreference(MultiBitModel.BRING_TO_FRONT, "true");
            displayView(View.SEND_BITCOIN_VIEW);
            return;
        } else {
            // Show the confirm dialog to see if the user wants to use URI.
            // Populate the model with the URI data.
            getModel().setUserPreference(MultiBitModel.OPEN_URI_ADDRESS, address);
            getModel().setUserPreference(MultiBitModel.OPEN_URI_LABEL, label);
            getModel().setUserPreference(MultiBitModel.OPEN_URI_AMOUNT, amount);
            log.debug("Routing to show open uri view for address = " + address);

            displayView(View.SHOW_OPEN_URI_DIALOG_VIEW);
            return;
        }
    }

    @Override
    public void onPreferencesEvent(GenericPreferencesEvent event) {
        displayView(View.PREFERENCES_VIEW);
    }

    @Override
    public void onAboutEvent(GenericAboutEvent event) {
        displayView(View.HELP_ABOUT_VIEW);
    }

    @Override
    public void onQuitEvent(GenericQuitEvent event, GenericQuitResponse response) {
        if (isOKToQuit()) {
            ExitAction exitAction = new ExitAction(this, null);
            exitAction.actionPerformed(null);
            response.performQuit();
        } else {
            response.cancelQuit();
        }
    }

    /**
     * @return True if the application can quit
     */
    private boolean isOKToQuit() {
        return true;
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
                                    synchronized(perWalletModelData.getWalletInfo()) {
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
                                            synchronized(perWalletModelData.getWalletInfo()) {
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
