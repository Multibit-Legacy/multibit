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
import org.multibit.model.bitcoin.wallet.WalletData;
import org.multibit.model.bitcoin.StatusEnum;
import org.multibit.model.bitcoin.wallet.WalletBusyListener;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.VerificationException;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletEventListener;
import com.google.bitcoin.uri.BitcoinURI;
import com.google.bitcoin.uri.BitcoinURIParseException;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.CoreModel;
import org.multibit.model.exchange.ExchangeModel;

/**
 * The MVC controller for MultiBit.
 * 
 * @author jim
 */
public class MultiBitController implements GenericOpenURIEventListener, GenericPreferencesEventListener,
        GenericAboutEventListener, GenericQuitEventListener, WalletEventListener {

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
    private CoreModel coreModel;
    private BitcoinModel bitcoinModel;
    private ExchangeModel exchangeModel;

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
     * The listener handling Peer events.
     */
    private PeerEventListener peerEventListener;
    
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

        // NO
        viewSystems = new ArrayList<ViewSystem>();
        
        walletBusyListeners = new ArrayList<WalletBusyListener>();

        fileHandler = new FileHandler(this);

        peerEventListener = new MultiBitPeerEventListener(this);
        
        // by default localise to English
        localiser = new Localiser(Locale.ENGLISH);
    }

    // models
    public void setCoreModel(CoreModel model) {
        this.coreModel = model;
    }
    public CoreModel getCoreModel() {
        return this.coreModel;
    }

    public void setBitcoinModel(BitcoinModel model) {
        this.bitcoinModel = model;
    }
    public BitcoinModel getBitcoinModel() {
        return this.bitcoinModel;
    }

    public void setExchangeModel(ExchangeModel model) {
        this.exchangeModel = model;
    }
    public ExchangeModel getExchangeModel() {
        return this.exchangeModel;
    }

    
    
    /**
     * Display the view specified.
     * 
     * @param viewToDisplay
     *            View to display. Must be one of the View constants
     */
    public void displayView(View viewToDisplay) {
        log.debug("Displaying view '" + viewToDisplay + "'");

        // NO

        // Tell all views to close the current view.
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(viewToDisplay);

        // Tell all views which view to display.
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
        // Tell all views to close the current view.

        // NO

        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(View.HELP_CONTENTS_VIEW);

        // Tell all views which view to display.
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
        // NO
        viewSystems.add(viewSystem);
    }
    
    /**
     * Register a new WalletBusyListener.
     */
    public void registerWalletBusyListener(WalletBusyListener walletBusyListener) {
        walletBusyListeners.add(walletBusyListener);
    }

    /**
     * Add a wallet to multibit from a filename.
     * 
     * @param walletFilename
     *            The wallet filename
     * 
     * @return The model data
     */
    public WalletData addWalletFromFilename(String walletFilename) throws IOException {
        WalletData perWalletModelDataToReturn = null;
        if (multiBitService != null) {
            perWalletModelDataToReturn = multiBitService.addWalletFromFilename(walletFilename);
        }
        return perWalletModelDataToReturn;
    }

    /**
     * The language has been changed.
     */
    public void fireDataStructureChanged() {
        log.debug("fireDataStructureChanged called");

        Locale newLocale = new Locale(this.getCoreModel().getUserPreference(CoreModel.USER_LANGUAGE_CODE));

        localiser.setLocale(newLocale);

        View viewToDisplay = getCurrentView();

        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews(true, viewToDisplay);
        }

        setCurrentView(viewToDisplay);
        fireDataChangedUpdateNow();
    }

    /**
     * Fire that all the views need recreating.
     */
    public void fireRecreateAllViews(boolean initUI) {
        log.debug("fireRecreateAllViews called");
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews(initUI, getCurrentView());
        }
    }

    /**
     * Fire that the model data has changed and the UI should be updated immediately.
     */
    public void fireDataChangedUpdateNow() {
        log.debug("fireDataChangedUpdateNow called");
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.fireDataChangedUpdateNow();
        }
    }


    /**
     * Fire that the model data has changed and similar events are to be collapsed.
     */
    public void fireDataChangedUpdateLater() {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.fireDataChangedUpdateLater();
        }
    }

    public void fireFilesHaveBeenChangedByAnotherProcess(WalletData perWalletModelData) {
        log.debug("fireFilesHaveBeenChangedByAnotherProcess called");
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        }

        fireDataChangedUpdateNow();
    }
       
    /**
     * Fire that a wallet has changed its busy state.
     */
    public void fireWalletBusyChange(boolean newWalletIsBusy) {
        log.debug("fireWalletBusyChange called");
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

    public void setOnlineStatus(StatusEnum statusEnum) {
        log.debug("setOnlineStatus called");
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

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        log.debug("onCoinsReceived called");
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        log.debug("onCoinsSent called");
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onCoinsSent(wallet, transaction, prevBalance, newBalance);
        }
    }
    
    @Override
    public void onWalletChanged(Wallet wallet) {
        if (wallet == null) {
            return;
        }
        // log.debug("onWalletChanged called");
        final int walletIdentityHashCode = System.identityHashCode(wallet);
        for (WalletData loopPerWalletModelData : getBitcoinModel().getPerWalletModelDataList()) {
            // Find the wallet object and mark as dirty.
            if (System.identityHashCode(loopPerWalletModelData.getWallet()) == walletIdentityHashCode) {
                loopPerWalletModelData.setDirty(true);
                break;
            }
        }

        fireDataChangedUpdateLater();
    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
        //log.debug("onTransactionConfidenceChanged called");
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onTransactionConfidenceChanged(wallet, transaction);
        }
    }

    @Override
    public void onKeyAdded(ECKey ecKey) {
        log.debug("Key added : " + ecKey.toString());
    }

    @Override
    public void onReorganize(Wallet wallet) {
        log.debug("onReorganize called");
        List<WalletData> perWalletModelDataList = getBitcoinModel().getPerWalletModelDataList();
        
        for (WalletData loopPerWalletModelData : perWalletModelDataList) {

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

    public View getCurrentView() {
        View view = (null == this.getCoreModel()) ? null : this.getCoreModel().getCurrentView();
        
        return (null == view) ? View.DEFAULT_VIEW() : view;
    }

    public void setCurrentView(View view) {
        // log.debug("setCurrentView = " + view);
        if (this.getCoreModel() != null) {
            this.getCoreModel().setCurrentView(view);
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
        String showOpenUriDialogText = this.getCoreModel().getUserPreference(BitcoinModel.OPEN_URI_SHOW_DIALOG);
        String useUriText = getCoreModel().getUserPreference(BitcoinModel.OPEN_URI_USE_URI);

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
            bitcoinURI = new BitcoinURI(this.getBitcoinModel().getNetworkParameters(), uriString);
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
            this.getBitcoinModel().setActiveWalletPreference(BitcoinModel.SEND_ADDRESS, address);
            this.getBitcoinModel().setActiveWalletPreference(BitcoinModel.SEND_LABEL, label);
            this.getBitcoinModel().setActiveWalletPreference(BitcoinModel.SEND_AMOUNT, amount);
            this.getBitcoinModel().setActiveWalletPreference(BitcoinModel.SEND_PERFORM_PASTE_NOW, "true");
            log.debug("Routing straight to send view for address = " + address);

            this.getCoreModel().setUserPreference(BitcoinModel.BRING_TO_FRONT, "true");
            displayView(View.SEND_BITCOIN_VIEW);
            return;
        } else {
            // Show the confirm dialog to see if the user wants to use URI.
            // Populate the model with the URI data.
            this.getCoreModel().setUserPreference(BitcoinModel.OPEN_URI_ADDRESS, address);
            this.getCoreModel().setUserPreference(BitcoinModel.OPEN_URI_LABEL, label);
            this.getCoreModel().setUserPreference(BitcoinModel.OPEN_URI_AMOUNT, amount);
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
                        java.util.List<WalletData> perWalletModelDataList = getBitcoinModel().getPerWalletModelDataList();

                        if (perWalletModelDataList != null) {
                            for (WalletData perWalletModelData : perWalletModelDataList) {
                                Wallet loopWallet = perWalletModelData.getWallet();
                                if (loopWallet != null) {
                                    if (loopWallet.isTransactionRelevant(transaction)) {
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

    public PeerEventListener getPeerEventListener() {
        return peerEventListener;
    }
}
