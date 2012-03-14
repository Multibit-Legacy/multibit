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
package org.multibit.controller;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EmptyStackException;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Stack;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Localiser;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
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
import org.multibit.qrcode.BitcoinURI;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.GetDataMessage;
import com.google.bitcoin.core.Message;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.VerificationException;
import com.google.bitcoin.core.Wallet;

/**
 * the MVC controller for Multibit
 * 
 * @author jim
 */
public class MultiBitController implements PeerEventListener, GenericOpenURIEventListener, GenericPreferencesEventListener,
        GenericAboutEventListener, GenericQuitEventListener {

    public static final String ENCODED_SPACE_CHARACTER = "%20";

    private Logger log = LoggerFactory.getLogger(MultiBitController.class);

    /**
     * the view systems under control of the MultiBitController
     */
    private Collection<ViewSystem> viewSystems;

    /**
     * the data model backing the views
     */
    private MultiBitModel model;

    /**
     * the localiser used to localise everything
     */
    private Localiser localiser;

    /**
     * the view currently being displayed to the user
     */
    private int currentView;

    /**
     * the previous view that was displayed to the user
     */
    private int previousView;

    /**
     * the next view that will be displayed to the user
     */
    private int nextView;

    /**
     * the stack of views
     */
    private Stack<Integer> viewStack;

    /**
     * the bitcoinj network interface
     */
    private MultiBitService multiBitService;

    /**
     * class encapsulating File IO
     */
    private FileHandler fileHandler;

    /**
     * class encapsulating the location of the Application Data Directory
     */
    private ApplicationDataDirectoryLocator applicationDataDirectoryLocator;

    /**
     * Multiple threads will write to this variable so require it to be volatile
     * to ensure that latest write is what gets read
     */
    private volatile URI rawBitcoinURI = null;

    private volatile boolean applicationStarting = true;

    /**
     * used for testing only
     */
    public MultiBitController() {
        this(null, null);
    }

    public MultiBitController(Properties userPreferences, ApplicationDataDirectoryLocator applicationDataDirectoryLocator) {
        this.applicationDataDirectoryLocator = applicationDataDirectoryLocator;

        viewSystems = new ArrayList<ViewSystem>();

        // initialize everything to look at the stored opened view and previous
        // view
        // if no properties passed in just initialize to the your wallets view
        int previousView = View.YOUR_WALLETS_VIEW;
        int initialView = View.YOUR_WALLETS_VIEW;
        if (userPreferences != null) {
            String viewString = (String) userPreferences.get(MultiBitModel.SELECTED_VIEW);
            if (viewString != null) {
                try {
                    int initialViewInProperties = Integer.parseInt(viewString);

                    // do not open obsolete views
                    if (View.OPEN_WALLET_VIEW != initialViewInProperties && View.SAVE_WALLET_AS_VIEW != initialViewInProperties
                            && View.SEND_BITCOIN_CONFIRM_VIEW != initialViewInProperties) {
                        initialView = initialViewInProperties;
                    }
                } catch (NumberFormatException nfe) {
                    // carry on
                }
            }
        }
        viewStack = new Stack<Integer>();
        viewStack.push(initialView);

        this.previousView = previousView;
        currentView = initialView;
        nextView = initialView;

        fileHandler = new FileHandler(this);
    }

    /**
     * set the action forward that will be used to determined the next view to
     * display
     * 
     * normally called by the action once it has decided what the next view is
     * 
     * this setActionForward should be used when the next view is a child of the
     * current view
     * 
     * @param actionForward
     *            The action forward
     */
    public void setActionForwardToChild(ActionForward actionForward) {
        // push current view onto the stack
        viewStack.push(currentView);
        determineNextView(actionForward);
        displayNextView();
    }

    /**
     * set the action forward that will be used to determined the next view to
     * display where the next view is a sibling of the current view
     * 
     * @param actionForward
     *            The action forward
     */
    public void setActionForwardToSibling(ActionForward actionForward) {
        determineNextView(actionForward);

        // do not change the call stack
        displayNextView();
    }

    /**
     * pop the view stack and then move to a sibling
     */
    public void setActionForwardToSiblingOfParent(ActionForward actionForward) {
        try {
            viewStack.pop();
        } catch (EmptyStackException ese) {
            log.error("setActionForwardToSiblingOfParent stack failure", ese);
        }
        determineNextView(actionForward);
        displayNextView();
    }

    /**
     * set the action forward that will be used to determined the next view to
     * display
     * 
     * normally called by the action once it has decided what the next view is
     * 
     * this setActionForward should be used when the next view is a child of the
     * current view
     * 
     * @param actionForward
     *            The action forward
     */
    public void determineNextView(ActionForward actionForward) {
        switch (actionForward) {
        case FORWARD_TO_SAME: {
            // redisplay the sameView
            nextView = currentView;
            break;
        }

        case FORWARD_TO_RECEIVE_BITCOIN: {
            // show the receive bitcoin view
            nextView = View.RECEIVE_BITCOIN_VIEW;
            break;
        }
        case FORWARD_TO_SEND_BITCOIN: {
            // show the send bitcoin view
            nextView = View.SEND_BITCOIN_VIEW;
            break;
        }
        case FORWARD_TO_HELP_ABOUT: {
            // show the help about view
            nextView = View.HELP_ABOUT_VIEW;
            break;
        }
        case FORWARD_TO_HELP_CONTENTS: {
            // show the help contents view
            nextView = View.HELP_CONTENTS_VIEW;
            break;
        }

        case FORWARD_TO_PREFERENCES: {
            // show the preferences view
            nextView = View.PREFERENCES_VIEW;
            break;
        }

        case FORWARD_TO_TRANSACTIONS: {
            // show the transactions page
            nextView = View.TRANSACTIONS_VIEW;
            break;
        }

        case FORWARD_TO_YOUR_WALLETS: {
            // show the your wallets view
            nextView = View.YOUR_WALLETS_VIEW;
            break;
        }

        case FORWARD_TO_CREATE_BULK_ADDRESSES_VIEW: {
            // show the create bulk addresses view
            nextView = View.CREATE_BULK_ADDRESSES_VIEW;
            break;
        }

        case FORWARD_TO_RESET_TRANSACTIONS_VIEW: {
            // show the reset transactions view
            nextView = View.RESET_TRANSACTIONS_VIEW;
            break;
        }

        case FORWARD_TO_SHOW_OPEN_URI_VIEW: {
            // show the open uri dialog
            nextView = View.SHOW_OPEN_URI_DIALOG_VIEW;
            break;
        }

        case FORWARD_TO_SHOW_IMPORT_PRIVATE_KEYS_VIEW: {
            // show the private key import view
            nextView = View.SHOW_IMPORT_PRIVATE_KEYS_VIEW;
            break;
        }

        case FORWARD_TO_SHOW_EXPORT_PRIVATE_KEYS_VIEW: {
            // show the private key export view
            nextView = View.SHOW_EXPORT_PRIVATE_KEYS_VIEW;
            break;
        }

        default: {
            nextView = View.YOUR_WALLETS_VIEW;
            break;
        }
        }
    }

    public void updateCurrentView() {
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.updateCurrentView();
        } 
    }
    
    /**
     */
    public void displayNextView() {
        if (nextView != 0) {
            // cycle the previous / current / next views
            previousView = currentView;
            currentView = nextView;
            nextView = View.UNKNOWN_VIEW;
        } else {
            log.warn("Could not determine next view to display, previousView = {}, currentView = {}", previousView, currentView);
            log.info("Displaying the my wallets view anyhow");
            previousView = currentView;
            currentView = View.YOUR_WALLETS_VIEW;
        }

        if (previousView == View.YOUR_WALLETS_VIEW && nextView == View.YOUR_WALLETS_VIEW) {
            // no need to redisplay - already there and ok to keep
            return;
        }

        // tell all views to close the previous view
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.navigateAwayFromView(previousView);
        }

        // for the top level views, clear the view stack
        // this makes the UI behaviour a bit more 'normal'
        if (currentView == View.YOUR_WALLETS_VIEW || currentView == View.TRANSACTIONS_VIEW
                || currentView == View.RECEIVE_BITCOIN_VIEW || currentView == View.SEND_BITCOIN_VIEW
                || currentView == View.HELP_ABOUT_VIEW || currentView == View.HELP_CONTENTS_VIEW
                || currentView == View.PREFERENCES_VIEW) {
            clearViewStack();
        }

        // remember the view in the preferences
        model.setUserPreference(MultiBitModel.SELECTED_VIEW, "" + currentView);

        // tell all views which view to display
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.displayView(currentView);
        }
    }

    /**
     * register a new MultiBitViewSystem from the list of views that are managed
     * 
     * @param viewSystem
     *            system
     */
    public void registerViewSystem(ViewSystem viewSystem) {
        viewSystems.add(viewSystem);
    }

    public MultiBitModel getModel() {
        return model;
    }

    public void setModel(MultiBitModel model) {
        this.model = model;
    }

    /**
     * add a wallet to multibit from a filename
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
     * the language has been changed
     */
    public void fireLanguageChanged() {
        Locale newLocale = new Locale(model.getUserPreference(MultiBitModel.USER_LANGUAGE_CODE));
        localiser.setLocale(newLocale);

        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews(true, true);
        }
    }

    /**
     * a new wallet has been created
     */
    public void fireNewWalletCreated() {
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.newWalletCreated();
        }
    }

    /**
     * the wallet file has been changed
     */
    public void fireWalletChanged() {
        fireRecreateAllViews(false);
    }

    /**
     * fire that all the views need recreating
     * 
     * @param clearCache
     *            True if the cache should be cleared
     */
    public void fireRecreateAllViews(boolean clearCache) {
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews(clearCache, false);
        }
    }

    /**
     * fire the model data has changed
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

    public Localiser getLocaliser() {
        return localiser;
    }

    public void setLocaliser(Localiser localiser) {
        this.localiser = localiser;
    }

    /**
     * the controller listens for PeerGroup events and notifies interested
     * parties
     */

    public void onBlocksDownloaded(Peer peer, Block block, int blocksLeft) {
        // log.debug("onBlocksDownloaded called");
        fireBlockDownloaded();
    }

    public void onChainDownloadStarted(Peer peer, int blocksLeft) {
        // log.debug("onChainDownloadStarted called");
        fireBlockDownloaded();
    }

    public void onPeerConnected(Peer peer, int peerCount) {
        // log.debug("Peer = " + peer + " connected.  PeerCount =  " +
        // peerCount);
        if (peerCount >= 1) {
            for (ViewSystem viewSystem : viewSystems) {
                viewSystem.nowOnline();
            }
        }
    }

    public void onPeerDisconnected(Peer peer, int peerCount) {
        // log.debug("Peer = " + peer + " disconnected.  PeerCount =  " +
        // peerCount);
        if (peerCount == 0) {
            for (ViewSystem viewSystem : viewSystems) {
                viewSystem.nowOffline();
            }
        }
    }

    /**
     * Update download status with a message
     * 
     * @param newStatusText
     *            The new status string
     */
    public void updateStatusLabel(String newStatusText) {
        updateStatusLabel(newStatusText, true);
    }

    /**
     * Update download status with a message that can clear automatically
     * 
     * @param newStatusText
     *            The new status string
     * @param clearAutomatically
     *            Clear automatically if true
     */
    public void updateStatusLabel(String newStatusText, boolean clearAutomatically) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.updateStatusLabel(newStatusText, clearAutomatically);
        }
    }

    /**
     * Update download status with percentage task complete (for sync messages)
     * 
     * @param newStatusText
     *            The new status string
     * @param percent
     *            Percent sync is complete
     */
    public void updateStatusLabel(String newStatusText, double percentComplete) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.updateStatusLabel(newStatusText, percentComplete);
        }
    }

    /**
     * method called by downloadListener whenever a block is downloaded
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
                    log.debug("Marking wallet '" + loopPerWalletModelData.getWalletFilename() + "' as dirty.");
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
        //checkForDirtyWallets(transaction);
    }

    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {        
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onCoinsSent(wallet, transaction, prevBalance, newBalance);
        }
        //checkForDirtyWallets(transaction);
    }

    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onTransactionConfidenceChanged(wallet, transaction);
        }
        checkForDirtyWallets(transaction);        
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

    public void clearViewStack() {
        viewStack.clear();
        viewStack.push(View.YOUR_WALLETS_VIEW);
        previousView = View.YOUR_WALLETS_VIEW;
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

    public int getNextView() {
        return nextView;
    }

    public void setNextView(int nextView) {
        this.nextView = nextView;
    }

    public int getCurrentView() {
        return currentView;
    }

    public void setCurrentView(int currentView) {
        this.currentView = currentView;
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
            setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
            updateStatusLabel(localiser.getString("showOpenUriView.paymentRequestIgnored"));
            return;
        }
        if (rawBitcoinURI == null) {
            log.debug("No Bitcoin URI found to handle");
            setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
            return;
        }
        // Process the URI
        // TODO Consider handling the possible runtime exception at a suitable
        // level for recovery

        // Early MultiBit versions did not URL encode the label hence may
        // have illegal embedded spaces - convert to ENCODED_SPACE_CHARACTER i.e
        // be lenient
        String uriString = rawBitcoinURI.toString().replace(" ", ENCODED_SPACE_CHARACTER);
        BitcoinURI bitcoinURI = new BitcoinURI(this.getMultiBitService().getNetworkParameters(), uriString);

        // Convert the URI data into suitably formatted view data
        String address = bitcoinURI.getAddress().toString();
        String label = "";
        try {
            // No label? Set it to a blank String otherwise perform a URL decode
            // on it just to be sure
            label = null == bitcoinURI.getLabel() ? "" : URLDecoder.decode(bitcoinURI.getLabel(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            log.error("Could not decode the label in UTF-8. Unusual URI entry or platform.");
        }
        // No amount? Set it to zero
        BigInteger numericAmount = null == bitcoinURI.getAmount() ? BigInteger.ZERO : bitcoinURI.getAmount();
        String amount = getLocaliser().bitcoinValueToString(numericAmount, false, false);

        if (Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // do not show confirm dialog - go straight to send view
            // Populate the model with the URI data
            getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, address);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, label);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, amount);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW, "true");
            log.debug("Routing straight to send view for address = " + address);

            getModel().setUserPreference(MultiBitModel.BRING_TO_FRONT, "true");
            setActionForwardToSibling(ActionForward.FORWARD_TO_SEND_BITCOIN);
            return;
        } else {
            // show the confirm dialog to see if the user wants to use URI
            // Populate the model with the URI data
            getModel().setUserPreference(MultiBitModel.OPEN_URI_ADDRESS, address);
            getModel().setUserPreference(MultiBitModel.OPEN_URI_LABEL, label);
            getModel().setUserPreference(MultiBitModel.OPEN_URI_AMOUNT, amount);
            log.debug("Routing to show open uri view for address = " + address);

            clearViewStack();
            setActionForwardToSibling(ActionForward.FORWARD_TO_SHOW_OPEN_URI_VIEW);
            return;
        }
    }

    @Override
    public void onPreferencesEvent(GenericPreferencesEvent event) {
        setActionForwardToSibling(ActionForward.FORWARD_TO_PREFERENCES);
    }

    @Override
    public void onAboutEvent(GenericAboutEvent event) {
        setActionForwardToSibling(ActionForward.FORWARD_TO_HELP_ABOUT);
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
    public Message onPreMessageReceived(Peer peer, Message m) {
        return m;
    }

    @Override
    public void onTransaction(Peer peer, Transaction transaction) {
        // loop through all the wallets, seeing if the transaction is relevant
        // and adding them as pending if so
        if (transaction != null) {
            try {
                java.util.List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();

                if (perWalletModelDataList != null) {
                    for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                        Wallet loopWallet = perWalletModelData.getWallet();
                        if (loopWallet.isTransactionRelevant(transaction, true)) {
                            // the perWalletModelData is marked as dirty
                            perWalletModelData.setDirty(true);
                            if (loopWallet.getTransaction(transaction.getHash()) == null) {
                                log.debug("MultiBit adding a new pending transaction for the wallet '"
                                        + perWalletModelData.getWalletDescription() + "'\n" + transaction.toString());
                                loopWallet.receivePending(transaction);
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
}
