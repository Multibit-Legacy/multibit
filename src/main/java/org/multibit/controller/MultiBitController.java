package org.multibit.controller;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EmptyStackException;
import java.util.Locale;
import java.util.Properties;
import java.util.Stack;

import org.multibit.Localiser;
import org.multibit.model.MultiBitModel;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;

import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * the MVC controller for Multibit - this is loosely based on the Apache Struts
 * controller
 * 
 * @author jim
 * 
 */
public class MultiBitController implements PeerEventListener {

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

    public MultiBitController() {
        this(null);
    }

    public MultiBitController(Properties userPreferences) {
        viewSystems = new ArrayList<ViewSystem>();

        // initialise everything to look at the stored opened view and previous view
        // if no properties passed in just initialise to the transactions view
        int previousView = View.TRANSACTIONS_VIEW;
        int initialView = View.TRANSACTIONS_VIEW;
        if (userPreferences != null) {
            String viewString = (String) userPreferences.get(MultiBitModel.SELECTED_VIEW);
            if (viewString != null) {
                try {
                    initialView = Integer.parseInt(viewString);
                } catch (NumberFormatException nfe) {
                    // carry on
                }
            }
            String previouslySelectedViewString = (String) userPreferences.get(MultiBitModel.PREVIOUSLY_SELECTED_VIEW);
            if (previouslySelectedViewString != null) {
                try {
                    previousView = Integer.parseInt(previouslySelectedViewString);
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
     */
    public void setActionForwardToChild(ActionForward actionForward) {
        // push current view onto the stack
        viewStack.push(currentView);
        determineNextView(actionForward);
        displayNextView(ViewSystem.NEW_VIEW_IS_CHILD_OF_PREVIOUS);
    }

    /**
     * set the action forward that will be used to determined the next view to
     * display where the next view is a sibling of the current view
     * 
     * @param actionForward
     */
    public void setActionForwardToSibling(ActionForward actionForward) {
        determineNextView(actionForward);

        // do not change the call stack
        displayNextView(ViewSystem.NEW_VIEW_IS_SIBLING_OF_PREVIOUS);
    }

    /**
     * set the next view to be the parent of the current
     * 
     * @param actionForward
     */
    public void setActionForwardToParent() {
        try {
            nextView = viewStack.pop();
        } catch (EmptyStackException ese) {
            log.error("setActionForwardToParent failed", ese);
            // go to the transactions page anyhow
            nextView = View.TRANSACTIONS_VIEW;
            viewStack.push(nextView);
        }
        displayNextView(ViewSystem.NEW_VIEW_IS_PARENT_OF_PREVIOUS);
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
     * @return next view (on View enum)
     */
    public void determineNextView(ActionForward actionForward) {
        switch (actionForward) {
        case FORWARD_TO_SAME: {
            // redisplay the sameView
            nextView = currentView;
            break;
        }
        case FORWARD_TO_PREVIOUS: {
            // go back to the previously displayed view
            nextView = previousView;
            break;
        }
        case FORWARD_TO_OPEN_WALLET: {
            // show the open wallet view
            // should check actually on home page
            nextView = View.OPEN_WALLET_VIEW;
            break;
        }
        case FORWARD_TO_SAVE_WALLET_AS: {
            // show the open wallet view
            // should check actually on home page
            nextView = View.SAVE_WALLET_AS_VIEW;
            break;
        }
        case FORWARD_TO_RECEIVE_BITCOIN: {
            // show the receive bitcoin view
            // should check actually on home page
            nextView = View.RECEIVE_BITCOIN_VIEW;
            break;
        }
        case FORWARD_TO_SEND_BITCOIN: {
            // show the send bitcoin view
            // should check actually on home page
            nextView = View.SEND_BITCOIN_VIEW;
            break;
        }
        case FORWARD_TO_SEND_BITCOIN_CONFIRM: {
            // show the send bitcoin confirm view
            // should check actually on send bitcoin view
            nextView = View.SEND_BITCOIN_CONFIRM_VIEW;
            break;
        }
        case FORWARD_TO_HELP_ABOUT: {
            // show the help about view
            // should check actually on home page
            nextView = View.HELP_ABOUT_VIEW;
            break;
        }
        case FORWARD_TO_HELP_CONTENTS: {
            // show the help contents view
            // should check actually on home page
            nextView = View.HELP_CONTENTS_VIEW;
            break;
        }

        case FORWARD_TO_PREFERENCES: {
            // show the preferences view
            // should check actually on home page
            nextView = View.PREFERENCES_VIEW;
            break;
        }

        case FORWARD_TO_TRANSACTIONS: {
            // show the transactions page
            nextView = View.TRANSACTIONS_VIEW;
            break;
        }

        case FORWARD_TO_VALIDATION_ERROR: {
            // show the validation error view
            nextView = View.VALIDATION_ERROR_VIEW;
            break;
        }

        default: {
            nextView = View.TRANSACTIONS_VIEW;
            break;
        }
        }
    }

    /**
     * @param relationshipOfNewViewToPrevious
     *            - one of ViewSystem relationship constants
     */
    public void displayNextView(int relationshipOfNewViewToPrevious) {
        if (nextView != 0) {
            // cycle the previous / current / next views
            previousView = currentView;
            currentView = nextView;
            nextView = View.UNKNOWN_VIEW;

        } else {
            log.warn("Could not determine next view to display, previousView = {}, currentView = {}",
                    previousView, currentView);
            log.info("Displaying the transaction view anyhow");
            previousView = currentView;
            currentView = View.TRANSACTIONS_VIEW;
        }

        // tell all views to close the previous view
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.navigateAwayFromView(previousView, currentView, relationshipOfNewViewToPrevious);
        }

        // for the top level views, clear the view stack
        // this makes the UI behaviour a bit more 'normal'
        if (currentView == View.TRANSACTIONS_VIEW || currentView == View.RECEIVE_BITCOIN_VIEW
                || currentView == View.SEND_BITCOIN_VIEW || currentView == View.HELP_ABOUT_VIEW
                || currentView == View.HELP_CONTENTS_VIEW || currentView == View.PREFERENCES_VIEW) {
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

    /**
     * deregister a MultiBitViewSystem from the list of views being managed
     * 
     * @param viewSystem
     */
    public void deregisterViewSystem(ViewSystem viewSystem) {
        viewSystems.remove(viewSystem);
    }

    /*
     * display a message to the user - localisation is done by the viewSystems
     * 
     * @param messageKey the key to localise for the message
     * 
     * @param titleKey the key to localise for the title
     */
    public void displayMessage(String messageKey, String titleKey) {
        displayMessage(messageKey, null, titleKey);
    }

    /*
     * display a message to the user - localisation is done by the viewSystems
     * 
     * @param messageKey the key to localise for the message
     * 
     * @param messageData the data used in the message
     * 
     * @param titleKey the key to localise for the title
     * 
     * @param any localisation data
     */
    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.displayMessage(messageKey, messageData, titleKey);
        }
    }

    public MultiBitModel getModel() {
        return model;
    }

    public void setModel(MultiBitModel model) {
        this.model = model;
    }

    /**
     * the language has been changed
     */
    public void fireLanguageChanged() {
        localiser.setLocale(new Locale(model.getUserPreference(MultiBitModel.USER_LANGUAGE_CODE)));

        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews();
        }
    }

    /**
     * the wallet file has been changed
     */
    public void fireWalletChanged() {
        // TODO rewire the blockchain etc to reference the new wallet

        fireRecreateAllViews();
    }

    /**
     * fire that all the views need recreating
     */
    public void fireRecreateAllViews() {
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.recreateAllViews();
        }
    }

    /**
     * firethe model data has changed
     */
    public void fireDataChanged() {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.fireDataChanged();
        }
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
        log.debug("onBlocksDownloaded called");
        fireBlockDownloaded();
    }

    public void onChainDownloadStarted(Peer peer, int blocksLeft) {
        log.debug("onChainDownloadStarted called");
        fireBlockDownloaded();
    }

    public void onPeerConnected(Peer peer, int peerCount) {
        // if now online, notify viewSystems
        // log.debug("MultiBitController#onPeerConnected called - peerCount = "
        // + peerCount);
        if (peerCount == 1) {
            for (ViewSystem viewSystem : viewSystems) {
                viewSystem.nowOnline();
            }
        }
    }

    public void onPeerDisconnected(Peer peer, int peerCount) {
        // log.debug("MultiBitController#onPeerDisconnected called - peerCount = "
        // + peerCount);
        // if now offline, notify viewSystems
        if (peerCount == 0) {
            for (ViewSystem viewSystem : viewSystems) {
                viewSystem.nowOffline();
            }
        }
    }

    /**
     * method called by downloadListener to update download status
     */
    public void updateDownloadStatus(String downloadStatus) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.updateStatusLabel(downloadStatus);
        }
    }

    /**
     * method called by downloadListener whenever a block is downloaded
     */
    public void fireBlockDownloaded() {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.blockDownloaded();
        }
    }

    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    public void onPendingCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (ViewSystem viewSystem : viewSystems) {
            viewSystem.onPendingCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    public void sendCoins(String sendAddressString, String sendLabel, String amount, BigInteger fee) throws IOException,
            AddressFormatException {
        // send the coins
        Transaction sendTransaction = multiBitService.sendCoins(sendAddressString, amount, fee);
        fireRecreateAllViews();
        if (sendTransaction == null) {
            throw new IllegalStateException("No transaction was created after send.   The send may have failed.");
        }
    }

    public void clearViewStack() {
        viewStack.clear();
        viewStack.push(View.TRANSACTIONS_VIEW);
        previousView = View.TRANSACTIONS_VIEW;
    }

    public MultiBitService getMultiBitService() {
        return multiBitService;
    }

    public void setMultiBitService(MultiBitService multiBitService) {
        this.multiBitService = multiBitService;
    }
}
