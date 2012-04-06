package org.multibit.viewsystem.commandline;

import java.io.InputStream;
import java.io.PrintStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.commandline.view.CommandLineView;
import org.multibit.viewsystem.commandline.view.HelpAboutView;
import org.multibit.viewsystem.commandline.view.HelpContentsView;
import org.multibit.viewsystem.commandline.view.HomePageView;
import org.multibit.viewsystem.commandline.view.ReceiveBitcoinView;
import org.multibit.viewsystem.commandline.view.SendBitcoinView;
import org.multibit.viewsystem.commandline.view.SimpleView;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.action.PasteAddressAction;
import org.multibit.viewsystem.swing.action.SendBitcoinConfirmAction;
import org.multibit.viewsystem.swing.action.SendBitcoinNowAction;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;

/**
 * a purely text / command line view for MultiBit
 * 
 * @author jim
 * 
 */
public class CommandLineViewSystem implements ViewSystem {

    private MultiBitController controller;

    private InputStream inputStream;
    private PrintStream printStream;

    private Map<Integer, CommandLineView> viewMap;

    private boolean alreadySaidIsOnline = false;

    /**
     * the current viewId - int - one of the View constants
     */
    private int currentViewId;

    public static String TEXT_VIEW_OUTPUT_PREFIX = "MB:";
    public static String SEPARATOR = " ";
    public static String MESSAGE_PREFIX = "Message   : ";
    public static String DISPLAY_VIEW_PREFIX = "Displaying: ";
    public static String NAVIGATE_AWAY_FROM_VIEW_PREFIX = "Leaving   : ";
    public static String ERROR_PREFIX = "Error     : ";
    public static String DISPLAY_MENU_OPTIONS_PREFIX = "Menu      : ";
    public static String MENU_CHOOSE_PREFIX = "CHOOSE    > ";

    public CommandLineViewSystem(MultiBitController controller) {
        this.controller = controller;

        currentViewId = View.TRANSACTIONS_VIEW;

        inputStream = System.in;
        printStream = System.out;

        populateViewList();
    }

    /**
     * populate the list of views note that the id used for the key matches the
     * constants in the View interface
     */
    private void populateViewList() {
        viewMap = new HashMap<Integer, CommandLineView>();
        MultiBitModel model = controller.getModel();
        Localiser localiser = controller.getLocaliser();

        viewMap.put(View.UNKNOWN_VIEW, new SimpleView(localiser, "Unknown view", inputStream, printStream));

        // home page
        CommandLineView homePageView = new HomePageView(model, localiser, "Home Page", inputStream, printStream);
        Collection<Action> homePageActions = new ArrayList<Action>();
        // homePageActions.add(new HelpContentsAction(controller));
        homePageActions.add(new OpenWalletAction(controller, null, null));
        // homePageActions.add(new ReceiveBitcoinAction(controller));
        // homePageActions.add(new SendBitcoinAction(controller));
        // homePageActions.add(new OpenAddressBookAction(controller, true,
        // true));
        // homePageActions.add(new ShowPreferencesAction(controller));
        // homePageActions.add(new HelpAboutAction(controller));
        homePageActions.add(new ExitAction(controller, null));
        homePageView.setPossibleActions(homePageActions);
        viewMap.put(View.TRANSACTIONS_VIEW, homePageView);

        // Send bitcoin
        CommandLineView sendBitcoinView = new SendBitcoinView(localiser, "Send Bitcoin", inputStream, printStream);
        Collection<Action> sendBitcoinActions = new ArrayList<Action>();
        sendBitcoinActions.add(new PasteAddressAction(controller, null, null));
        sendBitcoinActions.add(new CancelBackToParentAction(controller, null, null));
        sendBitcoinActions.add(new SendBitcoinConfirmAction(controller, null, null));
        sendBitcoinView.setPossibleActions(sendBitcoinActions);
        viewMap.put(View.SEND_BITCOIN_VIEW, sendBitcoinView);

        // Send bitcoin confirm
        CommandLineView sendBitcoinConfirmView = new SendBitcoinView(localiser, "Send Bitcoin Confirm", inputStream, printStream);
        Collection<Action> sendBitcoinConfirmActions = new ArrayList<Action>();
        sendBitcoinConfirmActions.add(new CancelBackToParentAction(controller, null, null));
        sendBitcoinConfirmActions.add(new SendBitcoinNowAction(null, controller, null, null));
        sendBitcoinConfirmView.setPossibleActions(sendBitcoinConfirmActions);
        viewMap.put(View.SEND_BITCOIN_CONFIRM_VIEW, sendBitcoinConfirmView);

        // Receive bitcoin
        CommandLineView receiveBitcoinView = new ReceiveBitcoinView(localiser, "Receive Bitcoin", inputStream, printStream);
        Collection<Action> receiveBitcoinActions = new ArrayList<Action>();
        // receiveBitcoinActions.add(new CopyQRCodeTextAction(controller));
        // receiveBitcoinActions.add(new CreateOrEditAddressAction(controller,
        // true, true));
        receiveBitcoinActions.add(new OkBackToParentAction(controller, null));
        receiveBitcoinView.setPossibleActions(receiveBitcoinActions);
        viewMap.put(View.RECEIVE_BITCOIN_VIEW, receiveBitcoinView);

        // Help contents
        CommandLineView helpContentsView = new HelpContentsView(localiser, "Help Contents", inputStream, printStream);
        Collection<Action> helpContentsActions = new ArrayList<Action>();
        helpContentsActions.add(new OkBackToParentAction(controller, null));
        helpContentsView.setPossibleActions(helpContentsActions);
        viewMap.put(View.HELP_CONTENTS_VIEW, helpContentsView);

        // Help About
        CommandLineView helpAboutView = new HelpAboutView(localiser, "Help About MultiBit", inputStream, printStream);
        Collection<Action> helpAboutActions = new ArrayList<Action>();
        helpAboutActions.add(new OkBackToParentAction(controller, null));
        helpAboutView.setPossibleActions(helpAboutActions);
        viewMap.put(View.HELP_ABOUT_VIEW, helpAboutView);

        // Settings
        CommandLineView settingsView = new SimpleView(localiser, "Preferences", inputStream, printStream);
        Collection<Action> settingsActions = new ArrayList<Action>();
        settingsActions.add(new OkBackToParentAction(controller, null));
        settingsView.setPossibleActions(settingsActions);
        viewMap.put(View.PREFERENCES_VIEW, settingsView);

        // // Address book receiving
        // View addressBookReceivingView = new
        // AddressBookReceivingView(localiser, "Address Book (Receiving)",
        // inputStream, printStream);
        // Collection<Action> addressBookReceivingActions = new
        // ArrayList<Action>();
        // // need selection of address shown
        // addressBookReceivingActions.add(new
        // CopyQRCodeTextAction(controller));
        // addressBookReceivingActions.add(new
        // CreateOrEditAddressAction(controller, true, true));
        // addressBookReceivingActions.add(new
        // CreateOrEditAddressAction(controller, false, true));
        // addressBookReceivingActions.add(new OpenAddressBookAction(controller,
        // false, false));
        // addressBookReceivingActions.add(new
        // OkBackToParentAction(controller));
        // addressBookReceivingView.setPossibleActions(addressBookReceivingActions);
        // viewMap.put(View.ADDRESS_BOOK_RECEIVING_VIEW,
        // addressBookReceivingView);
        //
        // // Address book sending
        // View addressBookSendingView = new AddressBookSendingView(localiser,
        // "Address Book (Sending)", inputStream, printStream);
        // Collection<Action> addressBookSendingActions = new
        // ArrayList<Action>();
        // // need selection of addresses shown
        // addressBookSendingActions.add(new CopyQRCodeTextAction(controller));
        // addressBookSendingActions.add(new
        // CreateOrEditAddressAction(controller, true, false));
        // addressBookSendingActions.add(new
        // CreateOrEditAddressAction(controller, false, false));
        // addressBookSendingActions.add(new OpenAddressBookAction(controller,
        // false, true));
        // addressBookSendingActions.add(new OkBackToParentAction(controller));
        // addressBookSendingView.setPossibleActions(addressBookSendingActions);
        // viewMap.put(View.ADDRESS_BOOK_SENDING_VIEW, addressBookSendingView);
        //
        // // Create new receiving address
        // CommandLineView createNewReceivingAddressView = new
        // SimpleView(localiser, "Create New Receiving Address", inputStream,
        // printStream);
        // Collection<Action> createNewReceivingAddressActions = new
        // ArrayList<Action>();
        // createNewReceivingAddressActions.add(new
        // CancelBackToParentAction(controller, null, null));
        // createNewReceivingAddressActions.add(new
        // OkBackToParentAction(controller, null));
        // createNewReceivingAddressView.setPossibleActions(createNewReceivingAddressActions);
        // viewMap.put(View.CREATE_NEW_RECEIVING_ADDRESS_VIEW,
        // createNewReceivingAddressView);
        //
        // // Create new sending address
        // CommandLineView createNewSendingAddressView = new
        // SimpleView(localiser, "Create New Sending Address", inputStream,
        // printStream);
        // Collection<Action> createNewSendingAddressActions = new
        // ArrayList<Action>();
        // createNewSendingAddressActions.add(new
        // CancelBackToParentAction(controller, null. null));
        // createNewSendingAddressActions.add(new
        // OkBackToParentAction(controller, null));
        // createNewSendingAddressView.setPossibleActions(createNewSendingAddressActions);
        // viewMap.put(View.CREATE_NEW_SENDING_ADDRESS_VIEW,
        // createNewSendingAddressView);

        // // Edit receiving address
        // CommandLineView editReceivingAddressView = new SimpleView(localiser,
        // "Edit Receiving Address", inputStream, printStream);
        // Collection<Action> editReceivingAddressActions = new
        // ArrayList<Action>();
        // editReceivingAddressActions.add(new
        // CancelBackToParentAction(controller, null, null));
        // editReceivingAddressActions.add(new OkBackToParentAction(controller,
        // null));
        // editReceivingAddressView.setPossibleActions(editReceivingAddressActions);
        // viewMap.put(View.EDIT_RECEIVING_ADDRESS_VIEW,
        // editReceivingAddressView);
        //
        // // Edit sending address
        // CommandLineView editSendingAddressView = new SimpleView(localiser,
        // "Edit Sending Address", inputStream, printStream);
        // Collection<Action> editSendingAddressActions = new
        // ArrayList<Action>();
        // editSendingAddressActions.add(new
        // CancelBackToParentAction(controller, null, null));
        // editSendingAddressActions.add(new OkBackToParentAction(controller,
        // null));
        // editSendingAddressView.setPossibleActions(editSendingAddressActions);
        // viewMap.put(View.EDIT_SENDING_ADDRESS_VIEW, editSendingAddressView);
        //
        // Open wallet
        CommandLineView openWalletView = new SimpleView(localiser, "Open Wallet", inputStream, printStream);
        Collection<Action> openWalletActions = new ArrayList<Action>();
        openWalletActions.add(new CancelBackToParentAction(controller, null, null));
        openWalletActions.add(new OkBackToParentAction(controller, null));
        openWalletView.setPossibleActions(openWalletActions);
        viewMap.put(View.OPEN_WALLET_VIEW, openWalletView);
    }

    // public void displayMessage(String messageKey, Object[] messageData,
    // String titleKey) {
    // CommandLineView currentView = viewMap.get(currentViewId);
    //
    // if (currentView != null) {
    // currentView.displayMessage(messageKey, messageData, titleKey);
    // } else {
    // printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX
    // + "No view to output a message to with id " + currentViewId);
    // }
    //
    // }

    public void displayView(int viewId) {
        currentViewId = viewId;

        CommandLineView currentView = viewMap.get(viewId);

        if (currentView != null) {
            currentView.displayView();
        } else {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX + "No view to display with id " + viewId);
        }
    }

    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextViewId, int relationshipOfNewViewToPrevious) {
        printStream.print("\n");
        assert viewToNavigateAwayFrom == currentViewId : "Different current views";
        CommandLineView currentView = viewMap.get(currentViewId);

        currentViewId = View.UNKNOWN_VIEW;

        if (currentView != null) {
            currentView.navigateAwayFromView();
        } else {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX + "No view to navigate away from.   Next view id is "
                    + nextViewId);
        }
    }

    /**
     * no need to recreate views as they are not cached
     */
    public void recreateAllViews() {
        // nothing to do
    }

    public void nowOnline() {
        if (!alreadySaidIsOnline) {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "MultiBit is now online");
        }
        alreadySaidIsOnline = true;
    }

    public void nowOffline() {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "MultiBit is now online");
        alreadySaidIsOnline = false;
    }

    public void updateStatusLabel(String updateDownloadStatus) {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + updateDownloadStatus);
    }

    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {

        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "onCoinsReceived, previous balance = " + prevBalance
                + ", new balance = " + newBalance);
    }

    public void onPendingCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {

        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "onPendingCoinsReceived, previous balance = " + prevBalance
                + ", new balance = " + newBalance);
    }

    public void blockDownloaded() {
        // do nothing
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
        // TODO Auto-generated method stub

    }

    @Override
    public void onReorganize(Wallet wallet) {
        // TODO Auto-generated method stub

    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction tx) {
        // TODO Auto-generated method stub

    }

    @Override
    public void navigateAwayFromView(int viewToNavigateAwayFrom) {
        // TODO Auto-generated method stub

    }

    @Override
    public void fireDataChanged() {
        // TODO Auto-generated method stub

    }

    @Override
    public void recreateAllViews(boolean initUI) {
        // TODO Auto-generated method stub

    }

    @Override
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        // TODO Auto-generated method stub

    }

    @Override
    public void updateStatusLabel(String updateDownloadStatus, boolean clearAutomatically) {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + updateDownloadStatus);
    }

    @Override
    public void updateStatusLabel(String updateDownloadStatus, double percentComplete) {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + updateDownloadStatus + ". " + percentComplete
                + "% complete.");

    }

    @Override
    public void setHelpContext(String helpContextToDisplay) {
        // TODO Auto-generated method stub

    }
}
