package org.multibit.viewsystem.commandline;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.action.CancelBackToParentAction;
import org.multibit.action.CopyAddressAction;
import org.multibit.action.CreateOrEditAddressAction;
import org.multibit.action.ExitAction;
import org.multibit.action.HelpAboutAction;
import org.multibit.action.HelpContentsAction;
import org.multibit.action.OkBackToParentAction;
import org.multibit.action.OpenAddressBookAction;
import org.multibit.action.OpenWalletAction;
import org.multibit.action.PasteAddressAction;
import org.multibit.action.ReceiveBitcoinAction;
import org.multibit.action.SaveWalletAsAction;
import org.multibit.action.SendBitcoinAction;
import org.multibit.action.SendBitcoinConfirmAction;
import org.multibit.action.SendBitcoinNowAction;
import org.multibit.action.ShowPreferencesAction;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.commandline.view.AddressBookReceivingView;
import org.multibit.viewsystem.commandline.view.AddressBookSendingView;
import org.multibit.viewsystem.commandline.view.HelpAboutView;
import org.multibit.viewsystem.commandline.view.HelpContentsView;
import org.multibit.viewsystem.commandline.view.HomePageView;
import org.multibit.viewsystem.commandline.view.ReceiveBitcoinView;
import org.multibit.viewsystem.commandline.view.SendBitcoinView;
import org.multibit.viewsystem.commandline.view.SimpleView;

/**
 * a purely text / command line view for MultiBit
 * 
 * the main purpose of this is to have a simple second view system of MultiBit to
 * ensure separation of concerns
 * 
 * @author jim
 * 
 */
public class CommandLineViewSystem implements ViewSystem {

    private MultiBitController controller;
    
    private InputStream inputStream;
    private PrintStream printStream;

    private Map<Integer, View> viewMap;

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

        currentViewId = View.HOME_PAGE_VIEW;

        inputStream = System.in;
        printStream = System.out;
           
        populateViewList();
    }
    
    /**
     * populate the list of views 
     * note that the id used for the key matches the constants in the View interface
     */
    private void populateViewList() {
        viewMap = new HashMap<Integer, View>();
        MultiBitModel model = controller.getModel();
        Localiser localiser = controller.getLocaliser();

        viewMap.put(View.UNKNOWN_VIEW, new SimpleView(localiser, "Unknown view", inputStream, printStream));
        
        // home page
        View homePageView = new HomePageView(model, localiser, "Home Page", inputStream, printStream);
        Collection<Action> homePageActions = new ArrayList<Action>();
        homePageActions.add(new HelpContentsAction(controller, localiser));
        homePageActions.add(new OpenWalletAction(controller, localiser));
        homePageActions.add(new SaveWalletAsAction(controller, localiser));
        homePageActions.add(new ReceiveBitcoinAction(controller, localiser));
        homePageActions.add(new SendBitcoinAction(controller, localiser));
        homePageActions.add(new OpenAddressBookAction(controller, localiser, true, true));
        homePageActions.add(new ShowPreferencesAction(controller, localiser));
        homePageActions.add(new HelpAboutAction(controller, localiser));
        homePageActions.add(new ExitAction());
        homePageView.setPossibleActions(homePageActions);
        viewMap.put(View.HOME_PAGE_VIEW, homePageView);

        // Send bitcoin
        View sendBitcoinView = new SendBitcoinView(localiser, "Send Bitcoin", inputStream, printStream);
        Collection<Action> sendBitcoinActions = new ArrayList<Action>();
        sendBitcoinActions.add(new PasteAddressAction(controller, localiser));
        sendBitcoinActions.add(new OpenAddressBookAction(controller, localiser, true, false));
        sendBitcoinActions.add(new CancelBackToParentAction(controller, localiser));
        sendBitcoinActions.add(new SendBitcoinConfirmAction(controller, localiser));
        sendBitcoinView.setPossibleActions(sendBitcoinActions);
        viewMap.put(View.SEND_BITCOIN_VIEW, sendBitcoinView);
        
        // Send bitcoin confirm
        View sendBitcoinConfirmView = new SendBitcoinView(localiser, "Send Bitcoin Confirm", inputStream, printStream);
        Collection<Action> sendBitcoinConfirmActions = new ArrayList<Action>();
        sendBitcoinConfirmActions.add(new CancelBackToParentAction(controller, localiser));
        sendBitcoinConfirmActions.add(new SendBitcoinNowAction(controller, localiser));
        sendBitcoinConfirmView.setPossibleActions(sendBitcoinConfirmActions);
        viewMap.put(View.SEND_BITCOIN_CONFIRM_VIEW, sendBitcoinConfirmView);
        
        // Receive bitcoin
        View receiveBitcoinView = new  ReceiveBitcoinView(localiser, "Receive Bitcoin", inputStream, printStream);
        Collection<Action> receiveBitcoinActions = new ArrayList<Action>();
        receiveBitcoinActions.add(new OpenAddressBookAction(controller, localiser, true, true));
        receiveBitcoinActions.add(new CopyAddressAction(controller, localiser));
        receiveBitcoinActions.add(new CreateOrEditAddressAction(controller, localiser, true, true));
        receiveBitcoinActions.add(new OkBackToParentAction(controller, localiser));
        receiveBitcoinView.setPossibleActions(receiveBitcoinActions);
        viewMap.put(View.RECEIVE_BITCOIN_VIEW, receiveBitcoinView);
        
        // Help contents
        View helpContentsView = new HelpContentsView(localiser, "Help Contents", inputStream, printStream);
        Collection<Action> helpContentsActions = new ArrayList<Action>();
        helpContentsActions.add(new OkBackToParentAction(controller, localiser));
        helpContentsView.setPossibleActions(helpContentsActions);
        viewMap.put(View.HELP_CONTENTS_VIEW, helpContentsView);

        // Help About
        View helpAboutView = new HelpAboutView(localiser, "Help About MultiBit", inputStream, printStream);
        Collection<Action> helpAboutActions = new ArrayList<Action>();
        helpAboutActions.add(new OkBackToParentAction(controller, localiser));
        helpAboutView.setPossibleActions(helpAboutActions);
        viewMap.put(View.HELP_ABOUT_VIEW, helpAboutView);
        
        // Settings
        View settingsView = new SimpleView(localiser, "Preferences", inputStream, printStream);
        Collection<Action> settingsActions = new ArrayList<Action>();
        settingsActions.add(new OkBackToParentAction(controller, localiser));
        settingsView.setPossibleActions(settingsActions);
        viewMap.put(View.PREFERENCES_VIEW, settingsView);
        
        // Address book receiving
        View addressBookReceivingView = new AddressBookReceivingView(localiser, "Address Book (Receiving)", inputStream, printStream);
        Collection<Action> addressBookReceivingActions = new ArrayList<Action>();
        // need selection of address shown
        addressBookReceivingActions.add(new CopyAddressAction(controller, localiser));
        addressBookReceivingActions.add(new CreateOrEditAddressAction(controller, localiser, true, true));
        addressBookReceivingActions.add(new CreateOrEditAddressAction(controller, localiser, false, true));
        addressBookReceivingActions.add(new OpenAddressBookAction(controller, localiser, false, false));
        addressBookReceivingActions.add(new OkBackToParentAction(controller, localiser));
        addressBookReceivingView.setPossibleActions(addressBookReceivingActions);
        viewMap.put(View.ADDRESS_BOOK_RECEIVING_VIEW, addressBookReceivingView);
        
        // Address book sending
        View addressBookSendingView = new AddressBookSendingView(localiser, "Address Book (Sending)", inputStream, printStream);
        Collection<Action> addressBookSendingActions = new ArrayList<Action>();
        // need selection of addresses shown
        addressBookSendingActions.add(new CopyAddressAction(controller, localiser));
        addressBookSendingActions.add(new CreateOrEditAddressAction(controller, localiser, true, false));
        addressBookSendingActions.add(new CreateOrEditAddressAction(controller, localiser, false, false));
        addressBookSendingActions.add(new OpenAddressBookAction(controller, localiser, false, true));
        addressBookSendingActions.add(new OkBackToParentAction(controller, localiser));
        addressBookSendingView.setPossibleActions(addressBookSendingActions);
        viewMap.put(View.ADDRESS_BOOK_SENDING_VIEW, addressBookSendingView);
  
        // Create new receiving address
        View createNewReceivingAddressView = new SimpleView(localiser, "Create New Receiving Address", inputStream, printStream);
        Collection<Action> createNewReceivingAddressActions = new ArrayList<Action>();
        createNewReceivingAddressActions.add(new CancelBackToParentAction(controller, localiser));
        createNewReceivingAddressActions.add(new OkBackToParentAction(controller, localiser));
        createNewReceivingAddressView.setPossibleActions(createNewReceivingAddressActions);
        viewMap.put(View.CREATE_NEW_RECEIVING_ADDRESS_VIEW, createNewReceivingAddressView);
  
        // Create new sending address
        View createNewSendingAddressView = new SimpleView(localiser, "Create New Sending Address", inputStream, printStream);
        Collection<Action> createNewSendingAddressActions = new ArrayList<Action>();
        createNewSendingAddressActions.add(new CancelBackToParentAction(controller, localiser));
        createNewSendingAddressActions.add(new OkBackToParentAction(controller, localiser));
        createNewSendingAddressView.setPossibleActions(createNewSendingAddressActions);
        viewMap.put(View.CREATE_NEW_SENDING_ADDRESS_VIEW, createNewSendingAddressView);
 
        // Edit receiving address
        View editReceivingAddressView = new SimpleView(localiser, "Edit Receiving Address", inputStream, printStream);
        Collection<Action> editReceivingAddressActions = new ArrayList<Action>();
        editReceivingAddressActions.add(new CancelBackToParentAction(controller, localiser));
        editReceivingAddressActions.add(new OkBackToParentAction(controller, localiser));
        editReceivingAddressView.setPossibleActions(editReceivingAddressActions);
        viewMap.put(View.EDIT_RECEIVING_ADDRESS_VIEW, editReceivingAddressView);
  
        // Edit sending address
        View editSendingAddressView = new SimpleView(localiser, "Edit Sending Address", inputStream, printStream);
        Collection<Action> editSendingAddressActions = new ArrayList<Action>();
        editSendingAddressActions.add(new CancelBackToParentAction(controller, localiser));
        editSendingAddressActions.add(new OkBackToParentAction(controller, localiser));
        editSendingAddressView.setPossibleActions(editSendingAddressActions);
        viewMap.put(View.EDIT_SENDING_ADDRESS_VIEW, editSendingAddressView);
   
        // Open wallet
        View openWalletView = new SimpleView(localiser, "Open Wallet", inputStream, printStream);
        Collection<Action> openWalletActions = new ArrayList<Action>();
        openWalletActions.add(new CancelBackToParentAction(controller, localiser));
        openWalletActions.add(new OkBackToParentAction(controller, localiser));
        openWalletView.setPossibleActions(openWalletActions);
        viewMap.put(View.OPEN_WALLET_VIEW, openWalletView);

        View saveWalletAsView = new SimpleView(localiser, "Save Wallet As", inputStream, printStream);
        Collection<Action> saveWalletAsActions = new ArrayList<Action>();
        saveWalletAsActions.add(new CancelBackToParentAction(controller, localiser));
        saveWalletAsActions.add(new OkBackToParentAction(controller, localiser));
        saveWalletAsView.setPossibleActions(saveWalletAsActions);
        viewMap.put(View.SAVE_WALLET_AS_VIEW, saveWalletAsView);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        View currentView = viewMap.get(currentViewId);

        if (currentView != null) {
            currentView.displayMessage(messageKey, messageData, titleKey);
        } else {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX
                    + "No view to output a message to with id " + currentViewId);
        }

    }

    public void displayView(int viewId) {
        currentViewId = viewId;

        View currentView = viewMap.get(viewId);

        if (currentView != null) {
            currentView.displayView();
        } else {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX
                    + "No view to display with id " + viewId);
        }
    }

    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextViewId) {
        printStream.print("\n");
        assert viewToNavigateAwayFrom == currentViewId : "Different current views";
        View currentView = viewMap.get(currentViewId);

        currentViewId = View.UNKNOWN_VIEW;

        if (currentView != null) {
            currentView.navigateAwayFromView(nextViewId);
        } else {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX
                    + "No view to navigate away from.   Next view id is " + nextViewId);
        }
    }
    
    /**
     * no need to recreate views as they are not cached
     */
    public void recreateAllViews(){
        // nothing to do
    }
}
