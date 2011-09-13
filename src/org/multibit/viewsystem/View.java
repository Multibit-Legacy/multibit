package org.multibit.viewsystem;

import java.util.Collection;

import org.multibit.action.Action;

/**
 * an interface describing the views that are being presented in this MVC
 * 
 * views are used to track the view being presented to the user
 * @author jim
 *
 */
public interface View {
    public static final int UNKNOWN_VIEW = 0;
    public static final int TRANSACTIONS_VIEW = 1;
    public static final int SEND_BITCOIN_VIEW = 2;
    public static final int SEND_BITCOIN_CONFIRM_VIEW = 3;
    public static final int RECEIVE_BITCOIN_VIEW = 4;
    public static final int HELP_CONTENTS_VIEW = 5;
    public static final int HELP_ABOUT_VIEW = 6;
    public static final int PREFERENCES_VIEW = 7;
    public static final int ADDRESS_BOOK_RECEIVING_VIEW = 8;
    public static final int ADDRESS_BOOK_SENDING_VIEW = 9;
    public static final int CREATE_NEW_RECEIVING_ADDRESS_VIEW = 10;
    public static final int CREATE_NEW_SENDING_ADDRESS_VIEW = 11;
    public static final int EDIT_RECEIVING_ADDRESS_VIEW = 12;
    public static final int EDIT_SENDING_ADDRESS_VIEW = 13;
    public static final int OPEN_WALLET_VIEW = 14;
    public static final int SAVE_WALLET_AS_VIEW = 15;
    public static final int VALIDATION_ERROR_VIEW = 16;
 
    public String getDescription();
    
    public void setPossibleActions(Collection<Action> possibleActions);
    
    public void displayView();
    
    /**
     * 
     * @param nextViewId - one of the View constants
     * @param relationshipOfNewViewToPrevious - one of the ViewSystem relationship constants
     */
    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious);
    
    public void displayMessage(String messageKey, Object[] messageData, String titleKey);   
}
