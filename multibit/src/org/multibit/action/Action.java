package org.multibit.action;

import org.multibit.model.DataProvider;

public interface Action {
    public static final int HOME_PAGE_ACTION = 1001;
    public static final int SEND_BITCOINS_ACTION = 1002;
    public static final int SEND_BITCOINS_CONFIRM_ACTION = 1003;
    public static final int RECEIVE_BITCOINS_ACTION = 1004;
    public static final int HELP_CONTENTS_ACTION = 1005;
    public static final int HELP_ABOUT_ACTION = 1006;
    public static final int ADDRESS_BOOK_ACTION = 1007;
    public static final int OPEN_WALLET_ACTION = 1008;
    public static final int SAVE_WALLET_AS_ACTION = 1009;;

    /**
     * execute the action using data from the data provider
     */
    public void execute(DataProvider dataProvider);
    
    /**
     * the text to display for this action's visual representation
     */
    public String getDisplayText();
}
