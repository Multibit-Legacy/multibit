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
package org.multibit.viewsystem;

import javax.swing.Icon;


/**
 * an interface describing the views that are being presented in this MVC
 * 
 * views are used to track the view being presented to the user
 * @author jim
 *
 */
public interface View {
    public static final int SAME_VIEW = -1;                 // Not a real view - used to forward to the same view as calling
    public static final int UNKNOWN_VIEW = 0;
    public static final int TRANSACTIONS_VIEW = 1;
    public static final int SEND_BITCOIN_VIEW = 2;
    public static final int SEND_BITCOIN_CONFIRM_VIEW = 3;  // obsolete - now done with Swing dialog
    public static final int RECEIVE_BITCOIN_VIEW = 4;
    public static final int HELP_CONTENTS_VIEW = 5;
    public static final int HELP_ABOUT_VIEW = 6;
    public static final int PREFERENCES_VIEW = 7; 
    public static final int OPEN_WALLET_VIEW = 8;           // obsolete - now done with Swing dialog
    public static final int SAVE_WALLET_AS_VIEW = 9;        // obsolete - now done with Swing dialog
    public static final int VALIDATION_ERROR_VIEW = 10;     // obsolete - now done with Swing dialog
    public static final int YOUR_WALLETS_VIEW = 11;
    public static final int CREATE_BULK_ADDRESSES_VIEW = 12;
    public static final int RESET_TRANSACTIONS_VIEW = 13;
    public static final int SHOW_OPEN_URI_DIALOG_VIEW = 14;
    public static final int SHOW_IMPORT_PRIVATE_KEYS_VIEW = 15;
    public static final int SHOW_EXPORT_PRIVATE_KEYS_VIEW = 16;
    
    public static final int DEFAULT_VIEW = TRANSACTIONS_VIEW;
   
    /**
     * display the view
     */
    public void displayView();
    
    /**
     * Navigate away from the view (including releasing any resources used)
     */
    public void navigateAwayFromView();  
    
    /**
     * @returns the icon for the view
     */
    public Icon getViewIcon();
    
    /**
     * @returns the title for the view
     */
    public String getViewTitle();
    
    /**
     * @returns the view identifier for the view
     */
    public int getViewId();
}
