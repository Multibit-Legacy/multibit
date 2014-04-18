/**
 * Copyright 2011 multibit.org
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
package org.multibit.viewsystem;

import java.util.EnumSet;

/**
 * An interface describing the views that are being presented in this MVC.
 *
 * Views are used to track the view being presented to the user.
 *
 * @author jim
 *
 */
public enum View {
    SAME_VIEW, // Not a real view - used to forward to the same view as calling
    UNKNOWN_VIEW,
    TRANSACTIONS_VIEW,
    SEND_BITCOIN_VIEW,
    SEND_BITCOIN_CONFIRM_VIEW, // obsolete - now done with Swing dialog
    RECEIVE_BITCOIN_VIEW,
    HELP_CONTENTS_VIEW,
    HELP_ABOUT_VIEW,
    PREFERENCES_VIEW,
    OPEN_WALLET_VIEW, // obsolete - now done with Swing dialog
    SAVE_WALLET_AS_VIEW, // obsolete - now done with Swing dialog
    VALIDATION_ERROR_VIEW, // obsolete - now done with Swing dialog
    YOUR_WALLETS_VIEW,
    CREATE_BULK_ADDRESSES_VIEW, // obsolete
    RESET_TRANSACTIONS_VIEW,
    SHOW_OPEN_URI_DIALOG_VIEW,
    SHOW_IMPORT_PRIVATE_KEYS_VIEW,
    SHOW_EXPORT_PRIVATE_KEYS_VIEW,
    SHOW_CHECK_PRIVATE_KEYS_VIEW,
    WELCOME_VIEW,
    MESSAGES_VIEW,
    ADD_PASSWORD_VIEW,
    CHANGE_PASSWORD_VIEW,
    REMOVE_PASSWORD_VIEW,
    CHARTS_VIEW,
    VERIFY_MESSAGE_VIEW,
    SIGN_MESSAGE_VIEW;

    public static final EnumSet<View> OBSOLETE = EnumSet.of(
            SEND_BITCOIN_CONFIRM_VIEW,
            OPEN_WALLET_VIEW,
            SAVE_WALLET_AS_VIEW,
            VALIDATION_ERROR_VIEW,
            CREATE_BULK_ADDRESSES_VIEW);
    
    public static View DEFAULT_VIEW() {
        return WELCOME_VIEW;
    }
    
    public final boolean isObsolete() {
        return OBSOLETE.contains(this);
    }
    
    /**
     * Finds the view from the old view name system.
     * @param viewNumber
     * @return view, or null if no view found
     * @deprecated (this feature may be removed in a future release)
     */
    @Deprecated
    public static View parseOldView(Integer viewNumber) {
        if (null == viewNumber) {
            return null;
        }
        
        switch (viewNumber) {
            case 1  : return TRANSACTIONS_VIEW;
            case 2  : return SEND_BITCOIN_VIEW;
          //case 3  : return SEND_BITCOIN_CONFIRM_VIEW; (obsolete)
            case 4  : return RECEIVE_BITCOIN_VIEW;
            case 5  : return HELP_CONTENTS_VIEW;
            case 6  : return HELP_ABOUT_VIEW;
            case 7  : return PREFERENCES_VIEW;
          //case 8  : return OPEN_WALLET_VIEW; (obsolete)
          //case 9  : return SAVE_WALLET_AS_VIEW; (obsolete)
          //case 10 : return VALIDATION_ERROR_VIEW; (obsolete)
            case 11 : return YOUR_WALLETS_VIEW;
          //case 12 : return CREATE_BULK_ADDRESSES_VIEW;  (obsolete)
            case 13 : return RESET_TRANSACTIONS_VIEW;
            case 14 : return SHOW_OPEN_URI_DIALOG_VIEW;
            case 15 : return SHOW_IMPORT_PRIVATE_KEYS_VIEW;
            case 16 : return SHOW_EXPORT_PRIVATE_KEYS_VIEW;
            case 17 : return WELCOME_VIEW;
            case 18 : return MESSAGES_VIEW;
            case 19 : return ADD_PASSWORD_VIEW;
            case 20 : return CHANGE_PASSWORD_VIEW;
            case 21 : return REMOVE_PASSWORD_VIEW;
            case 22 : return CHARTS_VIEW;
            case 23 : return SIGN_MESSAGE_VIEW;
            case 24 : return VERIFY_MESSAGE_VIEW;
                
            case 3 : case 8 : case 9 : case 10 : case 12 :
                return TRANSACTIONS_VIEW;
                
            default : return null;
        }
    }
    
    /**
     * Finds the old view name system from the view.
     * Used for backwards compatibility.
     * @param view
     * @return viewNumber, or 0 if not found
     * @deprecated (this feature may be removed in a future release)
     */
    @Deprecated
    public static int toOldViewNumeric(View view) {
        if (null == view) {
            return 0;
        }
        
        switch (view) {
            case TRANSACTIONS_VIEW : return 1;
            case SEND_BITCOIN_VIEW : return 2;
            case RECEIVE_BITCOIN_VIEW : return 4;
            case HELP_CONTENTS_VIEW : return 5;
            case HELP_ABOUT_VIEW : return 6;
            case PREFERENCES_VIEW : return 7;
            case YOUR_WALLETS_VIEW : return 11;
            case RESET_TRANSACTIONS_VIEW : return 13;
            case SHOW_OPEN_URI_DIALOG_VIEW : return 14;
            case SHOW_IMPORT_PRIVATE_KEYS_VIEW : return 15;
            case SHOW_EXPORT_PRIVATE_KEYS_VIEW : return 16;
            case WELCOME_VIEW : return 17;
            case MESSAGES_VIEW : return 18;
            case ADD_PASSWORD_VIEW : return 19;
            case CHANGE_PASSWORD_VIEW : return 20;
            case REMOVE_PASSWORD_VIEW : return 21;
            case CHARTS_VIEW : return 22;
            case SIGN_MESSAGE_VIEW : return 23;
            case VERIFY_MESSAGE_VIEW : return 24;

            default : return 0;
        }
    }
}
