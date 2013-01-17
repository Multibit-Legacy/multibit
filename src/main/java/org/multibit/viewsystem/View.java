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
import javax.swing.Icon;

/**
 * an interface describing the views that are being presented in this MVC
 *
 * views are used to track the view being presented to the user
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
    WELCOME_VIEW,
    MESSAGES_VIEW,
    ADD_PASSWORD_VIEW,
    CHANGE_PASSWORD_VIEW,
    REMOVE_PASSWORD_VIEW,
    CHARTS_VIEW;

    public static final EnumSet<View> OBSOLETE = EnumSet.of(
            SEND_BITCOIN_CONFIRM_VIEW,
            OPEN_WALLET_VIEW,
            SAVE_WALLET_AS_VIEW,
            VALIDATION_ERROR_VIEW,
            CREATE_BULK_ADDRESSES_VIEW);
    
    
    public static View DEFAULT_VIEW() {
        return WELCOME_VIEW;
    }
    
    public final boolean isObsolete()
    {
        return OBSOLETE.contains(this);
    }
    

}
