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
package org.multibit.viewsystem.core;


/**
 * an interface describing the views that are being presented in this MVC
 * 
 * views are used to track the view being presented to the user
 * @author jim
 *
 */
public interface MultiBitView extends View{
    public static final int TRANSACTIONS_VIEW = 101;
    public static final int SEND_BITCOIN_VIEW = 102;
    public static final int SEND_BITCOIN_CONFIRM_VIEW = 103;  // obsolete - now done with Swing dialog
    public static final int RECEIVE_BITCOIN_VIEW = 104;
    public static final int OPEN_WALLET_VIEW = 108;           // obsolete - now done with Swing dialog
    public static final int SAVE_WALLET_AS_VIEW = 109;        // obsolete - now done with Swing dialog
    public static final int VALIDATION_ERROR_VIEW = 110;     // obsolete - now done with Swing dialog
    public static final int YOUR_WALLETS_VIEW = 111;
    public static final int CREATE_BULK_ADDRESSES_VIEW = 112;  // obsolete
    public static final int RESET_TRANSACTIONS_VIEW = 113;
    public static final int SHOW_OPEN_URI_DIALOG_VIEW = 114;
    public static final int SHOW_IMPORT_PRIVATE_KEYS_VIEW = 115;
    public static final int SHOW_EXPORT_PRIVATE_KEYS_VIEW = 116;
    public static final int MESSAGES_VIEW = 118;
    public static final int ADD_PASSWORD_VIEW = 119;
    public static final int CHANGE_PASSWORD_VIEW = 120;
    public static final int REMOVE_PASSWORD_VIEW = 121;
    public static final int CHARTS_VIEW = 122;
}
