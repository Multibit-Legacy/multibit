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

/**
 * an enum encapsulating the ActionForwards 
 * these are used (mainly in Actions) to point to the next logical View to display
 * @author jim
 *
 */
public enum ActionForward {
    FORWARD_TO_SAME,
    FORWARD_TO_PREVIOUS,
    FORWARD_TO_TRANSACTIONS,
    FORWARD_TO_SEND_BITCOIN,
    FORWARD_TO_SEND_BITCOIN_CONFIRM,
    FORWARD_TO_RECEIVE_BITCOIN,
    FORWARD_TO_HELP_CONTENTS,
    FORWARD_TO_HELP_ABOUT,
    FORWARD_TO_PREFERENCES,
    FORWARD_TO_OPEN_WALLET,
    FORWARD_TO_CREATE_NEW_WALLET,
    FORWARD_TO_VALIDATION_ERROR,
    FORWARD_TO_YOUR_WALLETS,
    FORWARD_TO_CREATE_BULK_ADDRESSES_VIEW,
    FORWARD_TO_RESET_TRANSACTIONS_VIEW,
    FORWARD_TO_SHOW_OPEN_URI_VIEW,
    FORWARD_TO_SHOW_IMPORT_PRIVATE_KEYS_VIEW,
    FORWARD_TO_SHOW_EXPORT_PRIVATE_KEYS_VIEW
}
