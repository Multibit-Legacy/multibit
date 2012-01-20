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
package org.multibit.viewsystem.swing.view;

import java.util.HashMap;
import java.util.Map;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ShowImportPrivateKeysAction;
import org.multibit.viewsystem.swing.view.yourwallets.YourWalletsPanel;

/**
 * a factory class that lazy loads views
 * 
 * @author jim
 * 
 */
public class ViewFactory {
    private Map<Integer, View> viewMap;

    MultiBitController controller;
    MultiBitFrame mainFrame;

    public ViewFactory(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        viewMap = new HashMap<Integer, View>();
    }

    public View getView(int viewNumber) {
        View viewToReturn = viewMap.get(viewNumber);

        if (viewToReturn == null) {
            viewToReturn = createView(viewNumber);
        }

        return viewToReturn;
    }

    public void addView(int viewNumber, View view) {
        viewMap.put(viewNumber, view);
    }
    
    private View createView(int viewNumber) {
        View viewToReturn = null;

        switch (viewNumber) {

        case View.TRANSACTIONS_VIEW: {
            viewToReturn = new ShowTransactionsPanel(mainFrame, controller);
            break;
        }

        case View.HELP_ABOUT_VIEW: {
            viewToReturn = new HelpAboutPanel(controller, mainFrame);
            break;
        }

        case View.HELP_CONTENTS_VIEW: {
            viewToReturn = new HelpContentsPanel(controller, mainFrame);
            break;
        }

        case View.OPEN_WALLET_VIEW: {
            viewToReturn = new OpenWalletView(controller, controller.getLocaliser(), mainFrame);
            break;
        }
        
        case View.SAVE_WALLET_AS_VIEW: {
            viewToReturn = new CreateNewWalletView(controller, controller.getLocaliser(), mainFrame);
            break;
        }
        
        case View.RECEIVE_BITCOIN_VIEW: {
            viewToReturn = new ReceiveBitcoinPanel(mainFrame, controller);
            break;
        }
        
        case View.SEND_BITCOIN_VIEW: {
            viewToReturn = new SendBitcoinPanel(mainFrame, controller);
            break;
        }
        
        case View.SEND_BITCOIN_CONFIRM_VIEW: {
            viewToReturn = new SendBitcoinConfirmView(controller, mainFrame);
            //viewToReturn = new ShowOpenUriView(controller, mainFrame);
            break;
        }
        
        case View.PREFERENCES_VIEW: {
            viewToReturn = new ShowPreferencesPanel(controller, mainFrame);
            break;
        }
        
        case View.VALIDATION_ERROR_VIEW: {
            viewToReturn = new ValidationErrorView(controller, mainFrame);
            break;
        }
        
        case View.YOUR_WALLETS_VIEW: {
            viewToReturn = new YourWalletsPanel(controller, mainFrame);
            break;
        }
       
        case View.CREATE_BULK_ADDRESSES_VIEW: {
            viewToReturn = new CreateBulkAddressesPanel(controller, mainFrame);
            break;
        }

        case View.RESET_TRANSACTIONS_VIEW: {
            viewToReturn = new ResetTransactionsPanel(controller, mainFrame);
            break;
        }

        case View.SHOW_OPEN_URI_DIALOG_VIEW: {
            viewToReturn = new ShowOpenUriView(controller, mainFrame);
            break;
        }

        case View.SHOW_IMPORT_PRIVATE_KEYS_VIEW: {
            viewToReturn = new ImportPrivateKeysPanel(controller, mainFrame);
            break;
        }

        case View.SHOW_EXPORT_PRIVATE_KEYS_VIEW: {
            viewToReturn = new ExportPrivateKeysPanel(controller, mainFrame);
            break;
        }

        default: {
        }
        }

        if (viewToReturn != null) {
            viewMap.put(viewNumber, viewToReturn);
        }
        return viewToReturn;
    }
}
