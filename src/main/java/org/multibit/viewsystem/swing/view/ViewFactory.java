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

import org.multibit.viewsystem.swing.view.dialogs.ShowOpenUriDialog;
import org.multibit.viewsystem.swing.view.panels.RemovePasswordPanel;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.MessagesPanel;
import org.multibit.viewsystem.swing.view.panels.ResetTransactionsPanel;
import org.multibit.viewsystem.swing.view.panels.WelcomePanel;
import org.multibit.viewsystem.swing.view.panels.HelpAboutPanel;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;
import org.multibit.viewsystem.swing.view.panels.ExportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.panels.ReceiveBitcoinPanel;
import org.multibit.viewsystem.swing.view.panels.AddPasswordPanel;
import org.multibit.viewsystem.swing.view.panels.ChangePasswordPanel;
import org.multibit.viewsystem.swing.view.panels.ShowPreferencesPanel;
import org.multibit.viewsystem.swing.view.panels.ChartsPanel;
import org.multibit.viewsystem.swing.view.panels.ImportPrivateKeysPanel;
import java.util.HashMap;
import java.util.Map;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

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
        initialise();
    }
    
    public void initialise() {
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

        case View.SAME_VIEW: {
            assert false;
            break;
        }

        case View.WELCOME_VIEW: {
            viewToReturn = new WelcomePanel(controller, mainFrame);
            break;
        }

        case View.TRANSACTIONS_VIEW: {
            viewToReturn = new ShowTransactionsPanel(controller, mainFrame);
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
               
        case View.RECEIVE_BITCOIN_VIEW: {
            viewToReturn = new ReceiveBitcoinPanel(controller, mainFrame);
            break;
        }
        
        case View.SEND_BITCOIN_VIEW: {
            viewToReturn = new SendBitcoinPanel(controller, mainFrame);
            break;
        }
        
        case View.PREFERENCES_VIEW: {
            viewToReturn = new ShowPreferencesPanel(controller, mainFrame);
            break;
        }

        case View.RESET_TRANSACTIONS_VIEW: {
            viewToReturn = new ResetTransactionsPanel(controller, mainFrame);
            break;
        }

        case View.SHOW_OPEN_URI_DIALOG_VIEW: {
            viewToReturn = new ShowOpenUriDialog(controller, mainFrame);
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

        case View.MESSAGES_VIEW: {
            viewToReturn = new MessagesPanel(controller, mainFrame);
            break;
        }
        
        case View.ADD_PASSWORD_VIEW: {
            viewToReturn = new AddPasswordPanel(controller, mainFrame);
            break;
        }
        
        case View.CHANGE_PASSWORD_VIEW: {
            viewToReturn = new ChangePasswordPanel(controller, mainFrame);
            break;
        }
        
        case View.REMOVE_PASSWORD_VIEW: {
            viewToReturn = new RemovePasswordPanel(controller, mainFrame);
            break;
        }

        case View.CHARTS_VIEW: {
            viewToReturn = new ChartsPanel(controller, mainFrame);
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
