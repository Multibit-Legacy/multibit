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
import org.multibit.viewsystem.swing.view.panels.PreferencesPanel;
import org.multibit.viewsystem.swing.view.panels.ChartsPanel;
import org.multibit.viewsystem.swing.view.panels.ImportPrivateKeysPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.MultiBitPreferencesPanel;

/**
 * a factory class that lazy loads views
 * 
 * @author jim
 * 
 */
public class MultiBitViewFactory extends AbstractViewFactory {

    MultiBitController controller;
    MultiBitFrame mainFrame;
    
    public MultiBitViewFactory(MultiBitController controller, MultiBitFrame mainFrame) {
        super();
        this.controller = controller;
        this.mainFrame = mainFrame;
    }
    
    @Override
    protected View createView(int viewNumber) {
        View viewToReturn = null;
        switch (viewNumber) {
            case MultiBitView.SAME_VIEW:
                {
                    assert false;
                    break;
                }
            case MultiBitView.WELCOME_VIEW:
                {
                    viewToReturn = new WelcomePanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.TRANSACTIONS_VIEW:
                {
                    viewToReturn = new ShowTransactionsPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.HELP_ABOUT_VIEW:
                {
                    viewToReturn = new HelpAboutPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.HELP_CONTENTS_VIEW:
                {
                    viewToReturn = new HelpContentsPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.RECEIVE_BITCOIN_VIEW:
                {
                    viewToReturn = new ReceiveBitcoinPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.SEND_BITCOIN_VIEW:
                {
                    viewToReturn = new SendBitcoinPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.PREFERENCES_VIEW:
                {
                    PreferencesPanel preferencesPanel = new PreferencesPanel(controller, mainFrame);
                    preferencesPanel.AddPreferencesPlugin(new MultiBitPreferencesPanel(controller, mainFrame, preferencesPanel));
                    viewToReturn = preferencesPanel;
                    break;
                }
            case MultiBitView.RESET_TRANSACTIONS_VIEW:
                {
                    viewToReturn = new ResetTransactionsPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.SHOW_OPEN_URI_DIALOG_VIEW:
                {
                    viewToReturn = new ShowOpenUriDialog(controller, mainFrame);
                    break;
                }
            case MultiBitView.SHOW_IMPORT_PRIVATE_KEYS_VIEW:
                {
                    viewToReturn = new ImportPrivateKeysPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.SHOW_EXPORT_PRIVATE_KEYS_VIEW:
                {
                    viewToReturn = new ExportPrivateKeysPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.MESSAGES_VIEW:
                {
                    viewToReturn = new MessagesPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.ADD_PASSWORD_VIEW:
                {
                    viewToReturn = new AddPasswordPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.CHANGE_PASSWORD_VIEW:
                {
                    viewToReturn = new ChangePasswordPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.REMOVE_PASSWORD_VIEW:
                {
                    viewToReturn = new RemovePasswordPanel(controller, mainFrame);
                    break;
                }
            case MultiBitView.CHARTS_VIEW:
                {
                    viewToReturn = new ChartsPanel(controller, mainFrame);
                    break;
                }
            default:
                {
                }
        }
        if (viewToReturn != null) {
            viewMap.put(viewNumber, viewToReturn);
        }
        return viewToReturn;
    }
    
    
    
}
