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
package org.multibit.viewsystem.swing.view;

import java.util.EnumMap;
import java.util.Map;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.viewsystem.View;
import static org.multibit.viewsystem.View.MESSAGES_VIEW;
import static org.multibit.viewsystem.View.PREFERENCES_VIEW;
import static org.multibit.viewsystem.View.TRANSACTIONS_VIEW;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.ShowOpenUriDialog;
import org.multibit.viewsystem.swing.view.panels.AddPasswordPanel;
import org.multibit.viewsystem.swing.view.panels.ChangePasswordPanel;
import org.multibit.viewsystem.swing.view.panels.ChartsPanel;
import org.multibit.viewsystem.swing.view.panels.ExportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.panels.HelpAboutPanel;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.ImportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.panels.MessagesPanel;
import org.multibit.viewsystem.swing.view.panels.ReceiveBitcoinPanel;
import org.multibit.viewsystem.swing.view.panels.RemovePasswordPanel;
import org.multibit.viewsystem.swing.view.panels.ResetTransactionsPanel;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;
import org.multibit.viewsystem.swing.view.panels.ShowPreferencesPanel;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;
import org.multibit.viewsystem.swing.view.panels.SignMessagePanel;
import org.multibit.viewsystem.swing.view.panels.VerifyMessagePanel;
import org.multibit.viewsystem.swing.view.panels.WelcomePanel;

/**
 * a factory class that lazy loads views
 *
 * @author jim
 *
 */
public class ViewFactory {

    private Map<View, Viewable> viewMap;
    private final Controller controller;
    private final BitcoinController bitcoinController;
    private final ExchangeController exchangeController;
    private final MultiBitFrame mainFrame;

    public ViewFactory(MultiBitFrame mainFrame, BitcoinController bitcoinController, ExchangeController exchangeController) {
        this.mainFrame = mainFrame;
        this.bitcoinController = bitcoinController;
        this.exchangeController = exchangeController;
        this.controller = this.bitcoinController;
        initialise();
    }

    public final void initialise() {
        viewMap = new EnumMap<View, Viewable>(View.class);
    }

    public Viewable getView(View viewNumber) {
        Viewable viewToReturn = viewMap.get(viewNumber);

        if (viewToReturn == null) {
            viewToReturn = createView(viewNumber);
        }

        return viewToReturn;
    }

    public void addView(View viewNumber, Viewable view) {
        viewMap.put(viewNumber, view);
    }

    private Viewable createView(View viewNumber) {
        Viewable viewToReturn = null;

        switch (viewNumber) {

            case SAME_VIEW: {
                assert false;
                break;
            }

            case WELCOME_VIEW: {
                viewToReturn = new WelcomePanel(this.mainFrame, this.controller);
                break;
            }

            case HELP_ABOUT_VIEW: {
                viewToReturn = new HelpAboutPanel(this.mainFrame, this.controller);
                break;
            }

            case HELP_CONTENTS_VIEW: {
                viewToReturn = new HelpContentsPanel(this.mainFrame, this.controller);
                break;
            }

            case MESSAGES_VIEW: {
                viewToReturn = new MessagesPanel(this.mainFrame, this.controller);
                break;
            }

            case TRANSACTIONS_VIEW: {
                viewToReturn = new ShowTransactionsPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case RECEIVE_BITCOIN_VIEW: {
                viewToReturn = new ReceiveBitcoinPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case SEND_BITCOIN_VIEW: {
                viewToReturn = new SendBitcoinPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case RESET_TRANSACTIONS_VIEW: {
                viewToReturn = new ResetTransactionsPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case SHOW_OPEN_URI_DIALOG_VIEW: {
                viewToReturn = new ShowOpenUriDialog(this.mainFrame, this.bitcoinController);
                break;
            }

            case SIGN_MESSAGE_VIEW: {
                viewToReturn = new SignMessagePanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case VERIFY_MESSAGE_VIEW: {
                viewToReturn = new VerifyMessagePanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case SHOW_IMPORT_PRIVATE_KEYS_VIEW: {
                viewToReturn = new ImportPrivateKeysPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case SHOW_EXPORT_PRIVATE_KEYS_VIEW: {
                viewToReturn = new ExportPrivateKeysPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case ADD_PASSWORD_VIEW: {
                viewToReturn = new AddPasswordPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case CHANGE_PASSWORD_VIEW: {
                viewToReturn = new ChangePasswordPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case REMOVE_PASSWORD_VIEW: {
                viewToReturn = new RemovePasswordPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case CHARTS_VIEW: {
                viewToReturn = new ChartsPanel(this.mainFrame, this.bitcoinController);
                break;
            }

            case PREFERENCES_VIEW: {
                viewToReturn = new ShowPreferencesPanel(this.mainFrame, this.bitcoinController, this.exchangeController);
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
