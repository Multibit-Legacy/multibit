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
package org.multibit.viewsystem.swing.core;

import java.util.EnumMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.core.CoreController;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.viewsystem.View;
import static org.multibit.viewsystem.View.MESSAGES_VIEW;
import static org.multibit.viewsystem.View.PREFERENCES_VIEW;
import static org.multibit.viewsystem.View.TRANSACTIONS_VIEW;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.bitcoin.dialogs.ShowOpenUriDialog;
import org.multibit.viewsystem.swing.bitcoin.panels.AddPasswordPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.ChangePasswordPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.ChartsPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.ExportPrivateKeysPanel;
import org.multibit.viewsystem.swing.core.panels.HelpAboutPanel;
import org.multibit.viewsystem.swing.core.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.ImportPrivateKeysPanel;
import org.multibit.viewsystem.swing.core.panels.MessagesPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.ReceiveBitcoinPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.RemovePasswordPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.ResetTransactionsPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.SendBitcoinPanel;
import org.multibit.viewsystem.swing.preferences.PreferencesPanel;
import org.multibit.viewsystem.swing.bitcoin.panels.TransactionsPanel;
import org.multibit.viewsystem.swing.core.panels.WelcomePanel;
import org.multibit.viewsystem.swing.preferences.PreferencesAction;
import org.multibit.viewsystem.swing.preferences.PreferencesModule;
import org.multibit.viewsystem.swing.preferences.actions.BitcoinPreferencesAction;
import org.multibit.viewsystem.swing.preferences.actions.CorePreferencesAction;
import org.multibit.viewsystem.swing.preferences.actions.ExchangePreferencesAction;
import org.multibit.viewsystem.swing.preferences.modules.BitcoinPreferencesModule;
import org.multibit.viewsystem.swing.preferences.modules.CorePreferencesModule;
import org.multibit.viewsystem.swing.preferences.modules.ExchangePreferencesModule;

/**
 * a factory class that lazy loads views
 *
 * @author jim
 *
 */
public class ViewFactory {

    private Map<View, Viewable> viewMap;
    private final Controller controller;
    private final CoreController coreController;
    private final BitcoinController bitcoinController;
    private final ExchangeController exchangeController;
    private final MultiBitFrame mainFrame;

    public ViewFactory(MultiBitFrame mainFrame, CoreController coreController, BitcoinController bitcoinController, ExchangeController exchangeController) {
        this.mainFrame = mainFrame;
        this.coreController = coreController;
        this.bitcoinController = bitcoinController;
        this.exchangeController = exchangeController;
        this.controller = this.coreController;
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
                viewToReturn = new TransactionsPanel(this.mainFrame, this.bitcoinController);
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

            // ToDo: sort out this, may need to make a wrapper class.
            case PREFERENCES_VIEW: {
                CorePreferencesModule corePreferencesModule = new CorePreferencesModule(this.coreController);
                BitcoinPreferencesModule bitcoinPreferencesModule = new BitcoinPreferencesModule(this.bitcoinController);
                ExchangePreferencesModule exchangePreferencesModule = new ExchangePreferencesModule(this.exchangeController);

                Set<PreferencesModule> preferencesModules = new LinkedHashSet<PreferencesModule>();
                {
                    preferencesModules.add(corePreferencesModule);
                    preferencesModules.add(bitcoinPreferencesModule);
                    preferencesModules.add(exchangePreferencesModule);
                }

                Set<PreferencesAction> preferencesActions = new LinkedHashSet<PreferencesAction>();
                {
                    preferencesActions.add(new CorePreferencesAction(corePreferencesModule, this.coreController));
                    preferencesActions.add(new BitcoinPreferencesAction(bitcoinPreferencesModule, this.bitcoinController));
                    preferencesActions.add(new ExchangePreferencesAction(exchangePreferencesModule, this.exchangeController));
                }

                viewToReturn = new PreferencesPanel(this.mainFrame, this.controller, preferencesModules, preferencesActions);
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
