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


import java.util.EnumMap;
import java.util.Map;

import org.multibit.controller.MultiBitController;

import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.preferences.PreferencesViewFactory;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.MessagesPanel;
import org.multibit.viewsystem.swing.view.panels.ResetTransactionsPanel;
import org.multibit.viewsystem.swing.view.panels.WelcomePanel;
import org.multibit.viewsystem.swing.view.panels.HelpAboutPanel;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;
import org.multibit.viewsystem.swing.view.panels.ExportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.panels.ReceiveBitcoinPanel;
import org.multibit.viewsystem.swing.view.dialogs.ShowOpenUriDialog;
import org.multibit.viewsystem.swing.view.panels.ImportPrivateKeysPanel;


/**
 * a factory class that lazy loads views
 * 
 * @author jim
 * 
 */
public class ViewFactory {

    private final Map<View, Viewable> viewMap;
    private final MultiBitController controller;
    private final MultiBitFrame mainFrame;
    private final PreferencesViewFactory prefViewFactory;

    public ViewFactory(MultiBitController controller, MultiBitFrame mainFrame, PreferencesViewFactory prefViewFactory) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.prefViewFactory = prefViewFactory;
        this.viewMap = new EnumMap<View, Viewable>(View.class);    
    }
    
    public void reset() {
        this.viewMap.clear();
    }

    public Viewable getView(View viewNumber) {
        Viewable viewToReturn = this.viewMap.get(viewNumber);

        if (viewToReturn == null) {
            viewToReturn = createView(viewNumber);
        }

        return viewToReturn;
    }

    public void addView(View viewNumber, Viewable view) {
        this.viewMap.put(viewNumber, view);
    }
    
    private Viewable createView(View viewNumber) {
        Viewable viewToReturn = null;

        switch (viewNumber) {

        case SAME_VIEW: {
            assert false;
            break;
        }

        case WELCOME_VIEW: {
            viewToReturn = new WelcomePanel(controller, mainFrame);
            break;
        }

        case TRANSACTIONS_VIEW: {
            viewToReturn = new ShowTransactionsPanel(controller, mainFrame);
            break;
        }

        case HELP_ABOUT_VIEW: {
            viewToReturn = new HelpAboutPanel(controller, mainFrame);
            break;
        }

        case HELP_CONTENTS_VIEW: {
            viewToReturn = new HelpContentsPanel(controller, mainFrame);
            break;
        }
               
        case RECEIVE_BITCOIN_VIEW: {
            viewToReturn = new ReceiveBitcoinPanel(mainFrame, controller);
            break;
        }
        
        case SEND_BITCOIN_VIEW: {
            viewToReturn = new SendBitcoinPanel(mainFrame, controller);
            break;
        }
        
        case PREFERENCES_VIEW: {
            viewToReturn = this.prefViewFactory.getModularView();
            break;
        }

        case RESET_TRANSACTIONS_VIEW: {
            viewToReturn = new ResetTransactionsPanel(controller, mainFrame);
            break;
        }

        case SHOW_OPEN_URI_DIALOG_VIEW: {
            viewToReturn = new ShowOpenUriDialog(controller, mainFrame);
            break;
        }

        case SHOW_IMPORT_PRIVATE_KEYS_VIEW: {
            viewToReturn = new ImportPrivateKeysPanel(controller, mainFrame);
            break;
        }

        case SHOW_EXPORT_PRIVATE_KEYS_VIEW: {
            viewToReturn = new ExportPrivateKeysPanel(controller, mainFrame);
            break;
        }

        case MESSAGES_VIEW: {
            viewToReturn = new MessagesPanel(controller, mainFrame);
            break;
        }
        
        case CHARTS_VIEW: {

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
