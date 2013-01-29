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
package org.multibit.viewsystem.swing.preferences.actions;

import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Timer;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import org.joda.money.CurrencyUnit;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.dataproviders.preferences.BitcoinPreferencesDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

import com.google.bitcoin.core.Utils;
import org.multibit.viewsystem.swing.action.MnemonicUtil;

/**
 * This {@link Action} applies changes to the preferences panel.
 */
public class ShowPreferencesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private BitcoinPreferencesDataProvider dataProvider;
    private MultiBitFrame mainFrame;
    private final SubmitActionCallback callback;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public ShowPreferencesSubmitAction(MultiBitController controller, MultiBitFrame mainFrame, Icon icon, SubmitActionCallback callback) {
        super(controller.getLocaliser().getString("showPreferencesSubmitAction.text"), icon);
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.callback = callback;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
    }

    /**
     * Change preferences.
     */
    @Override
    public void actionPerformed(ActionEvent event) {


        try {
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.WAIT_CURSOR);
            }
            
            callback.fireSubmitAction(); // all the magic happens in the callback

        } finally {
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.DEFAULT_CURSOR);
            }
        }
    }
    
    
    public interface SubmitActionCallback
    {
        void fireSubmitAction();
    }
    
    
}