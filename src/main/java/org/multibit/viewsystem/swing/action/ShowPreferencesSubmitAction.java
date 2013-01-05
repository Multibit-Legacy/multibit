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
package org.multibit.viewsystem.swing.action;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.math.BigInteger;
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
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.dataproviders.PreferencesDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.ShowPreferencesPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

import com.google.bitcoin.core.Utils;

/**
 * This {@link Action} applies changes to the preferences panel.
 */
public class ShowPreferencesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private PreferencesDataProvider dataProvider;
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public ShowPreferencesSubmitAction(MultiBitController controller, PreferencesDataProvider dataProvider, Icon icon, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("showPreferencesSubmitAction.text"), icon);
        this.controller = controller;
        this.dataProvider = dataProvider;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
    }

    /**
     * Change preferences.
     */
    public void actionPerformed(ActionEvent event) {
        boolean feeValidationError = false;

        boolean wantToFireDataStructureChanged = false;

        String updateStatusText = "";

        if (dataProvider != null) {
            controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT,
                    dataProvider.getPreviousUndoChangesText());

            String previousSendFee = dataProvider.getPreviousSendFee();
            String newSendFee = dataProvider.getNewSendFee();
            controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_SEND_FEE, previousSendFee);
            
            // Check fee is set.
            if (newSendFee == null || "".equals(newSendFee)) {
                // Fee must be set validation error.
                controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                feeValidationError = true;
                updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.aFeeMustBeSet");
            }

            if (!feeValidationError) {
                try {
                    // Check fee is a number.
                    BigInteger feeAsBigInteger = Utils.toNanoCoins(newSendFee);

                    // Check fee is at least the minimum fee.
                    if (feeAsBigInteger.compareTo(MultiBitModel.SEND_MINIMUM_FEE) < 0) {
                        feeValidationError = true;
                        updateStatusText = controller.getLocaliser().getString(
                                "showPreferencesPanel.feeCannotBeSmallerThanMinimumFee");
                    } else {
                        // Fee is ok.
                        controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, newSendFee);
                    }
                } catch (NumberFormatException nfe) {
                    // Recycle the old fee and set status message.
                    controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[] { newSendFee });
                } catch (ArithmeticException ae) {
                    // Recycle the old fee and set status message.
                    controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[] { newSendFee });
                }
            }
        }

        String previousLanguageCode = dataProvider.getPreviousUserLanguageCode();
        String newLanguageCode = dataProvider.getNewUserLanguageCode();
        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_USER_LANGUAGE_CODE, previousLanguageCode);

        if (previousLanguageCode != null && !previousLanguageCode.equals(newLanguageCode)) {
            // New language to set on model.
            controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE, newLanguageCode);
            wantToFireDataStructureChanged = true;
        }

        // Open URI - use dialog.
        String openUriDialog = dataProvider.getOpenUriDialog();
        if (openUriDialog != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG, openUriDialog);
        }

        // Open URI - use URI.
        String openUriUseUri = dataProvider.getOpenUriUseUri();
        if (openUriUseUri != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI, openUriUseUri);
        }

        // Font data.
        boolean fontHasChanged = false;
        String previousFontName = dataProvider.getPreviousFontName();
        String newFontName = dataProvider.getNewFontName();

        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_FONT_NAME, previousFontName);

        if (newFontName != null) {
            controller.getModel().setUserPreference(MultiBitModel.FONT_NAME, newFontName);

            if (!newFontName.equals(previousFontName)) {
                fontHasChanged = true;
            }
        }

        String previousFontStyle = dataProvider.getPreviousFontStyle();
        String newFontStyle = dataProvider.getNewFontStyle();

        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_FONT_STYLE, previousFontStyle);

        if (newFontStyle != null) {
            controller.getModel().setUserPreference(MultiBitModel.FONT_STYLE, newFontStyle);

            if (!newFontStyle.equals(previousFontStyle)) {
                fontHasChanged = true;
            }
        }

        String previousFontSize = dataProvider.getPreviousFontSize();
        String newFontSize = dataProvider.getNewFontSize();

        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_FONT_SIZE, previousFontSize);

        if (newFontSize != null) {
            controller.getModel().setUserPreference(MultiBitModel.FONT_SIZE, newFontSize);

            if (!newFontSize.equals(previousFontSize)) {
                fontHasChanged = true;
            }
        }
        
        // Look and feel.
        boolean lookAndFeelHasChanged = false;
        String previousLookAndFeel = dataProvider.getPreviousLookAndFeel();
        String newLookAndFeel = dataProvider.getNewLookAndFeel();

        controller.getModel().setUserPreference(MultiBitModel.LOOK_AND_FEEL, previousLookAndFeel);

        if (newLookAndFeel != null && (!newLookAndFeel.equals(previousLookAndFeel) || !newLookAndFeel.equals(UIManager.getLookAndFeel().getName()))) {
            controller.getModel().setUserPreference(MultiBitModel.LOOK_AND_FEEL, newLookAndFeel);

            lookAndFeelHasChanged = true;
        }


        // Currency ticker.
        boolean showTicker = dataProvider.getNewShowTicker();
        boolean showBitcoinConvertedToFiat = dataProvider.getNewShowBitcoinConvertedToFiat();
        boolean showCurrency = dataProvider.getNewShowCurrency();
        boolean showRate = dataProvider.getNewShowRate();
        boolean showBid = dataProvider.getNewShowBid();
        boolean showAsk = dataProvider.getNewShowAsk();
        boolean showExchange = dataProvider.getNewShowExchange();

        boolean restartTickerTimer = false;
        
        if (dataProvider.getPreviousShowCurrency() != showCurrency) {
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        } else if (dataProvider.getPreviousShowBitcoinConvertedToFiat() != showBitcoinConvertedToFiat) {
            wantToFireDataStructureChanged = true;
        } else if (dataProvider.getPreviousShowTicker() != showTicker || showTicker != dataProvider.isTickerVisible()) {
            wantToFireDataStructureChanged = true;
        } else if (dataProvider.getPreviousShowRate() != showRate) {
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        } else if (dataProvider.getPreviousShowBid() != showBid) {
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        } else if (dataProvider.getPreviousShowAsk() != showAsk) {
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        } else if (dataProvider.getPreviousShowExchange() != showExchange) {
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        }

        controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW, new Boolean(showTicker).toString());
        controller.getModel().setUserPreference(MultiBitModel.SHOW_BITCOIN_CONVERTED_TO_FIAT, new Boolean(showBitcoinConvertedToFiat).toString());
        
        String columnsToShow = "";
        if (showCurrency)
            columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_CURRENCY;
        if (showRate)
            columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_LAST_PRICE;
        if (showBid)
            columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_BID;
        if (showAsk)
            columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_ASK;
        if (showExchange)
            columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_EXCHANGE;

        if ("".equals(columnsToShow)) {
            // A user could just switch all the columns off in the settings so
            // put a 'none' in the list of columns
            // this is to stop the default columns appearing.
            columnsToShow = TickerTableModel.TICKER_COLUMN_NONE;
        }
        controller.getModel().setUserPreference(MultiBitModel.TICKER_COLUMNS_TO_SHOW, columnsToShow);

        String previousExchange1 = dataProvider.getPreviousExchange1();
        String newExchange1 = dataProvider.getNewExchange1();
        if (newExchange1 != null && !newExchange1.equals(previousExchange1)) {
            controller.getModel().setUserPreference(MultiBitModel.TICKER_FIRST_ROW_EXCHANGE, newExchange1);
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        }

        String previousCurrency1 = dataProvider.getPreviousCurrency1();
        String newCurrency1 = dataProvider.getNewCurrency1();
        if (newCurrency1 != null && !newCurrency1.equals(previousCurrency1)) {
            controller.getModel().setUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY, newCurrency1);
            CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(newCurrency1));
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        }

        String previousShowSecondRow = new Boolean(dataProvider.getPreviousShowSecondRow()).toString();
        String newShowSecondRow = new Boolean(dataProvider.getNewShowSecondRow()).toString();
        if (newShowSecondRow != null && !newShowSecondRow.equals(previousShowSecondRow)) {
            // New show second row is set on model.
            controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW_SECOND_ROW, newShowSecondRow);
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        }

        String previousExchange2 = dataProvider.getPreviousExchange2();
        String newExchange2 = dataProvider.getNewExchange2();
        if (newExchange2 != null && !newExchange2.equals(previousExchange2)) {
            controller.getModel().setUserPreference(MultiBitModel.TICKER_SECOND_ROW_EXCHANGE, newExchange2);
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        }

        String previousCurrency2 = dataProvider.getPreviousCurrency2();
        String newCurrency2 = dataProvider.getNewCurrency2();
        if (newCurrency2 != null && !newCurrency2.equals(previousCurrency2)) {
            controller.getModel().setUserPreference(MultiBitModel.TICKER_SECOND_ROW_CURRENCY, newCurrency2);
            wantToFireDataStructureChanged = true;
            restartTickerTimer = true;
        }

        // Set on the model the currencies we are interested in - only these get
        // downloaded to save bandwidth/ server time.
        if (dataProvider.getNewShowSecondRow()) {
            controller.getModel().getExchangeData().setCurrenciesWeAreInterestedIn(new String[] { newCurrency1, newCurrency2 });
        } else {
            controller.getModel().getExchangeData().setCurrenciesWeAreInterestedIn(new String[] { newCurrency1 });
        }

        // Can undo.
        controller.getModel().setUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES, "true");

        if (restartTickerTimer) {
            // Reinitialise the currency converter.
            CurrencyConverter.INSTANCE.initialise(controller);
            
            // Cancel any existing timer.
            if (mainFrame.getTickerTimer() != null) {
                mainFrame.getTickerTimer().cancel();
            }
            // Start ticker timer.
            Timer tickerTimer = new Timer();
            mainFrame.setTickerTimer(tickerTimer);
            tickerTimer.schedule(new TickerTimerTask(controller, mainFrame), 0, TickerTimerTask.DEFAULT_REPEAT_RATE);
        }
        
        if (fontHasChanged) {
            Font newFont = dataProvider.getSelectedFont();
            FontSizer.INSTANCE.initialise(controller);
            if (newFont != null) {
                UIManager.put("ToolTip.font", newFont);
            }

            wantToFireDataStructureChanged = true;
        }

        if (wantToFireDataStructureChanged) {
            // Redo everything.
            controller.fireDataStructureChanged();
        }
        
        boolean seaglass = false;
        if (lookAndFeelHasChanged) {
            try {
                if (ShowPreferencesPanel.SYSTEM_LOOK_AND_FEEL.equals(newLookAndFeel)) {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
//                } else if (MultiBitModel.SEA_GLASS_LOOK_AND_FEEL.equals(newLookAndFeel)) {
//                    UIManager.setLookAndFeel("com.seaglasslookandfeel.SeaGlassLookAndFeel");
//                    seaglass = true;
//                } 
                else {
                    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                        if (newLookAndFeel.equalsIgnoreCase(info.getName())) {
                            UIManager.setLookAndFeel(info.getClassName());
                            break;
                        }
                    }
                }
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            } catch (InstantiationException e) {
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (UnsupportedLookAndFeelException e) {
                e.printStackTrace();
            }

            ColorAndFontConstants.init(seaglass);
            controller.fireDataStructureChanged();
            SwingUtilities.updateComponentTreeUI(mainFrame);
        }

        // Return to the same view.
        controller.displayView(controller.getCurrentView());

        if (feeValidationError) {
            MessageManager.INSTANCE.addMessage(new Message(updateStatusText));
        } else {
            // Clear any previous validation error.
            MessageManager.INSTANCE.addMessage(new Message(" "));
        }
    }
}