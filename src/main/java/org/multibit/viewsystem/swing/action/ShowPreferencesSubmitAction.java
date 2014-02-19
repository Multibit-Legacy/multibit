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


import org.joda.money.CurrencyUnit;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.CoreModel;
import org.multibit.model.exchange.ExchangeData;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.viewsystem.dataproviders.PreferencesDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

import javax.swing.*;
import javax.swing.UIManager.LookAndFeelInfo;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.Timer;

/**
 * This {@link Action} applies changes to the preferences panel.
 */
public class ShowPreferencesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    private final ExchangeController exchangeController;
    
    private PreferencesDataProvider dataProvider;
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public ShowPreferencesSubmitAction(BitcoinController bitcoinController, ExchangeController exchangeController, PreferencesDataProvider dataProvider, Icon icon,
            MultiBitFrame mainFrame) {
        super(bitcoinController.getLocaliser().getString("showPreferencesSubmitAction.text"), icon);
        
        this.bitcoinController = bitcoinController;
        this.exchangeController = exchangeController;
        this.controller = this.bitcoinController;
        
        this.dataProvider = dataProvider;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
    }

    /**
     * Change preferences.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
//        boolean feeValidationError = false;

        boolean wantToFireDataStructureChanged = false;

        try {
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            }

//            String updateStatusText = "";

            if (dataProvider != null) {
                controller.getModel().setUserPreference(CoreModel.PREVIOUS_UNDO_CHANGES_TEXT,
                        dataProvider.getPreviousUndoChangesText());

//                String previousSendFee = dataProvider.getPreviousSendFee();
//                String newSendFee = dataProvider.getNewSendFee();
//                controller.getModel().setUserPreference(BitcoinModel.PREVIOUS_SEND_FEE, previousSendFee);
//
//                // Check fee is set.
//                if (newSendFee == null || "".equals(newSendFee)) {
//                    // Fee must be set validation error.
//                    controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
//                    feeValidationError = true;
//                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.aFeeMustBeSet");
//                }
//
//                if (!feeValidationError) {
//                    if (newSendFee.startsWith(ShowPreferencesPanel.UNPARSEABLE_FEE)) {
//                        String newSendFeeTrimmed = newSendFee.substring(ShowPreferencesPanel.UNPARSEABLE_FEE.length() + 1);
//                        // Recycle the old fee and set status message.
//                        controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
//                        feeValidationError = true;
//                        updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
//                                new Object[] { newSendFeeTrimmed });
//                    }
//                }
//                
//                if (!feeValidationError) {
//                    try {
//                        // Check fee is a number.
//                        BigInteger feeAsBigInteger = Utils.toNanoCoins(newSendFee);
//
//                        // Check fee is at least the minimum fee.
//                        if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MINIMUM_FEE) < 0) {
//                            feeValidationError = true;
//                            updateStatusText = controller.getLocaliser().getString(
//                                    "showPreferencesPanel.feeCannotBeSmallerThanMinimumFee");
//                        } else {
//                            if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MAXIMUM_FEE) >= 0) {
//                                feeValidationError = true;
//                                updateStatusText = controller.getLocaliser().getString(
//                                        "showPreferencesPanel.feeCannotBeGreaterThanMaximumFee");
//                            } else {
//                                // Fee is ok.
//                                controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, newSendFee);
//                            }
//                        }
//                    } catch (NumberFormatException nfe) {
//                        // Recycle the old fee and set status message.
//                        controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
//                        feeValidationError = true;
//                        updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
//                                new Object[] { newSendFee });
//                    } catch (ArithmeticException ae) {
//                        // Recycle the old fee and set status message.
//                        controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
//                        feeValidationError = true;
//                        updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
//                                new Object[] { newSendFee });
//                    }
//                }
            }

            String previousLanguageCode = dataProvider.getPreviousUserLanguageCode();
            String newLanguageCode = dataProvider.getNewUserLanguageCode();
            controller.getModel().setUserPreference(CoreModel.PREVIOUS_USER_LANGUAGE_CODE, previousLanguageCode);

            if (previousLanguageCode != null && !previousLanguageCode.equals(newLanguageCode)) {
                // New language to set on model.
                controller.getModel().setUserPreference(CoreModel.USER_LANGUAGE_CODE, newLanguageCode);
                wantToFireDataStructureChanged = true;
            }

            // Open URI - use dialog.
            String openUriDialog = dataProvider.getOpenUriDialog();
            if (openUriDialog != null) {
                controller.getModel().setUserPreference(BitcoinModel.OPEN_URI_SHOW_DIALOG, openUriDialog);
            }

            // Open URI - use URI.
            String openUriUseUri = dataProvider.getOpenUriUseUri();
            if (openUriUseUri != null) {
                controller.getModel().setUserPreference(BitcoinModel.OPEN_URI_USE_URI, openUriUseUri);
            }

            // Font data.
            boolean fontHasChanged = false;
            String previousFontName = dataProvider.getPreviousFontName();
            String newFontName = dataProvider.getNewFontName();

            controller.getModel().setUserPreference(CoreModel.PREVIOUS_FONT_NAME, previousFontName);

            if (newFontName != null) {
                controller.getModel().setUserPreference(CoreModel.FONT_NAME, newFontName);

                if (!newFontName.equals(previousFontName)) {
                    fontHasChanged = true;
                }
            }

            String previousFontStyle = dataProvider.getPreviousFontStyle();
            String newFontStyle = dataProvider.getNewFontStyle();

            controller.getModel().setUserPreference(CoreModel.PREVIOUS_FONT_STYLE, previousFontStyle);

            if (newFontStyle != null) {
                controller.getModel().setUserPreference(CoreModel.FONT_STYLE, newFontStyle);

                if (!newFontStyle.equals(previousFontStyle)) {
                    fontHasChanged = true;
                }
            }

            String previousFontSize = dataProvider.getPreviousFontSize();
            String newFontSize = dataProvider.getNewFontSize();

            controller.getModel().setUserPreference(CoreModel.PREVIOUS_FONT_SIZE, previousFontSize);

            if (newFontSize != null) {
                controller.getModel().setUserPreference(CoreModel.FONT_SIZE, newFontSize);

                if (!newFontSize.equals(previousFontSize)) {
                    fontHasChanged = true;
                }
            }

            // Look and feel.
            boolean lookAndFeelHasChanged = false;
            String previousLookAndFeel = dataProvider.getPreviousLookAndFeel();
            String newLookAndFeel = dataProvider.getNewLookAndFeel();

            controller.getModel().setUserPreference(CoreModel.LOOK_AND_FEEL, previousLookAndFeel);

            if (newLookAndFeel != null
                    && (!newLookAndFeel.equals(previousLookAndFeel) && !newLookAndFeel.equals(UIManager.getLookAndFeel().getName()))) {
                controller.getModel().setUserPreference(CoreModel.LOOK_AND_FEEL, newLookAndFeel);

                lookAndFeelHasChanged = true;
            }

            //Minimze to tray
            boolean newMinimizeToTray = dataProvider.getNewMinimizeToTray();
            controller.getModel().setUserPreference(CoreModel.MINIMIZE_TO_TRAY, Boolean.valueOf(newMinimizeToTray).toString());
            mainFrame.allowMinimizeToTray = Boolean.valueOf(newMinimizeToTray);

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
                if (showBitcoinConvertedToFiat) {
                    restartTickerTimer = true;
                }
            } else if (dataProvider.getPreviousShowTicker() != showTicker || showTicker != dataProvider.isTickerVisible()) {
                wantToFireDataStructureChanged = true;
                if (showTicker) {
                    restartTickerTimer = true;
                }
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

            controller.getModel().setUserPreference(ExchangeModel.TICKER_SHOW, Boolean.valueOf(showTicker).toString());
            controller.getModel().setUserPreference(ExchangeModel.SHOW_BITCOIN_CONVERTED_TO_FIAT,
              Boolean.valueOf(showBitcoinConvertedToFiat).toString());

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
                // A user could just switch all the columns off in the settings
                // so
                // put a 'none' in the list of columns
                // this is to stop the default columns appearing.
                columnsToShow = TickerTableModel.TICKER_COLUMN_NONE;
            }
            controller.getModel().setUserPreference(ExchangeModel.TICKER_COLUMNS_TO_SHOW, columnsToShow);

            String previousExchange1 = dataProvider.getPreviousExchange1();
            String newExchange1 = dataProvider.getNewExchange1();
            if (newExchange1 != null && !newExchange1.equals(previousExchange1)) {
                controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE, newExchange1);
                ExchangeData newExchangeData = new ExchangeData();
                newExchangeData.setShortExchangeName(newExchange1);
                this.exchangeController.getModel().getShortExchangeNameToExchangeMap().put(newExchange1, newExchangeData);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousCurrency1 = dataProvider.getPreviousCurrency1();
            String newCurrency1 = dataProvider.getNewCurrency1();
            if (newCurrency1 != null && !newCurrency1.equals(previousCurrency1)) {
                controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY, newCurrency1);
                String newCurrencyCode = newCurrency1;
                if (ExchangeData.BITCOIN_CHARTS_EXCHANGE_NAME.equals(newExchange1)) {
                    // Use only the last three characters - the currency code.
                     if (newCurrency1.length() >= 3) {
                        newCurrencyCode = newCurrency1.substring(newCurrency1.length() - 3);
                    }
                }
                try {
                    CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(newCurrencyCode));
                } catch ( org.joda.money.IllegalCurrencyException e) {
                    e.printStackTrace();
                }
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousShowSecondRow = Boolean.valueOf(dataProvider.getPreviousShowSecondRow()).toString();
            String newShowSecondRow = Boolean.valueOf(dataProvider.getNewShowSecondRow()).toString();
            if (newShowSecondRow != null && !newShowSecondRow.equals(previousShowSecondRow)) {
                // New show second row is set on model.
                controller.getModel().setUserPreference(ExchangeModel.TICKER_SHOW_SECOND_ROW, newShowSecondRow);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousExchange2 = dataProvider.getPreviousExchange2();
            String newExchange2 = dataProvider.getNewExchange2();
            if (newExchange2 != null && !newExchange2.equals(previousExchange2)) {
                controller.getModel().setUserPreference(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE, newExchange2);
                ExchangeData newExchangeData = new ExchangeData();
                newExchangeData.setShortExchangeName(newExchange2);
                this.exchangeController.getModel().getShortExchangeNameToExchangeMap().put(newExchange2, newExchangeData);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousCurrency2 = dataProvider.getPreviousCurrency2();
            String newCurrency2 = dataProvider.getNewCurrency2();
            if (newCurrency2 != null && !newCurrency2.equals(previousCurrency2)) {
                controller.getModel().setUserPreference(ExchangeModel.TICKER_SECOND_ROW_CURRENCY, newCurrency2);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }
            
            String previousOerApicode = dataProvider.getPreviousOpenExchangeRatesApiCode();
            String newOerApiCode = dataProvider.getNewOpenExchangeRatesApiCode();
            if (newOerApiCode != null && !newOerApiCode.equals(previousOerApicode)) {
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;

                controller.getModel().setUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE, newOerApiCode);
            }

            // Can undo.
            controller.getModel().setUserPreference(CoreModel.CAN_UNDO_PREFERENCES_CHANGES, "true");

            if (restartTickerTimer) {
                // Reinitialise the currency converter.
                CurrencyConverter.INSTANCE.initialise(controller);

                // Cancel any existing timer.
                if (mainFrame.getTickerTimer1() != null) {
                    mainFrame.getTickerTimer1().cancel();
                }
                if (mainFrame.getTickerTimer2() != null) {
                    mainFrame.getTickerTimer2().cancel();
                }                // Start ticker timer.
                Timer tickerTimer1 = new Timer();
                mainFrame.setTickerTimer1(tickerTimer1);
                
                TickerTimerTask tickerTimerTask1 = new TickerTimerTask(this.exchangeController, mainFrame, true);
                tickerTimerTask1.createExchangeObjects(controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE));
                mainFrame.setTickerTimerTask1(tickerTimerTask1);

                tickerTimer1.schedule(tickerTimerTask1, 0, TickerTimerTask.DEFAULT_REPEAT_RATE);
                
                boolean showSecondRow = Boolean.TRUE.toString().equals(
                        controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW_SECOND_ROW));
                
                if (showSecondRow) {
                    Timer tickerTimer2 = new Timer();
                    mainFrame.setTickerTimer2(tickerTimer2);

                    TickerTimerTask tickerTimerTask2 = new TickerTimerTask(this.exchangeController, mainFrame, false);
                    tickerTimerTask2.createExchangeObjects(controller.getModel().getUserPreference(
                            ExchangeModel.TICKER_SECOND_ROW_EXCHANGE));
                    mainFrame.setTickerTimerTask2(tickerTimerTask2);

                    tickerTimer2.schedule(tickerTimerTask2, TickerTimerTask.TASK_SEPARATION, TickerTimerTask.DEFAULT_REPEAT_RATE);
                }
            }

            if (fontHasChanged) {
                wantToFireDataStructureChanged = true;
            }

            if (lookAndFeelHasChanged) {
                try {
                    if (CoreModel.SYSTEM_LOOK_AND_FEEL.equals(newLookAndFeel)) {
                        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    } else {
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
            }
            
            Font newFont = dataProvider.getSelectedFont();
            if (newFont != null) {
                UIManager.put("ToolTip.font", newFont);
            }

            if (wantToFireDataStructureChanged || lookAndFeelHasChanged) {
                ColorAndFontConstants.init();
                FontSizer.INSTANCE.initialise(controller);
                HelpContentsPanel.clearBrowser();

                // Switch off blinks.
                this.bitcoinController.getModel().setBlinkEnabled(false);

                try {
                    controller.fireDataStructureChanged();
                    SwingUtilities.updateComponentTreeUI(mainFrame);
                } finally {
                    // Switch blinks back on.
                    this.bitcoinController.getModel().setBlinkEnabled(true);
                }            
            }

//            if (feeValidationError) {
//                MessageManager.INSTANCE.addMessage(new Message(updateStatusText, false));
//            } else {
//                // Clear any previous validation error.
//                MessageManager.INSTANCE.addMessage(new Message(" "));
//            }
        } finally {
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.getDefaultCursor());
            }
        }
    }
}