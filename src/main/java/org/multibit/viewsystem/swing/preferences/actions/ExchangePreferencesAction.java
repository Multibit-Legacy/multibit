/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham <da2ce7@gmail.com>.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.viewsystem.swing.preferences.actions;

import java.util.Timer;
import org.joda.money.CurrencyUnit;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.model.core.CoreModel;
import org.multibit.model.exchange.ExchangeData;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.viewsystem.dataproviders.exchange.ExchangePreferencesDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.exchange.TickerTableModel;
import org.multibit.viewsystem.swing.preferences.AbstractPreferencesAction;

/**
 *
 * @author Cameron Garnham <da2ce7@gmail.com>
 */
public class ExchangePreferencesAction extends AbstractPreferencesAction<ExchangePreferencesDataProvider,ExchangeController> {

    public ExchangePreferencesAction(ExchangePreferencesDataProvider exchangePreferencesDataProvider, ExchangeController exchangeController)
    {
        super(exchangePreferencesDataProvider, exchangeController);
    }
    
    @Override
    public Boolean Submit() {
        
        boolean wantToFireDataStructureChanged = false;
        
        if (dataProvider != null) {
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
            } else if (dataProvider.getPreviousShowTicker() != showTicker) {
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

            controller.getModel().setUserPreference(ExchangeModel.TICKER_SHOW, new Boolean(showTicker).toString());
            controller.getModel().setUserPreference(ExchangeModel.SHOW_BITCOIN_CONVERTED_TO_FIAT,
                    new Boolean(showBitcoinConvertedToFiat).toString());

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
                controller.getModel().getShortExchangeNameToExchangeMap().put(newExchange1, newExchangeData);
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

            String previousShowSecondRow = new Boolean(dataProvider.getPreviousShowSecondRow()).toString();
            String newShowSecondRow = new Boolean(dataProvider.getNewShowSecondRow()).toString();
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
                controller.getModel().getShortExchangeNameToExchangeMap().put(newExchange2, newExchangeData);
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
                if (controller.getModel().getTickerTimer1() != null) {
                    controller.getModel().getTickerTimer1().cancel();
                }
                if (controller.getModel().getTickerTimer2() != null) {
                    controller.getModel().getTickerTimer2().cancel();
                }                // Start ticker timer.
                Timer tickerTimer1 = new Timer();
                controller.getModel().setTickerTimer1(tickerTimer1);
                
                // tempory hack job.
                MultiBitFrame mainFrame = controller.getModel().getTickerTimerTask1().getMultiBitFrame();
                
                TickerTimerTask tickerTimerTask1 = new TickerTimerTask(controller, mainFrame, true);
                tickerTimerTask1.createExchangeObjects(controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE));
                controller.getModel().setTickerTimerTask1(tickerTimerTask1);

                tickerTimer1.schedule(tickerTimerTask1, 0, TickerTimerTask.DEFAULT_REPEAT_RATE);
                
                boolean showSecondRow = Boolean.TRUE.toString().equals(
                        controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW_SECOND_ROW));
                
                if (showSecondRow) {
                    Timer tickerTimer2 = new Timer();
                    controller.getModel().setTickerTimer2(tickerTimer2);

                    TickerTimerTask tickerTimerTask2 = new TickerTimerTask(controller, mainFrame, false);
                    tickerTimerTask2.createExchangeObjects(controller.getModel().getUserPreference(
                            ExchangeModel.TICKER_SECOND_ROW_EXCHANGE));
                    controller.getModel().setTickerTimerTask2(tickerTimerTask2);

                    tickerTimer2.schedule(tickerTimerTask2, TickerTimerTask.TASK_SEPARATION, TickerTimerTask.DEFAULT_REPEAT_RATE);
                }
            }
        }
        return wantToFireDataStructureChanged;
    }

    @Override
    public Boolean Undo() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
