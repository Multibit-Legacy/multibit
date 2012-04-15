/**
 * Copyright 2012 multibit.org
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
package org.multibit.exchange;

import java.util.List;
import java.util.TimerTask;

import org.joda.money.BigMoney;
import org.multibit.controller.MultiBitController;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.xeiam.xchange.Exchange;
import com.xeiam.xchange.ExchangeFactory;
import com.xeiam.xchange.SymbolPair;
import com.xeiam.xchange.service.marketdata.MarketDataService;
import com.xeiam.xchange.service.marketdata.Order;
import com.xeiam.xchange.service.marketdata.OrderBook;
import com.xeiam.xchange.service.marketdata.Ticker;

/**
 * TimerTask to poll MtGox for ticker data process
 */
public class TickerTimerTask extends TimerTask {

    public static final int DEFAULT_REPEAT_RATE = 15000; // milliseconds

    private static Logger log = LoggerFactory.getLogger(TickerTimerTask.class);

    private final MultiBitController controller;
    private final MultiBitFrame mainFrame;

    private Exchange mtGox;
    private MarketDataService marketDataService;
    private List<SymbolPair> exchangeSymbols;

    /**
     * Constructs the object, sets the string to be output in function run()
     * 
     * @param str
     */
    public TickerTimerTask(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        // Demonstrate the public market data service
        // Use the factory to get the version 1 MtGox exchange API using default
        // settings
        try {
            mtGox = ExchangeFactory.INSTANCE.createExchange("com.xeiam.xchange.mtgox.v1.MtGoxExchange");

            // Interested in the public market data feed (no authentication)
            marketDataService = mtGox.getMarketDataService();

            // get the list of available currencies
            exchangeSymbols = marketDataService.getExchangeSymbols();

            if (exchangeSymbols != null) {
                String[] availableCurrencies = new String[exchangeSymbols.size()];
                for (int i = 0; i < exchangeSymbols.size(); i++) {
                    availableCurrencies[i] = exchangeSymbols.get(i).counterSymbol;
                    log.debug("Available currency " + i + " = " +  exchangeSymbols.get(i).counterSymbol);
                }
                controller.getModel().getExchangeData()
                        .setAvailableCurrenciesForExchange(ExchangeData.MT_GOX_EXCHANGE_NAME, availableCurrencies);
            }
        } catch (NoClassDefFoundError e) {
            // probably xchange is not on classpath - ticker will not run
            // but error should not spread out from here to rest of MultiBit
            log.error(e.getClass().getName() + " " + e.getMessage());
        }

        // set the list of currencies we are interested in
        String currency1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY);
        if (currency1 == null || "".equals(currency1)) {
            currency1 = TickerTableModel.DEFAULT_CURRENCY;
        }
        String currency2 = controller.getModel().getUserPreference(MultiBitModel.TICKER_SECOND_ROW_CURRENCY);
        String showSecondRow = controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW_SECOND_ROW);
         if (Boolean.TRUE.toString().equals(showSecondRow)) {
            controller.getModel().getExchangeData().setCurrenciesWeAreInterestedIn(new String[] { currency1, currency2 });
        } else {
            controller.getModel().getExchangeData().setCurrenciesWeAreInterestedIn(new String[] { currency1 });
        }

    }

    /**
     * When the timer executes, this code is run.
     */
    public void run() {
        try {
            if (marketDataService != null) {
                if (exchangeSymbols != null) {
                    for (SymbolPair loopSymbolPair : exchangeSymbols) {
                        // get symbol ticker if it is one of the currencies we
                        // are interested in
                        // (this is to save hitting the server for ever currency
                        boolean getItFromTheServer = false;
                        String[] currenciesWeAreInterestedIn = controller.getModel().getExchangeData()
                                .getCurrenciesWeAreInterestedIn();
                        if (currenciesWeAreInterestedIn != null) {
                            for (int i = 0; i < currenciesWeAreInterestedIn.length; i++) {
                                if (loopSymbolPair.counterSymbol.equals(currenciesWeAreInterestedIn[i])) {
                                    getItFromTheServer = true;
                                    break;
                                }
                            }
                            if (getItFromTheServer) {
                                Ticker loopTicker = marketDataService.getTicker(loopSymbolPair);
                                BigMoney last = loopTicker.getLast();
                                BigMoney bid = loopTicker.getBid();
                                BigMoney ask = loopTicker.getAsk();
                                System.out.println("TickerTimerTask - Current exchange rate for " + loopSymbolPair.toString()
                                        + ": " + last + ", bid = " + bid + ", ask = " + ask);
                                controller.getModel().getExchangeData().setLastPrice(loopSymbolPair.counterSymbol, last);
                                controller.getModel().getExchangeData().setLastBid(loopSymbolPair.counterSymbol, bid);
                                controller.getModel().getExchangeData().setLastAsk(loopSymbolPair.counterSymbol, ask);
                            }
                        }
                    }
                }

                mainFrame.fireExchangeDataChanged();
            }
        } catch (Exception e) {
            // stop any xchange errors percolating out
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
    }
}