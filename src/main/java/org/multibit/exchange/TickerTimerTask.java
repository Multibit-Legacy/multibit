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
import org.joda.money.CurrencyUnit;
import org.multibit.controller.MultiBitController;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.xeiam.xchange.CurrencyPair;
import com.xeiam.xchange.Exchange;
import com.xeiam.xchange.ExchangeFactory;
import com.xeiam.xchange.dto.marketdata.Ticker;
import com.xeiam.xchange.service.marketdata.polling.PollingMarketDataService;

/**
 * TimerTask to poll MtGox for ticker data process
 */
public class TickerTimerTask extends TimerTask {

    public static final int DEFAULT_REPEAT_RATE = 600000; // milliseconds

    public static final int INITIAL_DELAY = 500; // milliseconds

    private static Logger log = LoggerFactory.getLogger(TickerTimerTask.class);

    private final MultiBitController controller;
    private final MultiBitFrame mainFrame;

    private Exchange mtGox;
    private PollingMarketDataService marketDataService;
    private List<CurrencyPair> exchangeSymbols;

    /**
     * Constructs the object, sets the string to be output in function run().
     */
    public TickerTimerTask(MultiBitController controller, MultiBitFrame mainFrame) {
    	
        this.controller = controller;
        this.mainFrame = mainFrame;

        // set the list of currencies we are interested in.
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
    @Override
    public void run() {
        try {
            // Create exchange.
            synchronized(this) {
                if (mtGox == null) {
                    log.debug("mtGox is null ... creating exchange ...");
                    createExchange();
                    log.debug("... done. mtGox exchange = " + mtGox);
                }
            }
            
            if (marketDataService != null) {
                if (exchangeSymbols != null) {
                    // Only get data from server if ticker is being shown.
                    if (!Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW))) {
                        for (CurrencyPair loopSymbolPair : exchangeSymbols) {
                            // Get symbol ticker if it is one of the currencies
                            // we are interested in.
                            // (This is to save hitting the server for every currency).
                            boolean getItFromTheServer = false;
                            String[] currenciesWeAreInterestedIn = controller.getModel().getExchangeData()
                                    .getCurrenciesWeAreInterestedIn();
                            
                            String currencyConverterCurrency = currenciesWeAreInterestedIn[0];
                            
                            if (currenciesWeAreInterestedIn != null) {
                                for (int i = 0; i < currenciesWeAreInterestedIn.length; i++) {
                                    if (loopSymbolPair.counterCurrency.equals(currenciesWeAreInterestedIn[i])) {
                                        getItFromTheServer = true;
                                        
                                        break;
                                    }
                                }
                                if (getItFromTheServer) {
                                    Ticker loopTicker = marketDataService.getTicker(loopSymbolPair.baseCurrency,
                                            loopSymbolPair.counterCurrency);
                                    BigMoney last = loopTicker.getLast();
                                    BigMoney bid = loopTicker.getBid();
                                    BigMoney ask = loopTicker.getAsk();

                                    controller.getModel().getExchangeData().setLastPrice(loopSymbolPair.counterCurrency, last);
                                    controller.getModel().getExchangeData().setLastBid(loopSymbolPair.counterCurrency, bid);
                                    controller.getModel().getExchangeData().setLastAsk(loopSymbolPair.counterCurrency, ask);
                                    
                                    if (currencyConverterCurrency.equals(loopSymbolPair.counterCurrency)) {
                                        // Put the exchange rate into the currency converter.
                                        CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(currencyConverterCurrency));
                                        CurrencyConverter.INSTANCE.setRate(last.getAmount());
                                    }
                                }
                            }
                        }
                    }
                }

                mainFrame.fireExchangeDataChanged();
            }
        } catch (Exception e) {
            // Stop any xchange errors percolating out.
            log.error(e.getClass().getName() + " " + e.getMessage());
            if (e.getCause() != null)  {
                log.error(e.getCause().getClass().getName() + " " + e.getCause().getMessage());                
            }
        }
    }
    
    public void createExchange() {
        try {
            // Demonstrate the public market data service.
            // Use the factory to get the version 1 MtGox exchange API using default
            // settings.

            mtGox = ExchangeFactory.INSTANCE.createExchange("com.xeiam.xchange.mtgox.v1.MtGoxExchange");
            log.debug("mtGox = " + mtGox);
            
            // Interested in the public market data feed (no authentication).
            marketDataService = mtGox.getPollingMarketDataService();
            log.debug("marketDataService = " + marketDataService);

            // Get the list of available currencies.
            exchangeSymbols = marketDataService.getExchangeSymbols();
            log.debug("exchangeSymbols = " + exchangeSymbols);

            if (exchangeSymbols != null) {
                String[] availableCurrencies = new String[exchangeSymbols.size()];
                for (int i = 0; i < exchangeSymbols.size(); i++) {
                    availableCurrencies[i] = exchangeSymbols.get(i).counterCurrency;
                    log.debug("Available currency " + i + " = " +  exchangeSymbols.get(i).counterCurrency);
                }
                controller.getModel().getExchangeData()
                        .setAvailableCurrenciesForExchange(ExchangeData.MT_GOX_EXCHANGE_NAME, availableCurrencies);
            }
        } catch (NoClassDefFoundError e) {
            // Probably xchange is not on classpath - ticker will not run
            // but error should not spread out from here to rest of MultiBit.
            log.error(e.getClass().getName() + " " + e.getMessage());
        } catch (NullPointerException e) {
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
    }

    public Exchange getMtGox() {
        return mtGox;
    }
}
