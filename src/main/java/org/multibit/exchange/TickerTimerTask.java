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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
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

import com.xeiam.xchange.Exchange;
import com.xeiam.xchange.ExchangeFactory;
import com.xeiam.xchange.currency.CurrencyPair;
import com.xeiam.xchange.dto.marketdata.Ticker;
import com.xeiam.xchange.service.marketdata.polling.PollingMarketDataService;

/**
 * TimerTask to poll currency exchanges for ticker data process
 */
public class TickerTimerTask extends TimerTask {

    public static final int DEFAULT_REPEAT_RATE = 600000; // milliseconds

    public static final int INITIAL_DELAY = 500; // milliseconds

    private static Logger log = LoggerFactory.getLogger(TickerTimerTask.class);

    private final MultiBitController controller;
    private final MultiBitFrame mainFrame;

    // Exchange dealing with the first row of the ticker
    private Exchange exchange1;
    private PollingMarketDataService marketDataService1;
    private List<CurrencyPair> exchangeSymbols1;
    
    // Exchange dealing with the second row of the ticker
    private Exchange exchange2;
    private PollingMarketDataService marketDataService2;    
    private List<CurrencyPair> exchangeSymbols2;
    
    /**
     * Constructs the TickerTimerTask and initialises currencies of interest.
     */
    public TickerTimerTask(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        // Set the list of currencies we are interested in.
        String currency1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY);
        if (currency1 == null || "".equals(currency1)) {
            currency1 = TickerTableModel.DEFAULT_CURRENCY;
        }
        Collection<String> currency1Collection = new ArrayList<String>();
        currency1Collection.add(currency1);
        controller.getModel().getExchangeData1().setCurrenciesWeAreInterestedIn(currency1Collection);
        
        String currency2 = controller.getModel().getUserPreference(MultiBitModel.TICKER_SECOND_ROW_CURRENCY);
        Collection<String> currency2Collection = new ArrayList<String>();
        currency2Collection.add(currency2);
        controller.getModel().getExchangeData2().setCurrenciesWeAreInterestedIn(currency2Collection);
    }


    /**
     * When the timer executes, get the exchange data and pass to the
     * CurrencyConverter, which notifies parties of interest.
     */
    @Override
    public void run() {
        try {
            // Create exchange.
            synchronized (this) {
                if (exchange2 == null) {
                    log.debug("exchange2 is null ... creating exchange ...");
                    ExchangeData exchangeData = controller.getModel().getExchangeData2();
                    if (exchangeData != null) {
                        createExchange2(controller.getModel().getExchangeData2().getShortExchangeName());
                    } else {
                        log.debug("controller.getModel().getExchangeData2() is null");
                        return;
                    }

                    if (exchange2 == null) {
                        log.debug("Cannot create exchange2");
                    }
                }
            }

            if (exchange2 != null) {
                if (marketDataService2 != null) {
                    if (exchangeSymbols2 != null) {
                        // Only get data from server if ticker is being
                        // shown.
                        // (This is to minimise the load on the remote
                        // servers).
                        if (!Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW))) {
                            for (CurrencyPair loopSymbolPair : exchangeSymbols2) {
                                // Get symbol ticker if it is one of the
                                // currencies
                                // we are interested in.
                                // (This is to save hitting the server for
                                // every
                                // currency).
                                boolean getItFromTheServer = false;
                                Collection<String> currenciesWeAreInterestedIn = controller.getModel().getExchangeData2()
                                        .getCurrenciesWeAreInterestedIn();

                                Iterator<String> currencyIterator = currenciesWeAreInterestedIn.iterator();
                                while (currencyIterator.hasNext()) {
                                    if (loopSymbolPair.counterCurrency.equals(currencyIterator.next())) {
                                        getItFromTheServer = true;

                                        break;
                                    }
                                }
                                if (getItFromTheServer) {
                                    Ticker loopTicker = marketDataService2.getTicker(loopSymbolPair.baseCurrency,
                                            loopSymbolPair.counterCurrency);
                                    BigMoney last = loopTicker.getLast();
                                    BigMoney bid = loopTicker.getBid();
                                    BigMoney ask = loopTicker.getAsk();

                                    controller.getModel().getExchangeData2().setLastPrice(loopSymbolPair.counterCurrency, last);
                                    controller.getModel().getExchangeData2().setLastBid(loopSymbolPair.counterCurrency, bid);
                                    controller.getModel().getExchangeData2().setLastAsk(loopSymbolPair.counterCurrency, ask);
                                }
                            }
                        }
                    }
                }
            }
            mainFrame.fireExchangeDataChanged();
        } catch (Exception e) {
            // Stop any xchange errors percolating out.
            log.error(e.getClass().getName() + " " + e.getMessage());
            if (e.getCause() != null) {
                log.error(e.getCause().getClass().getName() + " " + e.getCause().getMessage());
            }
        }

        try {
            // Create exchange.
            synchronized (this) {
                if (exchange1 == null) {
                    log.debug("exchange1 is null ... creating exchange ...");
                    ExchangeData exchangeData = controller.getModel().getExchangeData1();
                    if (exchangeData != null) {
                        createExchange1(controller.getModel().getExchangeData1().getShortExchangeName());
                    } else {
                        log.debug("controller.getModel().getExchangeData1() is null");
                    }

                    if (exchange1 == null) {
                        log.debug("Cannot create exchange1");
                    }
                }
            }

            if (marketDataService1 != null) {
                if (exchangeSymbols1 != null) {
                    // Only get data from server if ticker is being shown or if
                    // currency conversion is switched on.
                    // (This is to minimise the load on the remote servers).
                    if (!Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW))
                            || !Boolean.FALSE.toString().equals(
                                    controller.getModel().getUserPreference(MultiBitModel.SHOW_BITCOIN_CONVERTED_TO_FIAT))) {
                        for (CurrencyPair loopSymbolPair : exchangeSymbols1) {
                            // Get symbol ticker if it is one of the currencies
                            // we are interested in.
                            // (This is to save hitting the server for every
                            // currency).
                            boolean getItFromTheServer = false;
                            Collection<String> currenciesWeAreInterestedIn = controller.getModel().getExchangeData1()
                                    .getCurrenciesWeAreInterestedIn();

                            Iterator<String> currencyIterator = currenciesWeAreInterestedIn.iterator();
                            String currencyConverterCurrency = currencyIterator.next();

                            currencyIterator = currenciesWeAreInterestedIn.iterator();
                            while (currencyIterator.hasNext()) {
                                if (loopSymbolPair.counterCurrency.equals(currencyIterator.next())) {
                                    getItFromTheServer = true;

                                    break;
                                }
                            }
                            if (getItFromTheServer) {
                                Ticker loopTicker = marketDataService1.getTicker(loopSymbolPair.baseCurrency,
                                        loopSymbolPair.counterCurrency);
                                BigMoney last = loopTicker.getLast();
                                BigMoney bid = loopTicker.getBid();
                                BigMoney ask = loopTicker.getAsk();

                                controller.getModel().getExchangeData1().setLastPrice(loopSymbolPair.counterCurrency, last);
                                controller.getModel().getExchangeData1().setLastBid(loopSymbolPair.counterCurrency, bid);
                                controller.getModel().getExchangeData1().setLastAsk(loopSymbolPair.counterCurrency, ask);

                                if (currencyConverterCurrency.equals(loopSymbolPair.counterCurrency)) {
                                    // Put the exchange rate into the
                                    // currency converter.
                                    CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(currencyConverterCurrency));
                                    CurrencyConverter.INSTANCE.setRate(last.getAmount());
                                }
                            }
                        }
                    }
                }

                // First currency fires exchange rate data changed - used by
                // rest of MultiBit.
                mainFrame.fireExchangeDataChanged();
            }
        } catch (Exception e) {
            // Stop any xchange errors percolating out.
            log.error(e.getClass().getName() + " " + e.getMessage());
            if (e.getCause() != null) {
                log.error(e.getCause().getClass().getName() + " " + e.getCause().getMessage());
            }
        }
    }

    public void createExchange1(String exchangeShortName) {
        exchange1 = createExchange(exchangeShortName);
        
        if (exchange1 != null) {
            // Interested in the public market data feed (no authentication).
            marketDataService1 = exchange1.getPollingMarketDataService();
            log.debug("marketDataService1 = " + marketDataService1);

            // Get the list of available currencies.
            exchangeSymbols1 = marketDataService1.getExchangeSymbols();
            log.debug("exchangeSymbols1 = " + exchangeSymbols1);

            if (exchangeSymbols1 != null) {
                Collection<String> availableCurrencies = new ArrayList<String>();
                for (int i = 0; i < exchangeSymbols1.size(); i++) {
                    String baseCurrency = exchangeSymbols1.get(i).baseCurrency;
                    String counterCurrency = exchangeSymbols1.get(i).counterCurrency;

                    log.debug("Available currency " + i + " baseCurrency = " + exchangeSymbols1.get(i).baseCurrency
                            + ", counterCurrency = " + exchangeSymbols1.get(i).counterCurrency);

                    if ("BTC".equalsIgnoreCase(baseCurrency)) {
                        availableCurrencies.add(counterCurrency);
                    }
                    if ("BTC".equalsIgnoreCase(counterCurrency)) {
                        availableCurrencies.add(baseCurrency);
                    }
                }
                ExchangeData.setAvailableCurrenciesForExchange(exchangeShortName, availableCurrencies);
            }
        }
    }

    public void createExchange2(String exchangeShortName) {
        exchange2 = createExchange(exchangeShortName);
        
        if (exchange2 != null) {
            // Interested in the public market data feed (no authentication).
            marketDataService2 = exchange2.getPollingMarketDataService();
            log.debug("marketDataService2 = " + marketDataService2);

            // Get the list of available currencies.
            exchangeSymbols2 = marketDataService2.getExchangeSymbols();
            log.debug("exchangeSymbols2 = " + exchangeSymbols2);

            if (exchangeSymbols2 != null) {
                Collection<String> availableCurrencies = new ArrayList<String>();
                for (int i = 0; i < exchangeSymbols2.size(); i++) {
                    String baseCurrency = exchangeSymbols2.get(i).baseCurrency;
                    String counterCurrency = exchangeSymbols2.get(i).counterCurrency;

                    log.debug("Available currency " + i + " baseCurrency = " + exchangeSymbols2.get(i).baseCurrency
                            + ", counterCurrency = " + exchangeSymbols2.get(i).counterCurrency);

                    if ("BTC".equalsIgnoreCase(baseCurrency)) {
                        availableCurrencies.add(counterCurrency);
                    }
                    if ("BTC".equalsIgnoreCase(counterCurrency)) {
                        availableCurrencies.add(baseCurrency);
                    }
                }
                ExchangeData.setAvailableCurrenciesForExchange(exchangeShortName, availableCurrencies);
            }
        }
    }

    /**
     * Create the exchange specified by the exchange class name specified
     * e.g. BitcoinChartsExchange.class.getName();
     * 
     * @param exchangeClassName
     */
    private Exchange createExchange(String exchangeShortname) {
        log.debug("creating exchange from exchangeShortname  = " + exchangeShortname);
        if (exchangeShortname == null) {
            return null;
        }
        
        try {
            // Demonstrate the public market data service.
            // Use the factory to get the exchange API using default
            // settings.
            String exchangeClassname = ExchangeData.convertExchangeShortNameToClassname(exchangeShortname);
            
            if (exchangeClassname == null) {
                return null;
            }
            
            Exchange exchangeToReturn = ExchangeFactory.INSTANCE.createExchange(exchangeClassname);

            log.debug("exchangeToReturn = " + exchangeToReturn);
            return exchangeToReturn;            
        } catch (NoClassDefFoundError e) {
            // Probably xchange is not on classpath - ticker will not run
            // but error should not spread out from here to rest of MultiBit.
            log.error(e.getClass().getName() + " " + e.getMessage());
        } catch (NullPointerException e) {
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
        return null;
    }

    /**
     * Get the exchange dealing with the first row of the table.
     */
    public Exchange getExchange1() {
        return exchange1;
    }
    
    /**
     * Get the exchange dealing with the first row of the table.
     */
    public Exchange getExchange2() {
        return exchange2;
    }
}
