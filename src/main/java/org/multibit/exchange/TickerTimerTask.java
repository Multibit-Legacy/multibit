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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TimerTask;

import com.xeiam.xchange.exceptions.ExchangeException;
import com.xeiam.xchange.poloniex.PoloniexExchange;
import com.xeiam.xchange.service.polling.marketdata.PollingMarketDataService;
import org.joda.money.BigMoney;
import org.joda.money.CurrencyUnit;

import org.multibit.controller.Controller;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.model.exchange.ExchangeData;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.xeiam.xchange.Exchange;
import com.xeiam.xchange.ExchangeFactory;
import com.xeiam.xchange.currency.CurrencyPair;
import com.xeiam.xchange.dto.marketdata.Ticker;

/**
 * TimerTask to poll currency exchanges for ticker data process
 */
public class TickerTimerTask extends TimerTask {

    public static final int DEFAULT_REPEAT_RATE = 600000; // milliseconds

    public static final int INITIAL_DELAY = 0; // milliseconds
    public static final int TASK_SEPARATION = 1000; // milliseconds

    public static final int NUMBER_OF_SIGNIFICANT_DIGITS = 20;

    private static Logger log = LoggerFactory.getLogger(TickerTimerTask.class);

    private final Controller controller;
    private final ExchangeController exchangeController;
    private final MultiBitFrame mainFrame;

    // Is this the first row in the ticker (=true) or the second row (=false).
    private final boolean isFirstExchange;

    private String shortExchangeName;
    private String currency;
    private Exchange exchange;
    private PollingMarketDataService marketDataServiceBTC;
    private PollingMarketDataService marketDataServiceDOGE;
    private List<CurrencyPair> exchangeSymbols;

    /**
     * Constructs the TickerTimerTask.
     */
    public TickerTimerTask(ExchangeController exchangeController, MultiBitFrame mainFrame, boolean isFirstExchange) {
        this.exchangeController = exchangeController;
        this.controller = this.exchangeController;
        this.mainFrame = mainFrame;
        this.isFirstExchange = isFirstExchange;

        if (isFirstExchange) {
            currency = controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY);
            if (currency == null || currency.length() == 0) {
                currency = ExchangeData.DEFAULT_CURRENCY;
                controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY, currency);
            }
            shortExchangeName = controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE);
        } else {
            currency = controller.getModel().getUserPreference(ExchangeModel.TICKER_SECOND_ROW_CURRENCY);
            shortExchangeName = controller.getModel().getUserPreference(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE);
        }
    }

    /**
     * When the timer executes, get the exchange data and pass to the
     * CurrencyConverter, which notifies parties of interest.
     */
    @Override
    public void run() {
        // If this is the second row and is not showing, do not do anything.
        if (!isFirstExchange && !Boolean.TRUE.toString().equals(
                controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW_SECOND_ROW))) {
            return;
        }
        
        try {
            // Create exchange.
            synchronized (this) {
                if (exchange == null) {
                    log.debug("exchange is null ... creating exchange ... (isFirstExchange = " + isFirstExchange + ")");
                    if (shortExchangeName == null) {
                        log.debug("shortExchangeName is null, defaulting to " + ExchangeData.DEFAULT_EXCHANGE);
                        shortExchangeName = ExchangeData.DEFAULT_EXCHANGE;
                    }

                    createExchangeObjects(shortExchangeName);

                    if (exchange == null) {
                        log.debug("Cannot create exchange (isFirstExchange = " + isFirstExchange + ")");
                    }
                }
            }

            if (marketDataServiceBTC != null && marketDataServiceDOGE != null) {
                if (exchangeSymbols != null) {
                    // Only get data from server if ticker is being shown if
                    // currency conversion is switched on.
                    // (This is to minimise the load on the remote servers).
                    if (!Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW))
                            || !Boolean.FALSE.toString().equals(
                                    controller.getModel().getUserPreference(ExchangeModel.SHOW_BITCOIN_CONVERTED_TO_FIAT))) {
                        // Get symbol ticker if it is one of the
                        // currencies we are interested in.
                        // (This is to save hitting the server for every
                        // currency).
                        boolean getItFromTheServer = false;
                        // Is the amount quoted the reciprocal of number
                        // of currency units per BTC
                        boolean invertedRates = false;

                        CurrencyPair currencyPairToUse = null;

                        for (CurrencyPair loopSymbolPair : exchangeSymbols) {
                            if ("BTC".equals(loopSymbolPair.baseSymbol) && loopSymbolPair.counterSymbol.equals(currency)) {
                                getItFromTheServer = true;
                                currencyPairToUse = loopSymbolPair;
                                break;
                            }
                            if ("BTC".equals(loopSymbolPair.counterSymbol) && loopSymbolPair.baseSymbol.equals(currency)) {
                                getItFromTheServer = true;
                                invertedRates = true;
                                currencyPairToUse = loopSymbolPair;
                                break;
                            }
                        }

                        if (getItFromTheServer) {
                            BigMoney lastBTC, bidBTC, askBTC, lastDOGE, bidDOGE, askDOGE;
                            Ticker tickerBTC, tickerDOGE;

                            log.debug("Getting ticker for " + currencyPairToUse.baseSymbol + " "
                                    + currencyPairToUse.counterSymbol);
                            tickerBTC = marketDataServiceBTC.getTicker(currencyPairToUse);
                            tickerDOGE = marketDataServiceDOGE.getTicker(CurrencyPair.DOGE_BTC);

                            log.debug("Got ticker for " + currencyPairToUse.baseSymbol + " "
                                    + currencyPairToUse.counterSymbol);
                            lastBTC = BigMoney.of(CurrencyUnit.USD,tickerBTC.getLast());
                            bidBTC = BigMoney.of(CurrencyUnit.USD,tickerBTC.getBid());
                            askBTC = BigMoney.of(CurrencyUnit.USD,tickerBTC.getAsk());
                            lastDOGE = BigMoney.of(CurrencyUnit.USD,tickerDOGE.getLast());
                            bidDOGE = BigMoney.of(CurrencyUnit.USD,tickerDOGE.getBid());
                            askDOGE = BigMoney.of(CurrencyUnit.USD,tickerDOGE.getAsk());

                            if (invertedRates) {
                                if (!lastBTC.getAmount().equals(BigDecimal.ZERO)) {
                                    lastBTC = BigMoney.of(lastBTC.getCurrencyUnit(), BigDecimal.ONE.divide(lastBTC.getAmount(),
                                            NUMBER_OF_SIGNIFICANT_DIGITS, BigDecimal.ROUND_HALF_EVEN));
                                } else {
                                    lastBTC = null;
                                }
                                if (!bidBTC.getAmount().equals(BigDecimal.ZERO)) {
                                    bidBTC = BigMoney.of(bidBTC.getCurrencyUnit(), BigDecimal.ONE.divide(bidBTC.getAmount(),
                                            NUMBER_OF_SIGNIFICANT_DIGITS, BigDecimal.ROUND_HALF_EVEN));
                                } else {
                                    bidBTC = null;
                                }

                                if (!askBTC.getAmount().equals(BigDecimal.ZERO)) {
                                    askBTC = BigMoney.of(askBTC.getCurrencyUnit(), BigDecimal.ONE.divide(askBTC.getAmount(),
                                            NUMBER_OF_SIGNIFICANT_DIGITS, BigDecimal.ROUND_HALF_EVEN));
                                } else {
                                    askBTC = null;
                                }
                            }

                            if (invertedRates) {
                                // BTC/ USD, reciprocal rate
                                currency = currencyPairToUse.counterSymbol;
                            } else {
                                // BTC/ USD, normal rate
                                currency = currencyPairToUse.counterSymbol;
                            }

                            if (lastBTC != null) {
                                this.exchangeController.getModel().getExchangeData(shortExchangeName).setLastPrice(currency, lastBTC.multipliedBy(lastDOGE.getAmount()));
                            }
                            if (bidBTC != null) {
                                this.exchangeController.getModel().getExchangeData(shortExchangeName).setLastBid(currency, bidBTC.multipliedBy(bidDOGE.getAmount()));
                            }
                            if (askBTC != null) {
                                this.exchangeController.getModel().getExchangeData(shortExchangeName).setLastAsk(currency, askBTC.multipliedBy(askDOGE.getAmount()));
                            }
                            log.debug("Exchange = " + shortExchangeName);

                            // Put the exchange rate into the currency converter.
                            if (isFirstExchange && lastBTC != null) {
                                String newCurrencyCode = currency;
                                CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(newCurrencyCode));
                                CurrencyConverter.INSTANCE.setRate(lastBTC.getAmount().multiply(lastDOGE.getAmount()));
                            }
                        }
                    }
                }

                // Fire exchange rate data changed - used by rest of MultiBit.
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

    public void createExchangeObjects(String newExchangeName) {
        exchange = createExchange(newExchangeName);
        Exchange exchangeDOGE = ExchangeFactory.INSTANCE.createExchange(PoloniexExchange.class.getName());

        if (exchange != null) {

            // Interested in the public market data feed (no authentication).
            marketDataServiceBTC = exchange.getPollingMarketDataService();
            marketDataServiceDOGE = exchangeDOGE.getPollingMarketDataService();
            log.debug("marketDataServiceBTC = " + marketDataServiceBTC);

            // Get the list of available currencies.
            exchangeSymbols = exchange.getMetaData().getCurrencyPairs();
            log.debug("exchangeSymbols = " + exchangeSymbols);

            if (exchangeSymbols != null) {
                Collection<String> availableCurrencies = new java.util.TreeSet<String>();

                for (int i = 0; i < exchangeSymbols.size(); i++) {
                    String baseCurrency = exchangeSymbols.get(i).baseSymbol;
                    String counterCurrency = exchangeSymbols.get(i).counterSymbol;

                    if ("BTC".equalsIgnoreCase(baseCurrency)) {
                        availableCurrencies.add(counterCurrency);
                    }
                    if ("BTC".equalsIgnoreCase(counterCurrency)) {
                        availableCurrencies.add(baseCurrency);
                    }
                }
                ExchangeData.setAvailableCurrenciesForExchange(newExchangeName, availableCurrencies);
            }
        }
    }

    /**
     * Create the exchange specified by the exchange class name specified e.g.
     * BitcoinChartsExchange.class.getName();
     * 
     * @param exchangeShortname
     */
    private Exchange createExchange(String exchangeShortname) {
        log.debug("creating exchange from exchangeShortname  = " + exchangeShortname);
        if (exchangeShortname == null) {
            return null;
        }

        try {
            // Demonstrate the public market data service.
            // Use the factory to get the exchange API using default settings.
            String exchangeClassname = ExchangeData.convertExchangeShortNameToClassname(exchangeShortname);

            if (exchangeClassname == null) {
                return null;
            }

            Exchange exchangeToReturn;
            exchangeToReturn = ExchangeFactory.INSTANCE.createExchange(exchangeClassname);
            
            if (this.exchangeController.getModel().getExchangeData(shortExchangeName) == null) {
                ExchangeData exchangeData = new ExchangeData();
                exchangeData.setShortExchangeName(shortExchangeName);
                this.exchangeController.getModel().getShortExchangeNameToExchangeMap().put(exchangeShortname, exchangeData);
            }

            return exchangeToReturn;
        } catch (ExchangeException e) {
            // Probably xchange is not on classpath - ticker will not run
            // but error should not spread out from here to rest of MultiBit.
            log.error(e.getClass().getName() + " " + e.getMessage());
        }catch (NoClassDefFoundError e) {
            // Probably xchange is not on classpath - ticker will not run
            // but error should not spread out from here to rest of MultiBit.
            log.error(e.getClass().getName() + " " + e.getMessage());
        } catch (NullPointerException e) {
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
        return null;
    }

    /**
     * Get the exchange used by this TickerTimerTask.
     */
    public Exchange getExchange() {
        return exchange;
    }

    public boolean isFirstExchange() {
        return isFirstExchange;
    }
}
