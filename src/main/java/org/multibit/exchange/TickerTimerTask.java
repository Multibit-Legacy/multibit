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

import com.xeiam.xchange.Exchange;
import com.xeiam.xchange.ExchangeFactory;
import com.xeiam.xchange.ExchangeSpecification;
import com.xeiam.xchange.currency.Currencies;
import com.xeiam.xchange.currency.CurrencyPair;
import com.xeiam.xchange.dto.marketdata.Ticker;
import com.xeiam.xchange.service.polling.PollingMarketDataService;
import org.joda.money.BigMoney;
import org.joda.money.CurrencyUnit;
import org.multibit.controller.Controller;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.model.exchange.ExchangeData;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.List;
import java.util.TimerTask;

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
    private PollingMarketDataService marketDataService;
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

            if (marketDataService != null) {
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

                        // Is the currency pair the other way round ie
                        // base currency = other, counter currency = BTC
                        boolean reverseRates = ExchangeData.doesExchangeUseReverseRates(shortExchangeName);
                        CurrencyPair currencyPairToUse = null;

                        for (CurrencyPair loopSymbolPair : exchangeSymbols) {
                            if (ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equals(shortExchangeName)) {
                                if (loopSymbolPair.baseCurrency.equals(currency)) {
                                    getItFromTheServer = true;
                                    invertedRates = true;
                                    currencyPairToUse = loopSymbolPair;
                                    break;
                                }
                            } else {
                                if ("BTC".equals(loopSymbolPair.baseCurrency) && loopSymbolPair.counterCurrency.equals(currency)) {
                                    getItFromTheServer = true;
                                    currencyPairToUse = loopSymbolPair;
                                    break;
                                }
                                if ("BTC".equals(loopSymbolPair.counterCurrency) && loopSymbolPair.baseCurrency.equals(currency)) {
                                    getItFromTheServer = true;
                                    invertedRates = true;
                                    currencyPairToUse = loopSymbolPair;
                                    break;
                                }
                            }
                        }

                        if (getItFromTheServer) {
                            BigMoney last = null;
                            BigMoney bid = null;
                            BigMoney ask = null;

                            Ticker loopTicker;

                            if (ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equals(shortExchangeName)) {
                                log.debug("Getting loopTicker for " + currency + " USD");
                                loopTicker = marketDataService.getTicker(currency, "USD");
                                System.out.println("loopTicker = " + loopTicker);
                                Ticker btcUsdTicker = null;
                                log.debug("Getting btcUsdTicker for BTC/USD");
                                btcUsdTicker = marketDataService.getTicker(Currencies.BTC, Currencies.USD);
                                System.out.println("btcUsdTicker = " + btcUsdTicker);

                                BigMoney usdBtcRateMoney = btcUsdTicker.getLast();
                                BigDecimal usdBtcRate = null;
                                if (usdBtcRateMoney != null) {
                                    usdBtcRate = usdBtcRateMoney.getAmount();
                                    if (loopTicker.getLast() != null) {
                                        last = loopTicker.getLast().dividedBy(usdBtcRate, RoundingMode.HALF_EVEN);
                                    }
                                    if (loopTicker.getBid() != null) {
                                        bid = loopTicker.getBid().dividedBy(usdBtcRate, RoundingMode.HALF_EVEN);
                                    }
                                    if (loopTicker.getAsk() != null) {
                                        ask = loopTicker.getAsk().dividedBy(usdBtcRate, RoundingMode.HALF_EVEN);
                                    }
                                }
                            } else {
                                log.debug("Getting ticker for " + currencyPairToUse.baseCurrency + " "
                                        + currencyPairToUse.counterCurrency);
                                loopTicker = marketDataService.getTicker(currencyPairToUse.baseCurrency,
                                        currencyPairToUse.counterCurrency);

                                log.debug("Got ticker for " + currencyPairToUse.baseCurrency + " "
                                        + currencyPairToUse.counterCurrency);
                                last = loopTicker.getLast();
                                bid = loopTicker.getBid();
                                ask = loopTicker.getAsk();

                                if (invertedRates && !reverseRates) {
                                    if (last != null && last.getAmount() != BigDecimal.ZERO) {
                                        last = BigMoney.of(last.getCurrencyUnit(), BigDecimal.ONE.divide(last.getAmount(),
                                                NUMBER_OF_SIGNIFICANT_DIGITS, BigDecimal.ROUND_HALF_EVEN));
                                    } else {
                                        last = null;
                                    }
                                    if (bid != null && bid.getAmount() != BigDecimal.ZERO) {
                                        bid = BigMoney.of(last.getCurrencyUnit(), BigDecimal.ONE.divide(bid.getAmount(),
                                                NUMBER_OF_SIGNIFICANT_DIGITS, BigDecimal.ROUND_HALF_EVEN));
                                    } else {
                                        bid = null;
                                    }

                                    if (ask != null && ask.getAmount() != BigDecimal.ZERO) {
                                        ask = BigMoney.of(last.getCurrencyUnit(), BigDecimal.ONE.divide(ask.getAmount(),
                                                NUMBER_OF_SIGNIFICANT_DIGITS, BigDecimal.ROUND_HALF_EVEN));
                                    } else {
                                        ask = null;
                                    }
                                }

                                if (invertedRates) {
                                    if (reverseRates) {
                                        // USD/ BTC, reciprocal rate
                                        currency = currencyPairToUse.baseCurrency;
                                    } else {
                                        // BTC/ USD, reciprocal rate
                                        currency = currencyPairToUse.counterCurrency;
                                    }
                                } else {
                                    if (reverseRates) {
                                        // USD/ BTC, normal rate
                                        currency = currencyPairToUse.baseCurrency;
                                    } else {
                                        // BTC/ USD, normal rate
                                        currency = currencyPairToUse.counterCurrency;
                                    }
                                }
                            }

                            this.exchangeController.getModel().getExchangeData(shortExchangeName).setLastPrice(currency, last);
                            this.exchangeController.getModel().getExchangeData(shortExchangeName).setLastBid(currency, bid);
                            this.exchangeController.getModel().getExchangeData(shortExchangeName).setLastAsk(currency, ask);
                            log.debug("Exchange = " + shortExchangeName);

                            // Put the exchange rate into the currency converter.
                            if (isFirstExchange) {
                                String newCurrencyCode = currency;
                                if (ExchangeData.BITCOIN_CHARTS_EXCHANGE_NAME.equals(shortExchangeName)) {
                                    // Use only the last three characters - the
                                    // currency code.
                                    if (currency.length() >= 3) {
                                        newCurrencyCode = currency.substring(currency.length() - 3);
                                    }
                                }
                                CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(newCurrencyCode));
                                CurrencyConverter.INSTANCE.setRate(last.getAmount());
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

        if (exchange != null) {
            // Interested in the public market data feed (no authentication).
            marketDataService = exchange.getPollingMarketDataService();
            log.debug("marketDataService = " + marketDataService);

            // Get the list of available currencies.
            exchangeSymbols = marketDataService.getExchangeSymbols();
            log.debug("exchangeSymbols = " + exchangeSymbols);

            if (exchangeSymbols != null) {
                Collection<String> availableCurrencies = new java.util.TreeSet<String>();

                for (int i = 0; i < exchangeSymbols.size(); i++) {
                    String baseCurrency = exchangeSymbols.get(i).baseCurrency;
                    String counterCurrency = exchangeSymbols.get(i).counterCurrency;

                    if (ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equals(newExchangeName)) {
                        if ("USD".equalsIgnoreCase(baseCurrency) && !"BTC".equalsIgnoreCase(counterCurrency)) {
                            if (!"EEK".equalsIgnoreCase(counterCurrency) && !"CLF".equalsIgnoreCase(counterCurrency) 
                             && !"JEP".equalsIgnoreCase(counterCurrency) && ! "SVC".equalsIgnoreCase(counterCurrency)) {
                                availableCurrencies.add(counterCurrency);
                            }
                        }
                        if ("USD".equalsIgnoreCase(counterCurrency) && !"BTC".equalsIgnoreCase(baseCurrency)) {
                            if (!"EEK".equalsIgnoreCase(baseCurrency) && !"CLF".equalsIgnoreCase(baseCurrency) 
                                    && !"JEP".equalsIgnoreCase(baseCurrency) && ! "SVC".equalsIgnoreCase(baseCurrency)) {
                                availableCurrencies.add(baseCurrency);
                            }
                        }
                    } else {
                        if ("BTC".equalsIgnoreCase(baseCurrency)) {
                            availableCurrencies.add(counterCurrency);
                        }
                        if ("BTC".equalsIgnoreCase(counterCurrency)) {
                            availableCurrencies.add(baseCurrency);
                        }
                    }
                }
                ExchangeData.setAvailableCurrenciesForExchange(newExchangeName, availableCurrencies);
            }
        }
    }

    /**
     * Create the exchange specified by the exchange short name
     * 
     * @param exchangeShortname The name of the exchange to create
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
            if (ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeShortname)) {
                ExchangeSpecification exchangeSpecification = new ExchangeSpecification(exchangeClassname);
                exchangeSpecification.setPlainTextUri("http://openexchangerates.org");
                exchangeSpecification
                        .setApiKey(controller.getModel().getUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE));
                exchangeToReturn = ExchangeFactory.INSTANCE.createExchange(exchangeSpecification);
            } else {
                exchangeToReturn = ExchangeFactory.INSTANCE.createExchange(exchangeClassname);
            }
            
            if (this.exchangeController.getModel().getExchangeData(shortExchangeName) == null) {
                ExchangeData exchangeData = new ExchangeData();
                exchangeData.setShortExchangeName(shortExchangeName);
                this.exchangeController.getModel().getShortExchangeNameToExchangeMap().put(exchangeShortname, exchangeData);
            }

            return exchangeToReturn;
        } catch (com.xeiam.xchange.ExchangeException e) {
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
