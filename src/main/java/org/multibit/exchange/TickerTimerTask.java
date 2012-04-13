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

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;
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

    public static final int DEFAULT_REPEAT_RATE = 20000; // milliseconds

    private static Logger log = LoggerFactory.getLogger(TickerTimerTask.class);

    private final MultiBitController controller;
    private final MultiBitFrame mainFrame;

    private Exchange mtGox;
    private MarketDataService marketDataService;

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
        } catch (NoClassDefFoundError e) {
            // probably xchange is not on classpath - ticker will not run
            // but error should not spread out from here to rest of MultiBit
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
    }

    /**
     * When the timer executes, this code is run.
     */
    public void run() {
        try {
            if (marketDataService != null) {
                Ticker tickerUSD = marketDataService.getTicker(SymbolPair.BTC_USD);
                Ticker tickerEUR = marketDataService.getTicker(SymbolPair.BTC_EUR);
                Ticker tickerGBP = marketDataService.getTicker(SymbolPair.BTC_GBP);

                // Get the latest ticker data showing BTC to USD
                String btcUSDRate = tickerUSD.getLast().toString();
                String btcUSDBid = tickerUSD.getBid().toString();
                String btcUSDAsk = tickerUSD.getAsk().toString();
                System.out.println("TickerTimerTask - Current exchange rate for BTC / USD: " + btcUSDRate + ", bid = " + btcUSDBid + ", ask = " + btcUSDAsk);
                controller.getModel().getExchangeData()
                        .setLastRate("USD", Double.parseDouble(tickerUSD.getLast().getAmount().toPlainString()));
                controller.getModel().getExchangeData()
                .setLastBid("USD", Double.parseDouble(tickerUSD.getBid().getAmount().toPlainString()));
                controller.getModel().getExchangeData()
                .setLastAsk("USD", Double.parseDouble(tickerUSD.getAsk().getAmount().toPlainString()));

                // Get the latest ticker data showing BTC to EUR
                String btcEURRate = tickerEUR.getLast().toString();
                String btcEURBid = tickerEUR.getBid().toString();
                String btcEURAsk = tickerEUR.getAsk().toString();
                System.out.println("TickerTimerTask - Current exchange rate for BTC / EUR: " + btcEURRate  + ", bid = " + btcEURBid + ", ask = " + btcEURAsk);

                controller.getModel().getExchangeData()
                .setLastRate("EUR", Double.parseDouble(tickerEUR.getLast().getAmount().toPlainString()));
                controller.getModel().getExchangeData()
                .setLastBid("EUR", Double.parseDouble(tickerEUR.getBid().getAmount().toPlainString()));
                controller.getModel().getExchangeData()
                .setLastAsk("EUR", Double.parseDouble(tickerEUR.getAsk().getAmount().toPlainString()));

                // Get the latest ticker data showing BTC to GBP
                String btcGBPRate = tickerGBP.getLast().toString();
                String btcGBPBid = tickerGBP.getBid().toString();
                String btcGBPAsk = tickerGBP.getAsk().toString();
                System.out.println("TickerTimerTask - Current exchange rate for BTC / GBP: " + btcGBPRate + ", bid = " + btcGBPBid + ", ask = " + btcGBPAsk);
                controller.getModel().getExchangeData()
                        .setLastRate("GBP", Double.parseDouble(tickerGBP.getLast().getAmount().toPlainString()));
                controller.getModel().getExchangeData()
                .setLastBid("GBP", Double.parseDouble(tickerGBP.getBid().getAmount().toPlainString()));
                controller.getModel().getExchangeData()
                .setLastAsk("GBP", Double.parseDouble(tickerGBP.getAsk().getAmount().toPlainString()));

                mainFrame.fireExchangeDataChanged();
            }
        } catch (Exception e) {
            // stop any xchange errors percolating out
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
    }
}