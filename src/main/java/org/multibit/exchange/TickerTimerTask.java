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

//                OrderBook mtgoxUSDOrderBook = marketDataService.getOrderBook(SymbolPair.BTC_USD);
//                List<Order> asksUSD = mtgoxUSDOrderBook.getAsks();
//                System.out.println("TickerTimerTask - Current asks (USD) " + asksUSD.toString());
//
//                List<Order> bidsUSD = mtgoxUSDOrderBook.getBids();
//                System.out.println("TickerTimerTask - Current bids (USD) " + bidsUSD.toString());
//
//                OrderBook mtgoxEUROrderBook = marketDataService.getOrderBook(SymbolPair.BTC_EUR);
//                List<Order> asksEUR = mtgoxEUROrderBook.getAsks();
//                System.out.println("TickerTimerTask - Current asks (EUR)" + asksEUR.toString());
//
//                List<Order> bidsEUR = mtgoxEUROrderBook.getBids();
//                System.out.println("TickerTimerTask - Current bids (EUR) " + bidsEUR.toString());

                // Get the latest ticker data showing BTC to USD
                String btcusd = tickerUSD.getLast().toString();
                System.out.println("TickerTimerTask - Current exchange rate for BTC / USD: " + btcusd);
                controller.getModel().getExchangeData()
                        .setLastTick("USD", Double.parseDouble(tickerUSD.getLast().getAmount().toPlainString()));

                // Get the latest ticker data showing BTC to EUR
                String btceur = tickerEUR.getLast().toString();
                System.out.println("TickerTimerTask - Current exchange rate for BTC / EUR: " + btceur);

                controller.getModel().getExchangeData()
                        .setLastTick("EUR", Double.parseDouble(tickerEUR.getLast().getAmount().toPlainString()));

                // Get the latest ticker data showing BTC to GBP
                String btcgbp = tickerGBP.getLast().toString();
                System.out.println("TickerTimerTask - Current exchange rate for BTC / GBP: " + btcgbp);
                controller.getModel().getExchangeData()
                        .setLastTick("GBP", Double.parseDouble(tickerGBP.getLast().getAmount().toPlainString()));

                mainFrame.fireExchangeDataChanged();
            }
        } catch (Exception e) {
            // stop any xchange errors percolating out
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
    }
}