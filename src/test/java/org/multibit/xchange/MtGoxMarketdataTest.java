/**
 * Copyright (C) 2012 Xeiam LLC http://xeiam.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.multibit.xchange;

import junit.framework.TestCase;

/**
 * <p>
 * Example showing the following:
 * </p>
 * <ul>
 * <li>Connecting to Mt Gox BTC exchange</li>
 * <li>Retrieving the last tick</li>
 * <li>Retrieving the current order book</li>
 * <li>Retrieving the current full order book</li>
 * <li>Retrieving trades</li>
 * </ul>
 */
public class MtGoxMarketdataTest  extends TestCase  {

  /**
   * Test the basic access to the MtGox data works ok in the MultiBit environment
   */
  public void testBasicLookup() {
      
      // cannot get this test running yet as the mt gox exchange classes aren't loading properly

//    // Demonstrate the public market data service
//    // Use the factory to get the version 1 MtGox exchange API using default settings
//    Exchange mtGox = ExchangeFactory.INSTANCE.createExchange("com.xeiam.xchange.mtgox.v1.MtGoxExchange");
//
//    // Interested in the public market data feed (no authentication)
//    MarketDataService marketDataService = mtGox.getMarketDataService();
//
//    // Get the latest ticker data showing BTC to USD
//    Ticker ticker = marketDataService.getTicker(SymbolPair.BTC_USD);
//    String btcusd = ticker.getLast().toString();
//    System.out.println("Current exchange rate for BTC / USD: " + btcusd);
//
//    // Get the current orderbook
//    OrderBook orderBook = marketDataService.getOrderBook(SymbolPair.BTC_USD);
//    System.out.println("Current Order Book size for BTC / USD: " + orderBook.getAsks().size() + orderBook.getBids().size());
//
//    // Get the current full orderbook
//    OrderBook fullOrderBook = marketDataService.getFullOrderBook(SymbolPair.BTC_USD);
//    System.out.println("Current Full Order Book size for BTC / USD: " + fullOrderBook.getAsks().size() + fullOrderBook.getBids().size());
//
//    // Get trades
//    Trades trades = marketDataService.getTrades(new SymbolPair("BTC", "PLN"));
//    System.out.println("Current trades size for BTC / PLN: " + trades.getTrades().size());
  }
}
