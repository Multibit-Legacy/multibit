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
package org.multibit.model;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author timmolter
 * 
 */
public class ExchangeData {

    public static final String MT_GOX_EXCHANGE_NAME = "MtGox";

    public static final String DEFAULT_EXCHANGE = MT_GOX_EXCHANGE_NAME;
    
    public static final String DEFAULT_CURRENCY = "USD";
    
    public static final double DO_NOT_KNOW = -1;
    
    private Map<String, Double> currencyToRateMap;
    private Map<String, Double> currencyToAskMap;
    private Map<String, Double> currencyToBidMap;
    
    public ExchangeData() {
        currencyToRateMap = new HashMap<String, Double>();
        currencyToBidMap = new HashMap<String, Double>();
        currencyToAskMap = new HashMap<String, Double>();
    }
    
    public double getLastRate(String currency) {
        Double rate = currencyToRateMap.get(currency);
        if (rate == null) {
            return DO_NOT_KNOW;
        } else {
            return rate.doubleValue();
        }
    }

    public double getLastBid(String currency) {
        Double bid = currencyToBidMap.get(currency);
        if (bid == null) {
            return DO_NOT_KNOW;
        } else {
            return bid.doubleValue();
        }
    }
    
    public double getLastAsk(String currency) {
        Double ask = currencyToAskMap.get(currency);
        if (ask == null) {
            return DO_NOT_KNOW;
        } else {
            return ask.doubleValue();
        }
    }

    public void setLastRate(String currency, double lastRate) {
        currencyToRateMap.put(currency, new Double(lastRate));
    }

    public void setLastBid(String currency, double lastBid) {
        currencyToBidMap.put(currency, new Double(lastBid));
    }

    public void setLastAsk(String currency, double lastAsk) {
        currencyToAskMap.put(currency, new Double(lastAsk));
    }

    public String[] getAvailableExchanges() {
        return new String[] { MT_GOX_EXCHANGE_NAME };
    }

    public String[] getAvailableCurrenciesForExchange(String exchangeName) {
        return new String[] { "USD", "EUR", "GBP"};
    }
}
