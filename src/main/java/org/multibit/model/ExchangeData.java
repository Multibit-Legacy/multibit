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
    
    public static final double DO_NOT_KNOW_EXCHANGE_RATE = -1;
    
    private Map<String, Double> currencyToExchangeRateMap;
    
    public ExchangeData() {
        currencyToExchangeRateMap = new HashMap<String, Double>();
    }
    
    public double getLastTick(String currency) {
        Double exchangeRate = currencyToExchangeRateMap.get(currency);
        if (exchangeRate == null) {
            return DO_NOT_KNOW_EXCHANGE_RATE;
        } else {
            return exchangeRate.doubleValue();
        }
    }

    public void setLastTick(String currency, double lastTick) {
        currencyToExchangeRateMap.put(currency, new Double(lastTick));
    }

    public String[] getAvailableExchanges() {
        return new String[] { MT_GOX_EXCHANGE_NAME };
    }

    public String[] getAvailableCurrenciesForExchange(String exchangeName) {
        return new String[] { "USD", "EUR", "GBP"};
    }
}
