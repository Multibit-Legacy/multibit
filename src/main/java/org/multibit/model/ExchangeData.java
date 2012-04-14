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

import org.joda.money.BigMoney;

/**
 * 
 * @author timmolter
 * 
 */
public class ExchangeData {

    public static final String MT_GOX_EXCHANGE_NAME = "MtGox";

    public static final String DEFAULT_EXCHANGE = MT_GOX_EXCHANGE_NAME;
    
    public static final String DEFAULT_CURRENCY = "USD";
    
    public static final String[] DEFAULT_CURRENCY_LIST = new String[] {"USD",  "EUR"};
    
    public static final BigMoney DO_NOT_KNOW = null;
    
    private Map<String, BigMoney> currencyToLastPriceMap;
    private Map<String, BigMoney> currencyToAskMap;
    private Map<String, BigMoney> currencyToBidMap;
    
    private String[] currenciesWeAreInterestedIn;
    
    private Map<String, String[]> exchangeNameToAvailableCurrenciesMap;
    
    public ExchangeData() {
        currencyToLastPriceMap = new HashMap<String, BigMoney>();
        currencyToBidMap = new HashMap<String, BigMoney>();
        currencyToAskMap = new HashMap<String, BigMoney>();
        currenciesWeAreInterestedIn = DEFAULT_CURRENCY_LIST;
        exchangeNameToAvailableCurrenciesMap = new HashMap<String, String[]>();
    }
    
    public BigMoney getLastPrice(String currency) {
        BigMoney lastPrice = currencyToLastPriceMap.get(currency);
        if (lastPrice == null) {
            return DO_NOT_KNOW;
        } else {
            return lastPrice;
        }
    }

    public BigMoney getLastBid(String currency) {
        BigMoney bid = currencyToBidMap.get(currency);
        if (bid == null) {
            return DO_NOT_KNOW;
        } else {
            return bid;
        }
    }
    
    public BigMoney getLastAsk(String currency) {
        BigMoney ask = currencyToAskMap.get(currency);
        if (ask == null) {
            return DO_NOT_KNOW;
        } else {
            return ask;
        }
    }

    public void setLastPrice(String currency, BigMoney lastPrice) {
        currencyToLastPriceMap.put(currency, lastPrice);
    }

    public void setLastBid(String currency, BigMoney lastBid) {
        currencyToBidMap.put(currency, lastBid);
    }

    public void setLastAsk(String currency, BigMoney lastAsk) {
        currencyToAskMap.put(currency, lastAsk);
    }

    public String[] getAvailableExchanges() {
        return new String[] { MT_GOX_EXCHANGE_NAME };
    }

    public String[] getCurrenciesWeAreInterestedIn() {
        return currenciesWeAreInterestedIn;
    }

    public void setCurrenciesWeAreInterestedIn(String[] currenciesWeAreInterestedIn) {
        this.currenciesWeAreInterestedIn = currenciesWeAreInterestedIn;
    }

    public String[] getAvailableCurrenciesForExchange(String exchangeName) {
        String[] availableCurrencies = exchangeNameToAvailableCurrenciesMap.get(exchangeName);
        if (availableCurrencies == null) {
            return DEFAULT_CURRENCY_LIST;
        } else {
            return availableCurrencies;
        }
    }

    public void setAvailableCurrenciesForExchange(String exchangeName, String[] currencies) {
        exchangeNameToAvailableCurrenciesMap.put(exchangeName, currencies);
    }
}
