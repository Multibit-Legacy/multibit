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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.joda.money.BigMoney;

import com.xeiam.xchange.bitcoincharts.BitcoinChartsExchange;
import com.xeiam.xchange.bitstamp.BitstampExchange;
import com.xeiam.xchange.btce.BTCEExchange;
import com.xeiam.xchange.campbx.CampBXExchange;
import com.xeiam.xchange.oer.OERExchange;
import com.xeiam.xchange.virtex.VirtExExchange;

/**
 * 
 * @author timmolter
 * 
 */
public class ExchangeData {

    public static final String EXCHANGE_NOT_SET = "NoExchangeSetYet";
    public static final String MT_GOX_EXCHANGE_NAME = "MtGox";
    public static final String BITCOIN_CHARTS_EXCHANGE_NAME = "BitcoinCharts";
    public static final String VIRTEX_EXCHANGE_NAME = "VirtEx";
    public static final String BTCE_EXCHANGE_NAME = "BTC-E";
    public static final String OPEN_EXCHANGE_RATES_EXCHANGE_NAME = "OpenExchangeRates";
    public static final String BITSTAMP_EXCHANGE_NAME = "Bitstamp";
    public static final String BITCOIN_CENTRAL_EXCHANGE_NAME = "BitcoinCentral";
    public static final String CAMPBX_EXCHANGE_NAME = "CampBX";

    public static final String DEFAULT_EXCHANGE = MT_GOX_EXCHANGE_NAME;
    
    public static final String DEFAULT_CURRENCY = "USD";
    
    public static final Collection<String> DEFAULT_CURRENCY_LIST = new ArrayList<String>();
    
    static {
        DEFAULT_CURRENCY_LIST.add("USD");
        DEFAULT_CURRENCY_LIST.add("EUR");
    }
    
    public static final BigMoney DO_NOT_KNOW = null;
    
    private String shortExchangeName;
    private Map<String, BigMoney> currencyToLastPriceMap;
    private Map<String, BigMoney> currencyToAskMap;
    private Map<String, BigMoney> currencyToBidMap;
    
    private Collection<String> currenciesWeAreInterestedIn;
    
    private static Map<String,  Collection<String>> exchangeNameToAvailableCurrenciesMap  = new HashMap<String, Collection<String>>();
    
    public ExchangeData() {
        setShortExchangeName(EXCHANGE_NOT_SET);
        currencyToLastPriceMap = new HashMap<String, BigMoney>();
        currencyToBidMap = new HashMap<String, BigMoney>();
        currencyToAskMap = new HashMap<String, BigMoney>();
        currenciesWeAreInterestedIn = DEFAULT_CURRENCY_LIST;
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

    public static String[] getAvailableExchanges() {
//        return new String[] { MT_GOX_EXCHANGE_NAME, BITCOIN_CENTRAL_EXCHANGE_NAME, BITCOIN_CHARTS_EXCHANGE_NAME, BITSTAMP_EXCHANGE_NAME, BTCE_EXCHANGE_NAME,  
//                CAMPBX_EXCHANGE_NAME, OPEN_EXCHANGE_RATES_EXCHANGE_NAME, VIRTEX_EXCHANGE_NAME};
        return new String[] { MT_GOX_EXCHANGE_NAME,
            //BITCOIN_CENTRAL_EXCHANGE_NAME,
            BITSTAMP_EXCHANGE_NAME,
            BTCE_EXCHANGE_NAME,
            CAMPBX_EXCHANGE_NAME,
            VIRTEX_EXCHANGE_NAME};
    }

    public Collection<String> getCurrenciesWeAreInterestedIn() {
        return currenciesWeAreInterestedIn;
    }

    public void setCurrenciesWeAreInterestedIn(Collection<String> currenciesWeAreInterestedIn) {
        this.currenciesWeAreInterestedIn = currenciesWeAreInterestedIn;
    }

    public static Collection<String>getAvailableCurrenciesForExchange(String shortExchangeName) {
        Collection<String>availableCurrencies = exchangeNameToAvailableCurrenciesMap.get(shortExchangeName);
        if (availableCurrencies == null) {
            return DEFAULT_CURRENCY_LIST;
        } else {
            return availableCurrencies;
        }
    }

    public static void setAvailableCurrenciesForExchange(String exchangeName, Collection<String> currencies) {
        exchangeNameToAvailableCurrenciesMap.put(exchangeName, currencies);
    }
    
    
    /**
     * Convert an exchange short name into a classname that can be used to create an Exchange.
     */
    public static String convertExchangeShortNameToClassname(String shortExchangeName) {
        if (MT_GOX_EXCHANGE_NAME.equals(shortExchangeName)) {
            return "com.xeiam.xchange.mtgox.v1.MtGoxExchange";
        } else if (BITCOIN_CHARTS_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  BitcoinChartsExchange.class.getName();
        } else if (VIRTEX_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  VirtExExchange.class.getName();
        } else if (BTCE_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  BTCEExchange.class.getName();
        } else if (OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  OERExchange.class.getName();
//        } else if (BITCOIN_CENTRAL_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
//            return  BitcoinCentralExchange.class.getName();
        } else if (CAMPBX_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  CampBXExchange.class.getName();
        }else if (BITSTAMP_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  BitstampExchange.class.getName();
        } else {
            // Unidentified exchange.
            return null;
        }
    }

    public void setShortExchangeName(String shortExchangeName) {
        this.shortExchangeName = shortExchangeName;
    }

    public String getShortExchangeName() {
        return shortExchangeName;
    }
}
