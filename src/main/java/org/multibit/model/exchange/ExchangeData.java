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
package org.multibit.model.exchange;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.joda.money.BigMoney;

import com.xeiam.xchange.bitstamp.BitstampExchange;
import com.xeiam.xchange.btce.BTCEExchange;
//import com.xeiam.xchange.btcchina.BTCChinaExchange;
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

    public static final String BTCCHINA_EXCHANGE_NAME = "BTCChina";
    public static final String BITCOIN_24_EXCHANGE_NAME = "Bitcoin-24";
    public static final String BITCOIN_CENTRAL_EXCHANGE_NAME = "BitcoinCentral";
    public static final String BITCOIN_CHARTS_EXCHANGE_NAME = "BitcoinCharts";
    public static final String BITFLOOR_EXCHANGE_NAME = "Bitfloor";
    public static final String BITSTAMP_EXCHANGE_NAME = "Bitstamp";
    public static final String BTCE_EXCHANGE_NAME = "BTC-E";
    public static final String CAMPBX_EXCHANGE_NAME = "CampBX";
    public static final String OPEN_EXCHANGE_RATES_EXCHANGE_NAME = "OpenExchangeRates";
    public static final String MT_GOX_EXCHANGE_NAME = "MtGox";
    public static final String VIRTEX_EXCHANGE_NAME = "VirtEx";

    public static final String DEFAULT_EXCHANGE = MT_GOX_EXCHANGE_NAME;
    
    public static final String DEFAULT_CURRENCY = "USD";
    
    public static final Collection<String> DEFAULT_CURRENCY_LIST = new ArrayList<String>();
    
    static {
        DEFAULT_CURRENCY_LIST.add("USD");
        DEFAULT_CURRENCY_LIST.add("EUR");
    }
    
    public static final BigMoney DO_NOT_KNOW = null;
    
    private String shortExchangeName;
    private final Map<String, BigMoney> currencyToLastPriceMap;
    private final Map<String, BigMoney> currencyToAskMap;
    private final Map<String, BigMoney> currencyToBidMap;
    private final Map<String, Date> currencyToLastUpdated;
       
    private static Map<String,  Collection<String>> exchangeNameToAvailableCurrenciesMap  = new HashMap<String, Collection<String>>();
    
    public ExchangeData() {
        setShortExchangeName(EXCHANGE_NOT_SET);
        currencyToLastPriceMap = new HashMap<String, BigMoney>();
        currencyToBidMap = new HashMap<String, BigMoney>();
        currencyToAskMap = new HashMap<String, BigMoney>();
        currencyToLastUpdated = new HashMap<String, Date>();
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

    public Date getLastUpdated(String currency){
        return currencyToLastUpdated.get(currency);
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

    public void setLastUpdated(String currency, Date lastUpdated){
        currencyToLastUpdated.put(currency, lastUpdated);
    }

    /**
     * Exchanges normally use a CurrencyPair with BTC as the base currency and the other as the countercurrency. e.g. BTCUSD = 20 i.e 1 BTC is worth 20 USD.
     * Some exchanges use reverse rates i.e they quote USDBTC = 20.
     * 
     * @return does this exchange use reverse rates
     */
    public static boolean doesExchangeUseReverseRates(String shortExchangeName) {
        if (shortExchangeName == null) {
            throw new IllegalArgumentException("Exchange name cannot be null");
        }
        return BITCOIN_CHARTS_EXCHANGE_NAME.equals(shortExchangeName);
    }
    
    public static String[] getAvailableExchanges() {
        return new String[] { MT_GOX_EXCHANGE_NAME,
            //BITCOIN_24_EXCHANGE_NAME,
            //BITCOIN_CENTRAL_EXCHANGE_NAME,
            //BITFLOOR_EXCHANGE_NAME,
            BITSTAMP_EXCHANGE_NAME,
            BTCE_EXCHANGE_NAME,
            //BTCCHINA_EXCHANGE_NAME,
            CAMPBX_EXCHANGE_NAME,
            OPEN_EXCHANGE_RATES_EXCHANGE_NAME,
            VIRTEX_EXCHANGE_NAME};
    }

    public static Collection<String> getAvailableCurrenciesForExchange(String shortExchangeName) {
        Collection<String>availableCurrencies = exchangeNameToAvailableCurrenciesMap.get(shortExchangeName);
        if (availableCurrencies == null) {
            return new ArrayList<String>();
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
            return "com.xeiam.xchange.mtgox.v2.MtGoxExchange";
        //} else if (BITCOIN_CHARTS_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
        //    return  BitcoinChartsExchange.class.getName();
        } else if (BTCE_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  BTCEExchange.class.getName();
        //} else if (BTCCHINA_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
        //    return  BTCChinaExchange.class.getName();
        } else if (OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  OERExchange.class.getName();
        //} else if (BITCOIN_CENTRAL_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
        //    return  BitcoinCentralExchange.class.getName();
        } else if (CAMPBX_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  CampBXExchange.class.getName();
        } else if (BITSTAMP_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  BitstampExchange.class.getName();
       // } else if (BITFLOOR_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
       //     return  BitfloorExchange.class.getName();
       // } else if (BITCOIN_24_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
       //     return  Bitcoin24Exchange.class.getName();
        } else if (VIRTEX_EXCHANGE_NAME.equalsIgnoreCase(shortExchangeName)) {
            return  VirtExExchange.class.getName();
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
