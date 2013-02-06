/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.model.exchange;

import org.multibit.model.AbstractModel;
import org.multibit.model.ModelEnum;
import org.multibit.model.core.CoreModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public class ExchangeModel extends AbstractModel<CoreModel> {
    
   private static final Logger log = LoggerFactory.getLogger(ExchangeModel.class);
    
    
    // Currency ticker.
    public static final String TICKER_SHOW = "tickerShow";
    public static final String TICKER_COLUMNS_TO_SHOW = "tickerColumnsToShow";
    public static final String TICKER_FIRST_ROW_EXCHANGE = "tickerFirstRowExchange";
    public static final String TICKER_FIRST_ROW_CURRENCY = "tickerFirstRowCurrency";
    public static final String TICKER_SHOW_SECOND_ROW = "tickerShowSecondRow";
    public static final String TICKER_SECOND_ROW_EXCHANGE = "tickerSecondRowExchange";
    public static final String TICKER_SECOND_ROW_CURRENCY = "tickerSecondRowCurrency";
   
    // Currency support.
    public static final String SHOW_BITCOIN_CONVERTED_TO_FIAT = "showBitcoinConvertedToFiat";   // boolean
    public static final String USE_LAST_AS_EXCHANGE_RATE = "useLastAsExchangeRate";             // boolean
    public static final String USE_BID_AS_EXCHANGE_RATE = "useBidAsExchangeRate";               // boolean
    public static final String USE_ASK_AS_EXCHANGE_RATE = "useAskAsExchangeRate";               // boolean
    public static final String SHOW_BTC_IN_WALLET_PANEL = "showBTCinWalletPanel";               // boolean
    
    
    /**
     * Holds exchange Data.
     * Two simultaneous exchanges are supported.
     */
    private ExchangeData exchangeData1;
    private ExchangeData exchangeData2;;
    
    
    public ExchangeModel(CoreModel coreModel){
        this(coreModel, null, null);
    }
    
    
    public ExchangeModel(CoreModel coreModel, String shortExchangeName1, String shortExchangeName2){
        super(coreModel);
        
        exchangeData1 = new ExchangeData();
        exchangeData2 = new ExchangeData();
        
        String name1 = shortExchangeName1;
        String name2 = shortExchangeName2;
        
        if (null == name1){
            name1 = super.getUserPreference(TICKER_FIRST_ROW_EXCHANGE);
            if (null == name1) {
                name1 = "USD"; // hardcoded default for now
            }
        }
        
        if (null == name2){
            name2 = super.getUserPreference(TICKER_SECOND_ROW_EXCHANGE);
            if (null == name2) {
                name2 = "CAD"; // hardcoded default for now
            }
        }
        
        exchangeData1.setShortExchangeName(name1);
        exchangeData2.setShortExchangeName(name2);
    }
    
    
    @Override
    public ModelEnum getModelEnum() {
        return ModelEnum.EXCHANGE;
    }
    
    public ExchangeData getExchangeData1() {
        return exchangeData1;
    }

    public void setExchangeData1(ExchangeData exchangeData) {
        this.exchangeData1 = exchangeData;
    }
    
    public ExchangeData getExchangeData2() {
        return exchangeData2;
    }

    public void setExchangeData2(ExchangeData exchangeData) {
        this.exchangeData2 = exchangeData;
    }

    
}
