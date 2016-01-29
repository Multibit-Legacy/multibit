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

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Cameron Garnham
 */
public class ExchangeModel extends AbstractModel<CoreModel> {

    
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
    
    
    private Map<String, ExchangeData> shortExchangeNameToExchangeMap;
    
    
    public ExchangeModel(CoreModel coreModel){
        super(coreModel);
        
        ExchangeData exchangeData1 = new ExchangeData();
        ExchangeData exchangeData2 = new ExchangeData();

        exchangeData1.setShortExchangeName(getUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE));
        exchangeData2.setShortExchangeName(getUserPreference(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE));
        
        shortExchangeNameToExchangeMap = new HashMap<String, ExchangeData>();
        shortExchangeNameToExchangeMap.put(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE, exchangeData1);
        shortExchangeNameToExchangeMap.put(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE, exchangeData2);
        
    }
    
    @Override
    public ModelEnum getModelEnum() {
        return ModelEnum.EXCHANGE;
    }
    
    public ExchangeData getExchangeData(String shortExchangeName) {
        return shortExchangeNameToExchangeMap.get(shortExchangeName);
    }
    
    public Map<String, ExchangeData> getShortExchangeNameToExchangeMap() {
        return shortExchangeNameToExchangeMap;
    }
    
}
