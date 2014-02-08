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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Locale;
import java.util.Map;

import junit.framework.TestCase;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Test;
import org.multibit.CreateControllers;
import org.multibit.Localiser;
import org.multibit.controller.core.CoreController;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.exchange.ExchangeModel;

public class CurrencyConverterTest extends TestCase {
    @Test
    public void testBasic() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;

        // set the default currency to USD
        controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY, "USD");
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;
        assertNotNull(converter);

        // Initialise - will pick up currency of interest.
        converter.initialise(controller);
        
        // Default currency should be USD
        assertEquals("Wrong default currency.1", CurrencyUnit.of("USD"), converter.getCurrencyUnit());
        
        // Initialise to CAD.
        controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY, "CAD");
        converter.initialise(controller);
        // Default currency should be CAD
        assertEquals("Wrong default currency.2", CurrencyUnit.of("CAD"), converter.getCurrencyUnit());
        
        // Set and get the exchange rate
        assertNull("Exchange rate was not initially null", converter.getRate());
        
        converter.setRate(BigDecimal.valueOf(5.0));
        assertEquals("Rate not set", BigDecimal.valueOf(5.0), converter.getRate());
    }
    
    @Test
    public void testConvert() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        // Initialise - will pick up currency of interest.
        converter.initialise(controller);
        converter.setCurrencyUnit(CurrencyUnit.of("USD"));
        converter.setRate(BigDecimal.valueOf(5.0));

        Money converted = converter.convertFromBTCToFiat(CurrencyConverter.NUMBER_OF_SATOSHI_IN_ONE_BITCOIN);   // 1 dogecoin
        
        assertNotNull(converted);
        assertEquals("Wrong currency", "USD", converted.getCurrencyUnit().getCurrencyCode());
        assertEquals("Wrong amount", "5.00", converted.getAmount().toString());
        
        String result = converter.getFiatAsLocalisedString(converted);
        assertEquals("Wrong result after formatting", "$5.00", result);
    }
    
    @Test 
    public void testFormatter() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "GBP");

        String result = converter.getFiatAsLocalisedString(Money.parse("GBP 1.23"));
        assertEquals("Wrong GBP formatting", "\u00A31.23", result);

        converter.initialise(controller, "EUR");

        result = converter.getFiatAsLocalisedString(Money.parse("EUR 1.23"));
        assertEquals("Wrong EURO formatting", "\u20AC1.23", result);
    }
    
    @Test 
    public void testGetBTCAsLocalisedStringEnglish() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(new Localiser(Locale.ENGLISH));
        final BitcoinController controller = controllers.bitcoinController;
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "GBP");

        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);
        String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.1", "1.23456789", result);

        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());
 
        testBTCAmount = BigDecimal.valueOf(1234567890123L);
        result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.2", "12,345.67890123", result);
        
        converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());
    }
    
    @Test 
    public void testGetBTCAsLocalisedStringGerman() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(new Localiser(Locale.GERMAN));
        final BitcoinController controller = controllers.bitcoinController;
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "EUR");

        
        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);
        String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.1", "1,23456789", result);

        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());        
        
        testBTCAmount = BigDecimal.valueOf(1234567890123L);
        result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.2", "12.345,67890123", result);
        
        converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());
    }
    
    @Test 
    public void testGetBTCAsLocalisedStringFrench() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(new Localiser(Locale.FRENCH));
        final BitcoinController controller = controllers.bitcoinController;
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "EUR");

        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);
        String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.1", "1,23456789", result);

        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());
        

        testBTCAmount = BigDecimal.valueOf(1234567890123L);
        result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.2", "12\u00A0345,67890123", result);
        
        converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());

        converterResult = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertNotNull(converterResult);
        assertEquals(testBTCAmount, converterResult.getBtcMoney().getAmount());
    }
    
    @Test 
    public void testAllCurrencies() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(new Localiser(Locale.FRENCH));
        final BitcoinController controller = controllers.bitcoinController;
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;
        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);

        // Cycle through all the currencies, making sure they all initialise
        Map<String, String> currencyCodeToDescriptionMap = converter.getCurrencyCodeToDescriptionMap();
        for (String currencyCode : currencyCodeToDescriptionMap.keySet()) {
            converter.initialise(controller, currencyCode);     
            String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, testBTCAmount));
            CurrencyConverterResult currencyConverterResult = converter.convertFromFiatToBTC("1.0");
        }
    }

}
