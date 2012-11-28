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
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Locale;

import junit.framework.TestCase;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.joda.money.format.MoneyFormatter;
import org.junit.Test;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

public class CurrencyConverterTest extends TestCase {
    @Test
    public void testBasic() throws IOException {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);

        CurrencyConverter converter = CurrencyConverter.INSTANCE;
        assertNotNull(converter);

        // Initialise - will pick up currency of interest.
        converter.initialise(controller, null);
        
        // Default currency should be USD
        assertEquals("Wrong default currency.1", CurrencyUnit.of("USD"), converter.getCurrencyUnit());
        
        // Initialise to CAD.
        controller.getModel().setUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY, "CAD");
        converter.initialise(controller, null);
        // Default currency should be CAD
        assertEquals("Wrong default currency.2", CurrencyUnit.of("CAD"), converter.getCurrencyUnit());
        
        // Set and get the exchange rate
        assertNull("Exchange rate was not initially null", converter.getRate());
        
        converter.setRate(BigDecimal.valueOf(5.0));
        assertEquals("Rate not set", BigDecimal.valueOf(5.0), converter.getRate());
    }
    
    @Test
    public void testConvert() throws Exception {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);   

        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        // Initialise - will pick up currency of interest.
        converter.initialise(controller, null);
        converter.setCurrencyUnit(CurrencyUnit.of("USD"));
        converter.setRate(BigDecimal.valueOf(5.0));

        Money converted = converter.convertFromBTCToFiat(CurrencyConverter.NUMBER_OF_SATOSHI_IN_ONE_BITCOIN);   // 1 bitcoin
        
        assertNotNull(converted);
        assertEquals("Wrong currency", "USD", converted.getCurrencyUnit().getCurrencyCode());
        assertEquals("Wrong amount", "5.00", converted.getAmount().toString());
        
        String result = converter.getFiatAsLocalisedString(converted);
        assertEquals("Wrong result after formatting", "$5.00", result);
    }
    
    @Test 
    public void testFormatter() throws Exception {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "GBP", null);

        String result = converter.getFiatAsLocalisedString(Money.parse("GBP 1.23"));
        assertEquals("Wrong GBP formatting", "\u00A31.23", result);

        converter.initialise(controller, "EUR", null);

        result = converter.getFiatAsLocalisedString(Money.parse("EUR 1.23"));
        assertEquals("Wrong EURO formatting", "\u20AC1.23", result);
    }
    
    @Test 
    public void testGetBTCAsLocalisedStringEnglish() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new Localiser(Locale.ENGLISH);
        controller.setLocaliser(localiser);
        
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "GBP", null);

        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);
        String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.1", "1.23456789", result);

        Money parsedBTC = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertEquals(testBTCAmount, parsedBTC.getAmount());
 
        testBTCAmount = BigDecimal.valueOf(1234567890123L);
        result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.2", "12,345.67890123", result);
        
        parsedBTC = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertEquals(testBTCAmount, parsedBTC.getAmount());
    }
    
    @Test 
    public void testGetBTCAsLocalisedStringGerman() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new Localiser(Locale.GERMAN);
        controller.setLocaliser(localiser);
        
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "EUR", null);

        
        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);
        String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.1", "1,23456789", result);

        Money parsedBTC = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertEquals(testBTCAmount, parsedBTC.getAmount());
        
        
        testBTCAmount = BigDecimal.valueOf(1234567890123L);
        result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.2", "12.345,67890123", result);
        
        parsedBTC = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertEquals(testBTCAmount, parsedBTC.getAmount());
    }
    
    @Test 
    public void testGetBTCAsLocalisedStringFrench() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new Localiser(Locale.FRENCH);
        controller.setLocaliser(localiser);
        
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        converter.initialise(controller, "EUR", null);

        BigDecimal testBTCAmount = BigDecimal.valueOf(123456789L);
        String result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.1", "1,23456789", result);

        Money parsedBTC = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertEquals(testBTCAmount, parsedBTC.getAmount());
        

        testBTCAmount = BigDecimal.valueOf(1234567890123L);
        result = converter.getBTCAsLocalisedString(Money.of(CurrencyConverter.BITCOIN_CURRENCY_UNIT, testBTCAmount));
        assertEquals("Wrong BTC localised value.2", "12\u00A0345,67890123", result);
        
        parsedBTC = CurrencyConverter.INSTANCE.parseToBTC(result);
        assertEquals(testBTCAmount, parsedBTC.getAmount());

        parsedBTC = CurrencyConverter.INSTANCE.parseToBTC("12 345,67890123"); // Space rather than nonbreakable space
        assertEquals(testBTCAmount, parsedBTC.getAmount());

    }
}
