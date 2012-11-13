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
        converter.initialise(controller);
        
        // Default currency should be USD
        assertEquals("Wrong default currency.1", CurrencyUnit.of("USD"), converter.getCurrencyUnit());
        
        // Initialise to CAD.
        controller.getModel().setUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY, "CAD");
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
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);   

        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        // Initialise - will pick up currency of interest.
        converter.initialise(controller);
        converter.setCurrencyUnit(CurrencyUnit.of("USD"));
        converter.setRate(BigDecimal.valueOf(5.0));

        Money converted = converter.convertToFiat(CurrencyConverter.NUMBER_OF_SATOSHI_IN_ONE_BITCOIN);   // 1 bitcoin
        
        assertNotNull(converted);
        assertEquals("Wrong currency", "USD", converted.getCurrencyUnit().getCurrencyCode());
        assertEquals("Wrong amount", "5.00", converted.getAmount().toString());
        
        String result = converter.getMoneyAsString(converted);
        assertEquals("Wrong result after formatting", "$5.00", result);
    }
    
    @Test 
    public void testFormatter() throws Exception {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        
        CurrencyConverter converter = CurrencyConverter.INSTANCE;

        // Initialise - will pick up currency of interest.
        converter.initialise(controller);

        String result = converter.getMoneyAsString(Money.parse("GBP 1.23"));
        assertEquals("Wrong GBP formatting", "£1.23", result);
        
        result = converter.getMoneyAsString(Money.parse("EUR 1.23"));
        assertEquals("Wrong EURO formatting", "\u20AC1.23", result);
    }
}
