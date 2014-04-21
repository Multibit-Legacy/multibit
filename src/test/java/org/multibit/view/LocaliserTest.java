/**
 * Copyright 2011 multibit.org
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
package org.multibit.view;

import junit.framework.TestCase;
import org.junit.Test;
import org.multibit.Localiser;

import java.math.BigInteger;
import java.util.Locale;

public class LocaliserTest extends TestCase {
    // some example keys that are in the viewer i18n files for test purposes
    private String CAPITAL_CITY_KEY = "localiserTest.capitalCity";
    private String SUBSTITUTE_ONE_KEY = "localiserTest.substituteOne";
    private String SUBSTITUTE_TWO_KEY = "localiserTest.substituteTwo";
    
    private String SUBSTITUTE_ONE_DATA="A";
    private String SUBSTITUTE_TWO_DATA="B";
       
    private String CAPITAL_CITY_EXPECTED_ENGLISH = "London";
    private String SUBSTITUTE_ONE_EXPECTED_ENGLISH = "first = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_ENGLISH = "first = " + SUBSTITUTE_ONE_DATA + ", second = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_SPANISH = "Madrid";
    private String SUBSTITUTE_ONE_EXPECTED_SPANISH = "primero = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_SPANISH = "primero = " + SUBSTITUTE_ONE_DATA + ", segundo = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_RUSSIAN = "\u041c\u043e\u0441\u043a\u0432\u0430";
    private String SUBSTITUTE_ONE_EXPECTED_RUSSIAN = "\u043F\u0435\u0440\u0432\u044B\u0439 = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_RUSSIAN = "\u043F\u0435\u0440\u0432\u044B\u0439 = " + SUBSTITUTE_ONE_DATA + ", \u0432\u0442\u043E\u0440\u043E\u0439 = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_SWEDISH = "Stockholm";
    private String SUBSTITUTE_ONE_EXPECTED_SWEDISH = "f\u00f6rsta = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_SWEDISH = "f\u00f6rsta = " + SUBSTITUTE_ONE_DATA + ", andra = " + SUBSTITUTE_TWO_DATA;
 
    private String CAPITAL_CITY_EXPECTED_NORWEGIAN = "Oslo";
    private String SUBSTITUTE_ONE_EXPECTED_NORWEGIAN = "f\u00f8rste = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_NORWEGIAN = "f\u00f8rste = " + SUBSTITUTE_ONE_DATA + ", andre = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_ITALIAN = "Roma";
    private String SUBSTITUTE_ONE_EXPECTED_ITALIAN = "primo = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_ITALIAN = "primo = " + SUBSTITUTE_ONE_DATA + ", secondo = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_FRENCH = "Paris";
    private String SUBSTITUTE_ONE_EXPECTED_FRENCH = "premier = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_FRENCH = "premier = " + SUBSTITUTE_ONE_DATA + ", deuxi\u00e8me = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_GERMAN = "Berlin";
    private String SUBSTITUTE_ONE_EXPECTED_GERMAN = "erster = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_GERMAN = "erster = " + SUBSTITUTE_ONE_DATA + ", zweiter = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_PORTUGUESE = "Lisboa";
    private String SUBSTITUTE_ONE_EXPECTED_PORTUGUESE = "Primeiro = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_PORTUGUESE = "Primeiro = " + SUBSTITUTE_ONE_DATA + ", Segundo = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_DUTCH = "Amsterdam";
    private String SUBSTITUTE_ONE_EXPECTED_DUTCH = "eerste = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_DUTCH = "eerste = " + SUBSTITUTE_ONE_DATA + ", tweede = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_LATVIAN = "R\u012bga";
    private String SUBSTITUTE_ONE_EXPECTED_LATVIAN = "pirmais = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_LATVIAN = "pirmais = " + SUBSTITUTE_ONE_DATA + ", otrais = " + SUBSTITUTE_TWO_DATA;

    private String CAPITAL_CITY_EXPECTED_TURKISH = "Ankara";
    private String SUBSTITUTE_ONE_EXPECTED_TURKISH = "ilk = " + SUBSTITUTE_ONE_DATA;
    private String SUBSTITUTE_TWO_EXPECTED_TURKISH = "ilk = " + SUBSTITUTE_ONE_DATA + ", ikinci = " + SUBSTITUTE_TWO_DATA;

 
     
    
    
    @Test
    public void testLocaliseEnglish() {
        Localiser localiser = new Localiser(new Locale("en"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_ENGLISH, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_ENGLISH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_ENGLISH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));
                       
    }
    
    @Test
    public void testLocaliseSpanish() {
        Localiser localiser = new Localiser(new Locale("es"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_SPANISH, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_SPANISH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_SPANISH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
    
    @Test
    public void testLocaliseRussian() {
        Localiser localiser = new Localiser(new Locale("ru"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_RUSSIAN, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_RUSSIAN, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_RUSSIAN, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
    
    @Test
    public void testLocaliseSwedish() {
        Localiser localiser = new Localiser(new Locale("sv"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_SWEDISH, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_SWEDISH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_SWEDISH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
    
    @Test
    public void testLocaliseNorwegian() {
        Localiser localiser = new Localiser(new Locale("no"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_NORWEGIAN, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_NORWEGIAN, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_NORWEGIAN, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
    
    @Test
    public void testLocaliseItalian() {
        Localiser localiser = new Localiser(new Locale("it"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_ITALIAN, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_ITALIAN, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_ITALIAN, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
    
    @Test
    public void testLocaliseFrench() {
        Localiser localiser = new Localiser(new Locale("fr"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_FRENCH, localiser.getString(CAPITAL_CITY_KEY));
        //assertEquals(SUBSTITUTE_ONE_EXPECTED_FRENCH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        //assertEquals(SUBSTITUTE_TWO_EXPECTED_FRENCH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
 
   @Test
    public void testLocaliseGerman() {
        Localiser localiser = new Localiser(new Locale("de"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_GERMAN, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_GERMAN, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_GERMAN, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
    }
    
   @Test
   public void testLocalisePortuguese() {
       Localiser localiser = new Localiser(new Locale("pt"));

       assertNotNull(localiser);
      
       assertEquals(CAPITAL_CITY_EXPECTED_PORTUGUESE, localiser.getString(CAPITAL_CITY_KEY));
       assertEquals(SUBSTITUTE_ONE_EXPECTED_PORTUGUESE, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
       assertEquals(SUBSTITUTE_TWO_EXPECTED_PORTUGUESE, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
   }
    
   @Test
   public void testLocaliseDutch() {
       Localiser localiser = new Localiser(new Locale("nl"));

       assertNotNull(localiser);
      
       //assertEquals(CAPITAL_CITY_EXPECTED_DUTCH, localiser.getString(CAPITAL_CITY_KEY));
       assertEquals(SUBSTITUTE_ONE_EXPECTED_DUTCH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
       assertEquals(SUBSTITUTE_TWO_EXPECTED_DUTCH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
   }
   
   @Test
   public void testLocaliseLatvian() {
       Localiser localiser = new Localiser(new Locale("lv"));

       assertNotNull(localiser);
      
       assertEquals(CAPITAL_CITY_EXPECTED_LATVIAN, localiser.getString(CAPITAL_CITY_KEY));
       assertEquals(SUBSTITUTE_ONE_EXPECTED_LATVIAN, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
       assertEquals(SUBSTITUTE_TWO_EXPECTED_LATVIAN, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
   }
    
   
   @Test
   public void testLocaliseTurkish() {
       Localiser localiser = new Localiser(new Locale("tr"));

       assertNotNull(localiser);
      
       //assertEquals(CAPITAL_CITY_EXPECTED_TURKISH, localiser.getString(CAPITAL_CITY_KEY));
       assertEquals(SUBSTITUTE_ONE_EXPECTED_TURKISH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
       assertEquals(SUBSTITUTE_TWO_EXPECTED_TURKISH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));                    
   }
    
    @Test
    public void testVersionNumber() {
        Localiser localiser = new Localiser(new Locale("en"));

        assertNotNull(localiser);
       
        // do not know what the version is, but it should be there and not be empty
        String versionSeen = localiser.getVersionNumber();
        assertTrue(versionSeen != null && versionSeen.length() > 0);                       
    }
    
    @Test
    public void testBitcoinValueToStringEnglish() {
        Localiser localiser = new Localiser(new Locale("en"));
        BigInteger bigPositiveValue = BigInteger.valueOf(1234567890123L);
        BigInteger positiveValue = BigInteger.valueOf(1234567890);
        
        BigInteger bigNegativeValue = BigInteger.valueOf(-1234567890123L);
        BigInteger negativeValue = BigInteger.valueOf(-1234567890);
        
        assertEquals("12,345.67890123", localiser.bitcoinValueToString(bigPositiveValue, false, false));    
        assertEquals("12,345.67890123 BTC", localiser.bitcoinValueToString(bigPositiveValue, true, false));    
        assertEquals("12,345.67890123 BTC", localiser.bitcoinValueToString(bigPositiveValue, true, true));    
        assertEquals("12,345.67890123", localiser.bitcoinValueToString(bigPositiveValue, false, true));    

        assertEquals("12.3456789", localiser.bitcoinValueToString(positiveValue, false, false));    
        assertEquals("12.3456789 BTC", localiser.bitcoinValueToString(positiveValue, true, false));    
        assertEquals("12.3456789 BTC", localiser.bitcoinValueToString(positiveValue, true, true));    
        assertEquals("12.3456789", localiser.bitcoinValueToString(positiveValue, false, true));    

        assertEquals("-12,345.67890123", localiser.bitcoinValueToString(bigNegativeValue, false, false));    
        assertEquals("-12,345.67890123 BTC", localiser.bitcoinValueToString(bigNegativeValue, true, false));    
        assertEquals("-12,345.67890123 BTC", localiser.bitcoinValueToString(bigNegativeValue, true, true));    
        assertEquals("-12,345.67890123", localiser.bitcoinValueToString(bigNegativeValue, false, true));    

        assertEquals("-12.3456789", localiser.bitcoinValueToString(negativeValue, false, false));    
        assertEquals("-12.3456789 BTC", localiser.bitcoinValueToString(negativeValue, true, false));    
        assertEquals("-12.3456789 BTC", localiser.bitcoinValueToString(negativeValue, true, true));    
        assertEquals("-12.3456789", localiser.bitcoinValueToString(negativeValue, false, true));    

        assertEquals("0", localiser.bitcoinValueToString(BigInteger.ZERO, false, false));    
        assertEquals("0 BTC", localiser.bitcoinValueToString(BigInteger.ZERO, true, false));    
        assertEquals("", localiser.bitcoinValueToString(BigInteger.ZERO, true, true));    
        assertEquals("", localiser.bitcoinValueToString(BigInteger.ZERO, false, true));    
    }
    
    @Test
    public void testBitcoinValueToStringGerman() {
        Localiser localiser = new Localiser(new Locale("de"));
        BigInteger bigPositiveValue = BigInteger.valueOf(1234567890123L);
        BigInteger positiveValue = BigInteger.valueOf(1234567890);
        
        BigInteger bigNegativeValue = BigInteger.valueOf(-1234567890123L);
        BigInteger negativeValue = BigInteger.valueOf(-1234567890);
        
        assertEquals("12.345,67890123", localiser.bitcoinValueToString(bigPositiveValue, false, false));    
        assertEquals("12.345,67890123 BTC", localiser.bitcoinValueToString(bigPositiveValue, true, false));    
        assertEquals("12.345,67890123 BTC", localiser.bitcoinValueToString(bigPositiveValue, true, true));    
        assertEquals("12.345,67890123", localiser.bitcoinValueToString(bigPositiveValue, false, true));    

        assertEquals("12,3456789", localiser.bitcoinValueToString(positiveValue, false, false));    
        assertEquals("12,3456789 BTC", localiser.bitcoinValueToString(positiveValue, true, false));    
        assertEquals("12,3456789 BTC", localiser.bitcoinValueToString(positiveValue, true, true));    
        assertEquals("12,3456789", localiser.bitcoinValueToString(positiveValue, false, true));    

        assertEquals("-12.345,67890123", localiser.bitcoinValueToString(bigNegativeValue, false, false));    
        assertEquals("-12.345,67890123 BTC", localiser.bitcoinValueToString(bigNegativeValue, true, false));    
        assertEquals("-12.345,67890123 BTC", localiser.bitcoinValueToString(bigNegativeValue, true, true));    
        assertEquals("-12.345,67890123", localiser.bitcoinValueToString(bigNegativeValue, false, true));    

        assertEquals("-12,3456789", localiser.bitcoinValueToString(negativeValue, false, false));    
        assertEquals("-12,3456789 BTC", localiser.bitcoinValueToString(negativeValue, true, false));    
        assertEquals("-12,3456789 BTC", localiser.bitcoinValueToString(negativeValue, true, true));    
        assertEquals("-12,3456789", localiser.bitcoinValueToString(negativeValue, false, true));    

        assertEquals("0", localiser.bitcoinValueToString(BigInteger.ZERO, false, false));    
        assertEquals("0 BTC", localiser.bitcoinValueToString(BigInteger.ZERO, true, false));    
        assertEquals("", localiser.bitcoinValueToString(BigInteger.ZERO, true, true));    
        assertEquals("", localiser.bitcoinValueToString(BigInteger.ZERO, false, true));    
    }

  @Test
   public void testBitcoinValueToStringNotLocalised() {
       Localiser localiser = new Localiser(new Locale("de"));
       BigInteger bigPositiveValue = BigInteger.valueOf(1234567890123L);
       BigInteger positiveValue = BigInteger.valueOf(1234567890);

       BigInteger bigNegativeValue = BigInteger.valueOf(-1234567890123L);
       BigInteger negativeValue = BigInteger.valueOf(-1234567890);

       assertEquals("12345.67890123", localiser.bitcoinValueToStringNotLocalised(bigPositiveValue, false, false));
       assertEquals("12345.67890123 BTC", localiser.bitcoinValueToStringNotLocalised(bigPositiveValue, true, false));
       assertEquals("12345.67890123 BTC", localiser.bitcoinValueToStringNotLocalised(bigPositiveValue, true, true));
       assertEquals("12345.67890123", localiser.bitcoinValueToStringNotLocalised(bigPositiveValue, false, true));

       assertEquals("12.3456789", localiser.bitcoinValueToStringNotLocalised(positiveValue, false, false));
       assertEquals("12.3456789 BTC", localiser.bitcoinValueToStringNotLocalised(positiveValue, true, false));
       assertEquals("12.3456789 BTC", localiser.bitcoinValueToStringNotLocalised(positiveValue, true, true));
       assertEquals("12.3456789", localiser.bitcoinValueToStringNotLocalised(positiveValue, false, true));

       assertEquals("-12345.67890123", localiser.bitcoinValueToStringNotLocalised(bigNegativeValue, false, false));
       assertEquals("-12345.67890123 BTC", localiser.bitcoinValueToStringNotLocalised(bigNegativeValue, true, false));
       assertEquals("-12345.67890123 BTC", localiser.bitcoinValueToStringNotLocalised(bigNegativeValue, true, true));
       assertEquals("-12345.67890123", localiser.bitcoinValueToStringNotLocalised(bigNegativeValue, false, true));

       assertEquals("-12.3456789", localiser.bitcoinValueToStringNotLocalised(negativeValue, false, false));
       assertEquals("-12.3456789 BTC", localiser.bitcoinValueToStringNotLocalised(negativeValue, true, false));
       assertEquals("-12.3456789 BTC", localiser.bitcoinValueToStringNotLocalised(negativeValue, true, true));
       assertEquals("-12.3456789", localiser.bitcoinValueToStringNotLocalised(negativeValue, false, true));

       assertEquals("0", localiser.bitcoinValueToStringNotLocalised(BigInteger.ZERO, false, false));
       assertEquals("0 BTC", localiser.bitcoinValueToStringNotLocalised(BigInteger.ZERO, true, false));
       assertEquals("", localiser.bitcoinValueToStringNotLocalised(BigInteger.ZERO, true, true));
       assertEquals("", localiser.bitcoinValueToStringNotLocalised(BigInteger.ZERO, false, true));
   }
}
