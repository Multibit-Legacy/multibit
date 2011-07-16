/**
 * Copyright 2011 Thilo Planz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.multibit;

import java.util.Locale;

import junit.framework.TestCase;

import org.junit.Test;

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

    
    @Test
    public void testLocaliseEnglish() {
        Localiser localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale("en"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_ENGLISH, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_ENGLISH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_ENGLISH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));
                       
    }
    
    @Test
    public void testLocaliseSpanish() {
        Localiser localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale("es"));

        assertNotNull(localiser);
       
        assertEquals(CAPITAL_CITY_EXPECTED_SPANISH, localiser.getString(CAPITAL_CITY_KEY));
        assertEquals(SUBSTITUTE_ONE_EXPECTED_SPANISH, localiser.getString(SUBSTITUTE_ONE_KEY, new Object[]{SUBSTITUTE_ONE_DATA}));
        assertEquals(SUBSTITUTE_TWO_EXPECTED_SPANISH, localiser.getString(SUBSTITUTE_TWO_KEY, new Object[]{SUBSTITUTE_ONE_DATA, SUBSTITUTE_TWO_DATA}));
                       
    }
    
    @Test
    public void testVersionNumber() {
        Localiser localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale("en"));

        assertNotNull(localiser);
       
        // do not know what the version is, but it should be there and not be empty
        String versionSeen = localiser.getVersionNumber();
        assertTrue(versionSeen != null && versionSeen.length() > 0);                       
    }
}
