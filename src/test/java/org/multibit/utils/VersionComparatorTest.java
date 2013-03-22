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
package org.multibit.utils;

import junit.framework.TestCase;

import org.junit.Test;

public class VersionComparatorTest extends TestCase {
    @Test
    public void testRegular() throws Exception {
        VersionComparator comparator = new VersionComparator();
        assertTrue(comparator.compare("0.0.1", "0.0.1") == 0);
        assertTrue(comparator.compare("0.0.2", "0.0.1") == 1);
        assertTrue(comparator.compare("0.0.1", "0.0.2") == -1);

        assertTrue(comparator.compare("0.1", "0.1") == 0);
        assertTrue(comparator.compare("0.2", "0.1") == 1);
        assertTrue(comparator.compare("0.1", "0.2") == -1);

        assertTrue(comparator.compare("1.0", "1.0") == 0);
        assertTrue(comparator.compare("2.0", "1.0") == 1);
        assertTrue(comparator.compare("1.0", "2.0") == -1);

        assertTrue(comparator.compare("1.4", "1.4") == 0);
        assertTrue(comparator.compare("2.4", "2.3") == 1);
        assertTrue(comparator.compare("2.3", "2.4") == -1);

        assertTrue(comparator.compare("1.4.7", "1.4.7") == 0);
        assertTrue(comparator.compare("1.4.8", "1.4.7") == 1);
        assertTrue(comparator.compare("1.4.7", "1.4.8") == -1);
 
    }
    
    @Test
    public void testAlpha() throws Exception {
        VersionComparator comparator = new VersionComparator();
        assertTrue(comparator.compare("0.0.1alpha", "0.0.1alpha") == 0);
        assertTrue(comparator.compare("0.0.1", "0.0.1alpha") == 1);
        assertTrue(comparator.compare("0.0.1alpha", "0.0.1") == -1);
        
        assertTrue(comparator.compare("0.0.1alpha2", "0.0.1alpha2") == 0);
        assertTrue(comparator.compare("0.0.1alpha2", "0.0.1alpha1") == 1);
        assertTrue(comparator.compare("0.0.1alpha1", "0.0.1alpha2") == -1);

        assertTrue(comparator.compare("0.1alpha", "0.1alpha") == 0);
        assertTrue(comparator.compare("0.1", "0.1alpha") == 1);
        assertTrue(comparator.compare("0.2", "0.1alpha") == 1);
        assertTrue(comparator.compare("0.1alpha", "0.1") == -1);

        assertTrue(comparator.compare("0.4.6", "0.5.0alpha") == -1);
    }    
    
    @Test
    public void testBeta() throws Exception {
        VersionComparator comparator = new VersionComparator();
        assertTrue(comparator.compare("0.0.1beta", "0.0.1beta") == 0);
        assertTrue(comparator.compare("0.0.1", "0.0.1beta") == 1);
        assertTrue(comparator.compare("0.0.1alpha", "0.0.1") == -1);
        
        assertTrue(comparator.compare("0.0.1beta2", "0.0.1beta2") == 0);
        assertTrue(comparator.compare("0.0.1beta2", "0.0.1beta1") == 1);
        assertTrue(comparator.compare("0.0.1beta1", "0.0.1beta2") == -1);
        
        assertTrue(comparator.compare("0.0.1alpha1", "0.0.1beta1") == -1);
        assertTrue(comparator.compare("0.0.1alpha2", "0.0.1beta1") == -1);
    }
    
    @Test
    public void testReleaseCandidate() throws Exception {
        VersionComparator comparator = new VersionComparator();
        assertTrue(comparator.compare("0.0.1rc", "0.0.1rc") == 0);
        assertTrue(comparator.compare("0.0.1", "0.0.1rc") == 1);
        assertTrue(comparator.compare("0.0.1rc", "0.0.1") == -1);
        
        assertTrue(comparator.compare("0.0.1rc2", "0.0.1rc2") == 0);
        assertTrue(comparator.compare("0.0.1rc2", "0.0.1rc1") == 1);
        assertTrue(comparator.compare("0.0.1rc1", "0.0.1rc2") == -1);
        
        assertTrue(comparator.compare("0.0.1rc1", "0.0.1beta1") == 1);
        assertTrue(comparator.compare("0.0.1rc1", "0.0.1beta2") == 1);
        assertTrue(comparator.compare("0.0.1rc2", "0.0.1alpha1") == 1);
        assertTrue(comparator.compare("0.0.1rc2", "0.0.1alpha2") == 1);
    }
    
    @Test
    public void testSnapshot() throws Exception {
        VersionComparator comparator = new VersionComparator();
        assertTrue(comparator.compare("0.4.23", "0.4.24-SNAPSHOT") == -1);
        assertTrue(comparator.compare("0.4.24-SNAPSHOT", "0.4.23") == 1);
        assertTrue(comparator.compare("0.4.24-SNAPSHOT", "0.4.24-SNAPSHOT") == 0);

    }
}
