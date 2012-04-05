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
package org.multibit.protcolhandler.builders;

import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.multibit.platform.GenericApplication;
import org.multibit.platform.GenericApplicationFactory;
import org.multibit.platform.GenericApplicationSpecification;

public class GenericApplicationFactoryTest {

    /**
     * This test is build specific so is ignored by default
     */
    @Test
    public void testMac() {
        GenericApplicationSpecification specification = new GenericApplicationSpecification();
        GenericApplication testObject = GenericApplicationFactory.INSTANCE.buildGenericApplication(specification);

        assertNotNull(testObject);

    }
}
