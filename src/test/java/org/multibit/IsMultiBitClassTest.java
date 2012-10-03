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
package org.multibit;

import junit.framework.TestCase;

import org.junit.Test;

/**
 * Test to ensure that the multibit duplicates of bitcoinj classes are available
 * correctly
 * 
 * @author jim
 * 
 */
public class IsMultiBitClassTest extends TestCase {
    private static final String GOOGLE_PREFIX = "com.google.bitcoin.";

    @Test
    public void testIsMultiBitClass() throws ClassNotFoundException {
        checkClass("core.Sha256Hash");
        checkClass("core.Transaction");
        checkClass("core.TransactionInput");
        checkClass("core.TransactionOutput");
        checkClass("core.Wallet");
        checkClass("store.WalletExtensionSerializer");
        checkClass("store.WalletProtobufSerializer");
    }

    private void checkClass(String className) throws ClassNotFoundException {
        Class<? extends Object> clazz = Class.forName(GOOGLE_PREFIX + className);

        Class<? extends Object>[] interfaces = clazz.getInterfaces();
        if (interfaces == null) {
            fail("Class " + clazz + " does not implement isMultiBitClass marker interface");
        } else {
            boolean success = false;
            for (int i = 0; i < interfaces.length; i++) {
                if (interfaces[i].getName().equals("org.multibit.IsMultiBitClass")) {
                    success = true;
                    break;
                }
            }
            if (success) {
                // carry on
            } else {
                fail("Class " + clazz + " does not implement isMultiBitClass marker interface");
            }
        }

    }
}
