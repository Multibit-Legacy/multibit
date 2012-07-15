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
package org.multibit.crypto;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

import com.google.bitcoin.core.Utils;
import com.google.bitcoin.utils.BriefLogFormatter;

public class EncryptableECKeyTest extends TestCase {
  
    private EncryptableECKey myKey;

    private static char[] PASSWORD1 = "aTestPassword".toCharArray();
    
    @Before
    public void setUp() throws Exception {
        myKey = new EncryptableECKey();

        BriefLogFormatter.init();
    }

    @Test
    public void testBasic() throws Exception {
        // The key should initially be unencrypted
        assertTrue("Key not unencrypted at start",  !myKey.isEncrypted());
        
        // Copy the private key bytes for checking later.
        byte[] originalPrivateKeyBytes = new byte[32];
        System.arraycopy(myKey.getPrivKeyBytes(), 0, originalPrivateKeyBytes, 0, 32);
        System.out.println("EncryptableECKeyTest - Original private key = " + Utils.bytesToHexString(originalPrivateKeyBytes));
        
        // Encrypt the key.
        myKey.encrypt(PASSWORD1);
        
        // The key should now be encrypted.
        assertTrue("Key is not encrypted but it should be", myKey.isEncrypted());
        
        // The unencrypted private key bytes of the encrypted keychain should all be blank.
        byte[] privateKeyBytes = myKey.getPrivKeyBytes();
        for (int i = 0; i < privateKeyBytes.length; i++) {
            assertEquals("Byte " + i + " of the private key was not zero but should be", 0, privateKeyBytes[i]);
        }
        
        // Decrypt the key.
        myKey.decrypt(PASSWORD1);

        // The key should be unencrypted
        assertTrue("Key is not unencrypted but it should be", !myKey.isEncrypted());

        // The reborn unencrypted private key bytes should match the original private key.
        privateKeyBytes = myKey.getPrivKeyBytes();
        System.out.println("EncryptableECKeyTest - Reborn decrypted private key = " + Utils.bytesToHexString(privateKeyBytes));

        for (int i = 0; i < privateKeyBytes.length; i++) {
            assertEquals("Byte " + i + " of the private key did not match the original", originalPrivateKeyBytes[i], privateKeyBytes[i]);
        }
    }
}
