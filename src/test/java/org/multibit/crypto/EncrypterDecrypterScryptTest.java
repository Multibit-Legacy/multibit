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

import java.io.UnsupportedEncodingException;
import java.security.SecureRandom;
import java.util.Random;
import java.util.UUID;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Utils;
import com.google.bitcoin.utils.BriefLogFormatter;

public class EncrypterDecrypterScryptTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(EncrypterDecrypterScryptTest.class);
    
    // Nonsense bytes for encryption test.
    private static final byte[] TEST_BYTES1 = new byte[]{0, -101, 2, 103, -4, 105, 6, 107, 8, -109, 10, 111, -12, 113, 14, -115, 16, 117, -18, 119, 20, 121, 22, 123, -24, 125, 26, 127, -28, 29, -30, 31};

    private static char[] PASSWORD1 = "aTestPassword".toCharArray();
    private static char[] PASSWORD2 = "0123456789".toCharArray();

    private static char[] WRONG_PASSWORD = "thisIsTheWrongPassword".toCharArray();

    // Moscow in Russian in Cyrillic.
    private static char[] PASSWORD3 = "\u041c\u043e\u0441\u043a\u0432\u0430".toCharArray();
    
    private SecureRandom secureRandom;
    private ScryptParameters scryptParameters;

    @Before
    public void setUp() throws Exception {
        secureRandom = new SecureRandom();
        
        byte[] salt = new byte[ScryptParameters.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        scryptParameters = new ScryptParameters(salt);

        BriefLogFormatter.init();
    }

    @Test
    public void testEncryptDecryptGood1() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);

        // Encrypt.
        EncryptedPrivateKey encryptedPrivateKey = encrypterDecrypter.encrypt(TEST_BYTES1, encrypterDecrypter.deriveKey(PASSWORD1));
        assertNotNull(encryptedPrivateKey);

        // Decrypt.
        byte[] reborn = encrypterDecrypter.decrypt(encryptedPrivateKey, encrypterDecrypter.deriveKey(PASSWORD1));
        log.debug("Original: " + Utils.bytesToHexString(TEST_BYTES1));
        log.debug("Reborn  : " + Utils.bytesToHexString(reborn));
        assertEquals(Utils.bytesToHexString(TEST_BYTES1), Utils.bytesToHexString(reborn));
    }

    /**
     * Test with random plain text strings and random passwords.
     * UUIDs are used and hence will only cover hex characters (and te separator hyphen).
     * @throws EncrypterDecrypterException
     * @throws UnsupportedEncodingException 
     */
    public void testEncryptDecryptGood3() throws EncrypterDecrypterException, UnsupportedEncodingException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);

        int numberOfTests = 16;
        System.out.print("EncrypterDecrypterTest: Trying random UUIDs for plainText and passwords :");
        for (int i = 0; i < numberOfTests; i++) {
            // Create a UUID as the plaintext and use another for the password.
            String plainText = UUID.randomUUID().toString();
            char[] password = UUID.randomUUID().toString().toCharArray();

            EncryptedPrivateKey encryptedPrivateKey = encrypterDecrypter.encrypt(plainText.getBytes(), encrypterDecrypter.deriveKey(password));

            assertNotNull(encryptedPrivateKey);

            byte[] reconstructedPlainBytes = encrypterDecrypter.decrypt(encryptedPrivateKey,encrypterDecrypter.deriveKey(password));
            assertEquals(Utils.bytesToHexString(plainText.getBytes()), Utils.bytesToHexString(reconstructedPlainBytes));
            System.out.print('.');
        }
        System.out.println(" Done.");
    }

    public void testEncryptDecryptWrongPassword() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);

        // create a longer encryption string
        StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < 100; i++) {
            stringBuffer.append(i + " ").append("The quick brown fox");
        }

        EncryptedPrivateKey encryptedPrivateKey = encrypterDecrypter.encrypt(stringBuffer.toString().getBytes(), encrypterDecrypter.deriveKey(PASSWORD2));
        assertNotNull(encryptedPrivateKey);

        try {
            encrypterDecrypter.decrypt(encryptedPrivateKey, encrypterDecrypter.deriveKey(WRONG_PASSWORD));
            fail("Decrypt with wrong password did not throw exception");
        } catch (EncrypterDecrypterException ede) {
            assertTrue(ede.getMessage().indexOf("Could not decrypt") > -1);
        }
    }
    
    @Test
    public void testEncryptDecryptBytes1() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);

        // Encrypt bytes.
        EncryptedPrivateKey encryptedPrivateKey = encrypterDecrypter.encrypt(TEST_BYTES1, encrypterDecrypter.deriveKey(PASSWORD1));
        assertNotNull(encryptedPrivateKey);
        log.debug("\nEncrypterDecrypterTest: cipherBytes = \nlength = " + encryptedPrivateKey.getEncryptedBytes().length + "\n---------------\n" + Utils.bytesToHexString(encryptedPrivateKey.getEncryptedBytes()) + "\n---------------\n");

        byte[] rebornPlainBytes = encrypterDecrypter.decrypt(encryptedPrivateKey, encrypterDecrypter.deriveKey(PASSWORD1));
        
        log.debug("Original: " + Utils.bytesToHexString(TEST_BYTES1));
        log.debug("Reborn1 : " + Utils.bytesToHexString(rebornPlainBytes));
        assertEquals( Utils.bytesToHexString(TEST_BYTES1),  Utils.bytesToHexString(rebornPlainBytes));
    }
    
    @Test
    public void testEncryptDecryptBytes2() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);

        // Encrypt random bytes of various lengths up to length 50.
        Random random = new Random();
        
        for (int i = 0; i < 50; i++) {
            byte[] plainBytes = new byte[i];
            random.nextBytes(plainBytes);
            
            EncryptedPrivateKey encryptedPrivateKey = encrypterDecrypter.encrypt(plainBytes, encrypterDecrypter.deriveKey(PASSWORD1));
            assertNotNull(encryptedPrivateKey);
            //log.debug("\nEncrypterDecrypterTest: cipherBytes = \nlength = " + cipherBytes.length + "\n---------------\n" + Utils.bytesToHexString(cipherBytes) + "\n---------------\n");

            byte[] rebornPlainBytes = encrypterDecrypter.decrypt(encryptedPrivateKey, encrypterDecrypter.deriveKey(PASSWORD1));
            
            log.debug("Original: (" + i + ") " + Utils.bytesToHexString(plainBytes));
            log.debug("Reborn1 : (" + i + ") " + Utils.bytesToHexString(rebornPlainBytes));
            assertEquals( Utils.bytesToHexString(plainBytes),  Utils.bytesToHexString(rebornPlainBytes));
        }
    }
}
