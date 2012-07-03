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
import java.util.UUID;

import junit.framework.TestCase;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Utils;

public class EncrypterDecrypterScryptTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(EncrypterDecrypterTest.class);

    private static final String TEST_STRING1 = "The quick brown fox jumps over the lazy dog. 01234567890 !@#$%^&*()-=[]{};':|`~,./<>?";
   
    // Chinese translation for 'The fee cannot be smaller than the minimum fee
    private static final String TEST_STRING2 = "\u4ea4\u6613\u8d39\u7528\u5fc5\u987b\u81f3\u5c11 0.0001 BTC\u3002\u4fdd\u6301\u539f\u6709\u8d39\u7528\u8bbe\u7f6e\u3002";
    
    // Nonsense bytes for encryption test.
    private static final byte[] TEST_BYTES1= new byte[]{0, -101, 2, 103, -4, 105, 6, 107, 8, -109, 10, 111, -12, 113, 14, -115, 16, 117, -18, 119, 20, 121, 22, 123, -24, 125, 26, 127, -28, 29, -30, 31};

    private static byte[] PASSWORD1;
    private static byte[] PASSWORD2;

    private static byte[] WRONG_PASSWORD;

    // Moscow in Russian in Cyrillic.
    private static byte[] PASSWORD3;
    
    static {
        try {
            PASSWORD1 = "aTestPassword".getBytes("UTF-8");
            PASSWORD2 = "0123456789".getBytes("UTF-8");

            WRONG_PASSWORD = "thisIsTheWrongPassword".getBytes("UTF-8");

            // Moscow in Russian in Cyrillic.
            PASSWORD3 = "\u041c\u043e\u0441\u043a\u0432\u0430".getBytes("UTF-8");
        } catch (UnsupportedEncodingException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Test
    public void testEncryptDecryptGood1() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt();

        // Encrypt.
        String cipherText = encrypterDecrypter.encrypt(TEST_STRING1, PASSWORD1);
        assertNotNull(cipherText);
        log.debug("\nEncrypterDecrypterTest: cipherText = \n---------------\n" + cipherText + "\n---------------\n");

        // Decrypt.
        String rebornPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD1);
        log.debug("Original: " + Utils.bytesToHexString(TEST_STRING1.getBytes()));
        log.debug("Reborn  : " + Utils.bytesToHexString(rebornPlainText.getBytes()));
        assertEquals(TEST_STRING1, rebornPlainText);
    }

    public void testEncryptDecryptGood2() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt();

        // Create a longer encryption string.
        StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < 100; i++) {
            stringBuffer.append(i + " ").append(TEST_STRING1);
        }

        System.out.println("EncrypterDecrypterTest: String to encrypt has length " + stringBuffer.toString().length());
        String cipherText = encrypterDecrypter.encrypt(stringBuffer.toString(), PASSWORD2);

        assertNotNull(cipherText);
        System.out.println("EncrypterDecrypterTest: CipherText has length " + cipherText.length());

        String reconstructedPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD2);
        assertEquals(stringBuffer.toString(), reconstructedPlainText);
    }

    /**
     * Test with random plain text strings and random passwords.
     * UUIDs are used and hence will only cover hex characters (and te separator hyphen).
     * @throws EncrypterDecrypterException
     * @throws UnsupportedEncodingException 
     */
    public void testEncryptDecryptGood3() throws EncrypterDecrypterException, UnsupportedEncodingException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt();

        int numberOfTests = 16;
        System.out.print("EncrypterDecrypterTest: Trying random UUIDs for plainText and passwords :");
        for (int i = 0; i < numberOfTests; i++) {
            // Create a UUID as the plaintext and use another for the password.
            String plainText = UUID.randomUUID().toString();
            String password = UUID.randomUUID().toString();

            String cipherText = encrypterDecrypter.encrypt(plainText, password.getBytes("UTF-8"));

            assertNotNull(cipherText);

            String reconstructedPlainText = encrypterDecrypter.decrypt(cipherText,password.getBytes("UTF-8"));
            assertEquals(plainText, reconstructedPlainText);
            System.out.print('.');
        }
        System.out.println(" Done.");
    }

    public void testEncryptDecryptWrongPassword() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt();

        // create a longer encryption string
        StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < 100; i++) {
            stringBuffer.append(i + " ").append(TEST_STRING1);
        }

        String cipherText = encrypterDecrypter.encrypt(stringBuffer.toString(), PASSWORD2);
        assertNotNull(cipherText);

        try {
            encrypterDecrypter.decrypt(cipherText, WRONG_PASSWORD);
            fail("Decrypt with wrong password did not throw exception");
        } catch (EncrypterDecrypterException ede) {
            assertTrue(ede.getMessage().indexOf("Could not decrypt") > -1);
        }
    }

    @Test
    public void testEncryptDecryptInternational() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt();

        // Encrypt.
        String cipherText = encrypterDecrypter.encrypt(TEST_STRING2, PASSWORD3);
        assertNotNull(cipherText);
        log.debug("\nEncrypterDecrypterTest: cipherText = \n---------------\n" + cipherText + "\n---------------\n");

        // Decrypt.
        String rebornPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD3);
        log.debug("Original: " + Utils.bytesToHexString(TEST_STRING2.getBytes()));
        log.debug("Reborn  : " + Utils.bytesToHexString(rebornPlainText.getBytes()));
        assertEquals(TEST_STRING2, rebornPlainText);
    }
    
    @Test
    public void testEncryptDecryptBytes1() throws EncrypterDecrypterException {
        EncrypterDecrypterScrypt encrypterDecrypter = new EncrypterDecrypterScrypt();

        // Encrypt bytes.
        byte[] cipherBytes = encrypterDecrypter.encrypt(TEST_BYTES1, PASSWORD1);
        assertNotNull(cipherBytes);
        log.debug("\nEncrypterDecrypterTest: cipherBytes = \nlength = " + cipherBytes.length + "\n---------------\n" + Utils.bytesToHexString(cipherBytes) + "\n---------------\n");

        // Decrypt bytes. Note the result is zero padded to a cipher block length so you have to truncate it to your expected length.
        byte[] rebornPlainBytes = encrypterDecrypter.decrypt(cipherBytes, PASSWORD1);
        
        byte[] truncatedRebornBytes = new byte[TEST_BYTES1.length];
        System.arraycopy(rebornPlainBytes, 0, truncatedRebornBytes, 0, TEST_BYTES1.length);

        log.debug("Original: " + Utils.bytesToHexString(TEST_BYTES1));
        log.debug("Reborn1 : " + Utils.bytesToHexString(rebornPlainBytes));
        log.debug("Reborn2 : " + Utils.bytesToHexString(truncatedRebornBytes));
        assertEquals( Utils.bytesToHexString(TEST_BYTES1),  Utils.bytesToHexString(truncatedRebornBytes));
    }
}
