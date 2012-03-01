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

import org.junit.Test;

import com.google.bitcoin.core.Utils;

public class EncrypterDecrypterTest extends TestCase {

    private static final String TEST_STRING1 = "The quick brown fox jumps over the lazy dog. 01234567890 !@#$%^&*()-=[]{};':|`~,./<>?";
    private static final char[] PASSWORD1 = "aTestPassword".toCharArray();
    private static final char[] PASSWORD2 = "0123456789".toCharArray();

    @Test
    public void testEncryptDecrypt1() throws EncrypterDecrypterException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // encrypt
        String cipherText = encrypterDecrypter.encrypt(TEST_STRING1, PASSWORD1);
        assertNotNull(cipherText);
        System.out.println("EncrypterDecrypterTest: cipherText = \n---------------\n" + cipherText + "\n---------------\n");

        // decrypt
        String reconstructedPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD1);
        // System.out.println("Original: " +
        // Utils.bytesToHexString(TEST_STRING1.getBytes()));
        // System.out.println("Reborn  : " +
        // Utils.bytesToHexString(reconstructedPlainText.getBytes()));
        assertEquals(TEST_STRING1, reconstructedPlainText);
    }

    public void testEncryptDecrypt2() throws EncrypterDecrypterException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // create a longer encryption string
        StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < 1000; i++) {
            stringBuffer.append(i + " ").append(TEST_STRING1);
        }

        System.out.println("EncrypterDecrypterTest: String to encrypt has length " + stringBuffer.toString().length());
        String cipherText = encrypterDecrypter.encrypt(stringBuffer.toString(), PASSWORD2);

        assertNotNull(cipherText);
        System.out.println("EncrypterDecrypterTest: CipherText has length " + cipherText.length());

        String reconstructedPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD2);
        // System.out.println("Original: " +
        // Utils.bytesToHexString(stringBuffer.toString().getBytes()));
        // System.out.println("Reborn  : " +
        // Utils.bytesToHexString(reconstructedPlainText.getBytes()));
        assertEquals(stringBuffer.toString(), reconstructedPlainText);
    }
}
