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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;

import junit.framework.TestCase;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Utils;

public class EncrypterDecrypterTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(EncrypterDecrypterTest.class);

    private static final String TEST_STRING1 = "The quick brown fox jumps over the lazy dog. 01234567890 !@#$%^&*()-=[]{};':|`~,./<>?";
   
    // Chinese translation for 'The fee cannot be smaller than the minimum fee
    private static final String TEST_STRING2 = "\u4ea4\u6613\u8d39\u7528\u5fc5\u987b\u81f3\u5c11 0.0001 BTC\u3002\u4fdd\u6301\u539f\u6709\u8d39\u7528\u8bbe\u7f6e\u3002";
    
    private static final char[] PASSWORD1 = "aTestPassword".toCharArray();
    private static final char[] PASSWORD2 = "0123456789".toCharArray();
    
    // Moscow in Russian in Cyrillic
    private static final char[] PASSWORD3 = "\u041c\u043e\u0441\u043a\u0432\u0430".toCharArray();

    @Test
    public void testEncryptDecrypt1() throws EncrypterDecrypterException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // encrypt
        String cipherText = encrypterDecrypter.encrypt(TEST_STRING1, PASSWORD1);
        assertNotNull(cipherText);
        log.debug("\nEncrypterDecrypterTest: cipherText = \n---------------\n" + cipherText + "\n---------------\n");

        // decrypt
        String rebornPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD1);
        log.debug("Original: " + Utils.bytesToHexString(TEST_STRING1.getBytes()));
        log.debug("Reborn  : " + Utils.bytesToHexString(rebornPlainText.getBytes()));
        assertEquals(TEST_STRING1, rebornPlainText);
    }

    public void testEncryptDecrypt2() throws EncrypterDecrypterException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // create a longer encryption string
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

    public void testEncryptJavaDecryptOpenSSL() throws EncrypterDecrypterException, IOException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // create a longer encryption string
        StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < 1000; i++) {
            stringBuffer.append(i + " ").append(TEST_STRING1);
        }

        System.out.println("EncrypterDecrypterTest: String to encrypt has length " + stringBuffer.toString().length());
        String cipherText = encrypterDecrypter.encrypt(stringBuffer.toString(), PASSWORD2);

        // Create temporary ciphertext file.
        File temporaryCipherTextFile = File.createTempFile("EncrypterDecrypterTest", ".cipher");

        // Delete temp file when program exits.
        temporaryCipherTextFile.deleteOnExit();

        // write the cipher text to a file
        writeToFile(cipherText, temporaryCipherTextFile);

        // create a file to write the decoded text to
        // Create temporary ciphertext file.
        File temporaryPlainTextFile = File.createTempFile("EncrypterDecrypterTest", ".plain");

        // Delete temp file when program exits.
        temporaryPlainTextFile.deleteOnExit();

        // run the openssl equivalent command to decrypt the ciphertext
        try {
            String commandToRun = "openssl enc -d -p -aes-256-cbc -a -in " + temporaryCipherTextFile.getAbsolutePath() 
            + " -out " + temporaryPlainTextFile.getAbsolutePath() + " -pass pass:" + new String(PASSWORD2);

            System.out.println("EncrypterDecrypterTest: Decrypting command = '" + commandToRun + "'");
            
            Process p = Runtime.getRuntime().exec(commandToRun);
            p.waitFor();
            BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
            String line = reader.readLine();
            while (line != null) {
                System.out.println("EncrypterDecrypterTest: " + line);
                line = reader.readLine();
            }
            
            // read in the decrypted text
            String rebornPlainText = readFile(temporaryPlainTextFile).trim();
           
            assertEquals(stringBuffer.toString(), rebornPlainText);

        } catch (IOException e1) {
            fail("Could not run OpenSSL command to decrypt file");
        } catch (InterruptedException e2) {
            fail("Could not run OpenSSL command to decrypt file");
        }
    }
    
    @Test
    public void testEncryptDecryptNullAndBlank() throws EncrypterDecrypterException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // encrypt null string
        String cipherText = encrypterDecrypter.encrypt(null, PASSWORD1);
        assertNotNull(cipherText);

        // decrypt
        String rebornPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD1);
        assertEquals("", rebornPlainText);
        
        // encrypt empty string
        cipherText = encrypterDecrypter.encrypt("", PASSWORD1);
        assertNotNull(cipherText);

        // decrypt
        rebornPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD1);
        assertEquals("", rebornPlainText);
    }

    @Test
    public void testEncryptDecryptInternational() throws EncrypterDecrypterException {
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();

        // encrypt
        String cipherText = encrypterDecrypter.encrypt(TEST_STRING2, PASSWORD3);
        assertNotNull(cipherText);
        log.debug("\nEncrypterDecrypterTest: cipherText = \n---------------\n" + cipherText + "\n---------------\n");

        // decrypt
        String rebornPlainText = encrypterDecrypter.decrypt(cipherText, PASSWORD3);
        log.debug("Original: " + Utils.bytesToHexString(TEST_STRING2.getBytes()));
        log.debug("Reborn  : " + Utils.bytesToHexString(rebornPlainText.getBytes()));
        assertEquals(TEST_STRING2, rebornPlainText);
    }
 
    private void writeToFile(String textToWrite, File destinationFile) throws IOException {
        if (!destinationFile.exists()) {
            destinationFile.createNewFile();
        }
        FileOutputStream fileOutputStream = null;

        try {
            fileOutputStream = new FileOutputStream(destinationFile);
            fileOutputStream.write(textToWrite.getBytes(EncrypterDecrypter.STRING_ENCODING));
        } finally {

        }
    }

    private String readFile(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        StringBuilder stringBuilder = new StringBuilder();
        String ls = System.getProperty("line.separator");
        while ((line = reader.readLine()) != null) {
            stringBuilder.append(line);
            stringBuilder.append(ls);
        }
        return stringBuilder.toString();
    }
}
