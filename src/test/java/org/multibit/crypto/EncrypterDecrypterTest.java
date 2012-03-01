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

public class EncrypterDecrypterTest  extends TestCase  {
    
    private static final String TEST_STRING1 = "The quick brown fox jumps over the lazy dog. 01234567890 !@#$%^&*()-=[]{};':|`~,./<>?";
    private static final String PASSWORD1 = "aTestPassword";
    private static final String PASSWORD2 = "0123456789";
    
    @Test
    public void testEncryptDecrypt1() throws EncrypterDecrypterException {   
       // encrypt
       String cipherText = EncrypterDecrypter.encrypt(PASSWORD1, TEST_STRING1);
       assertNotNull(cipherText);
       System.out.println("EncrypterDecrypterTest: cipherText = \n'" + cipherText + "'");
       
       // decrypt
       String reconstructedPlainText = EncrypterDecrypter.decrypt(PASSWORD1, cipherText);
       assertEquals(TEST_STRING1, reconstructedPlainText);
    }
    
    public void testEncryptDecrypt2() throws EncrypterDecrypterException {
       // create a longer encryption string
       StringBuffer stringBuffer = new StringBuffer();
       for (int i = 0; i< 1000; i++) {
           stringBuffer.append(" " + i + " ").append(TEST_STRING1);
       }
       
       System.out.println("EncrypterDecrypterTest: String to encrypt has length " + stringBuffer.toString().length());
       String  cipherText = EncrypterDecrypter.encrypt(PASSWORD2, stringBuffer.toString());

       assertNotNull(cipherText);
       System.out.println("EncrypterDecrypterTest: CipherText has length " + cipherText.length());
       
       String reconstructedPlainText = EncrypterDecrypter.decrypt(PASSWORD2, cipherText);
       assertEquals(stringBuffer.toString(), reconstructedPlainText);
    }
}
