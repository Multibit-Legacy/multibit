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

import java.security.Security;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

public class EncrypterDecrypter {

    /**
     * bytes used to salt the key
     */
    private static final byte[] SALT = { (byte) 0xa2, (byte) 0x73, (byte) 0x27, (byte) 0x8c, (byte) 0xb1, (byte) 0xd8, (byte) 0x1f,
            (byte) 0xc3 };

    /**
     * number of times the password & salt are hashed during key creation
     */
    private static final int NUMBER_OF_ITERATIONS = 1024;

    /**
     * The string encoding to use when converting strings to bytes
     */
    private static final String STRING_ENCODING = "utf-8";


    static {
        Security.addProvider(new BouncyCastleProvider());        
    }

    public static final String ALGORITHM = "PBEWithSHA1And256BitAES-CBC-BC";

    public static String encrypt(final String password, final String plainText) throws EncrypterDecrypterException {
        try {
            // Create the encryption key
            final SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(ALGORITHM, "BC");
            final PBEKeySpec keySpec = new PBEKeySpec(password.toCharArray());
            final SecretKey secretKey = keyFactory.generateSecret(keySpec);

            // Encrypt the plain text
            final byte[] plainTextAsBytes = plainText.getBytes(STRING_ENCODING);
            final PBEParameterSpec cipherSpec = new PBEParameterSpec(SALT, NUMBER_OF_ITERATIONS);
            final Cipher cipher = Cipher.getInstance(ALGORITHM, "BC");
            cipher.init(Cipher.ENCRYPT_MODE, secretKey, cipherSpec);
            final byte[] encryptedBytes = cipher.doFinal(plainTextAsBytes);

            return Base64.encodeBase64String(encryptedBytes);
        } catch (final Throwable t) {
            throw new EncrypterDecrypterException(t.toString());
        }
    }

    public static String decrypt(final String password, final String cipherText) throws EncrypterDecrypterException {
        try {
            // Create the encryption key
            final SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(ALGORITHM, "BC");
            final PBEKeySpec keySpec = new PBEKeySpec(password.toCharArray());
            final SecretKey secretKey = keyFactory.generateSecret(keySpec);

            // Decrypt the cipher text
            final byte[] cipherTextAsBytes = Base64.decodeBase64(cipherText.getBytes(STRING_ENCODING));
            final PBEParameterSpec cipherSpec = new PBEParameterSpec(SALT, NUMBER_OF_ITERATIONS);
            final Cipher cipher = Cipher.getInstance(ALGORITHM, "BC");
            cipher.init(Cipher.DECRYPT_MODE, secretKey, cipherSpec);
            final byte[] decryptedBytes = cipher.doFinal(cipherTextAsBytes);

            return new String(decryptedBytes, STRING_ENCODING);
        } catch (final Throwable t) {
            throw new EncrypterDecrypterException(t.toString());
        }
    }
}