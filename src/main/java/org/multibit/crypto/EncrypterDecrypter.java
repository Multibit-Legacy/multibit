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

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.crypto.BufferedBlockCipher;
import org.bouncycastle.crypto.CipherParameters;
import org.bouncycastle.crypto.PBEParametersGenerator;
import org.bouncycastle.crypto.engines.AESFastEngine;
import org.bouncycastle.crypto.generators.OpenSSLPBEParametersGenerator;
import org.bouncycastle.crypto.modes.CBCBlockCipher;
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.bouncycastle.crypto.params.ParametersWithIV;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Utils;

/**
 * This class encrypts and decrypts a string in a manner that is compatible with
 * OpenSSL
 * 
 * If you encrypt a string with this class you can decrypt it with the OpenSSL
 * command: openssl enc -d -aes-256-cbc -a -in cipher.txt -out plain.txt -pass
 * pass:aTestPassword
 * 
 * where: cipher.txt = file containing the cipher text plain.txt - where you
 * want the plaintext to be saved
 * 
 * substitute your password for "aTestPassword" or remove the "-pass" parameter
 * to be prompted
 * 
 * @author jim
 * 
 */
public class EncrypterDecrypter {
    private static final Logger log = LoggerFactory.getLogger(EncrypterDecrypter.class);

    /**
     * The string encoding to use when converting strings to bytes
     */
    public static final String STRING_ENCODING = "UTF-8";

    /**
     * number of times the password & salt are hashed during key creation
     */
    private static final int NUMBER_OF_ITERATIONS = 1024;

    /**
     * key length
     */
    private static final int KEY_LENGTH = 256;

    /**
     * initialization vector length
     */
    private static final int IV_LENGTH = 128;

    /**
     * the length of the salt
     */
    private static final int SALT_LENGTH = 8;

    /**
     * OpenSSL salted prefix text
     */
    public static final String OPENSSL_SALTED_TEXT = "Salted__";

    /**
     * OpenSSL salted prefix bytes - also used as magic number for encrypted
     * key file
     */
    public byte[] openSSLSaltedBytes;

    /**
     * Magic text that appears at the beginning of every OpenSSL encrypted file.
     * Used in identifying encrypted key files.
     */
    private String openSSLMagicText = null;

    public static final int NUMBER_OF_CHARACTERS_TO_MATCH_IN_OPENSSL_MAGIC_TEXT = 10;

    private static SecureRandom secureRandom = new SecureRandom();

    public EncrypterDecrypter() {
        try {
            openSSLSaltedBytes = OPENSSL_SALTED_TEXT.getBytes(STRING_ENCODING);

            openSSLMagicText = Base64.encodeBase64String(
                    EncrypterDecrypter.OPENSSL_SALTED_TEXT.getBytes(EncrypterDecrypter.STRING_ENCODING)).substring(0,
                    EncrypterDecrypter.NUMBER_OF_CHARACTERS_TO_MATCH_IN_OPENSSL_MAGIC_TEXT);

        } catch (UnsupportedEncodingException e) {
            throw new EncrypterDecrypterException("Could not construct EncrypterDecrypter", e);
        }
    }

    /**
     * Get password and generate key and iv
     * 
     * @param password
     *            The password to use in key generation
     * @param salt
     *            The salt to use in key generation
     * @return The CipherParameters containing the created key
     * @throws Exception
     */
    private CipherParameters getAESPasswordKey(char[] password, byte[] salt) throws EncrypterDecrypterException {
        try {
            PBEParametersGenerator generator = new OpenSSLPBEParametersGenerator();
            generator.init(PBEParametersGenerator.PKCS5PasswordToBytes(password), salt, NUMBER_OF_ITERATIONS);

            ParametersWithIV key = (ParametersWithIV) generator.generateDerivedParameters(KEY_LENGTH, IV_LENGTH);

            return key;
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not generate key from password of length " + password.length
                    + " and salt '" + Utils.bytesToHexString(salt), e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits
     * 
     * @param plainText
     *            The text to encrypt
     * @param password
     *            The password to use for encryption
     * @return The encrypted string
     * @throws EncrypterDecrypterException
     */
    public String encrypt(String plainText, char[] password) throws EncrypterDecrypterException {
        try {
            // generate salt - each encryption call has a different salt
            byte[] salt = new byte[SALT_LENGTH];
            secureRandom.nextBytes(salt);

            byte[] plainTextAsBytes;
            if (plainText == null) {
                plainTextAsBytes = new byte[0];
            } else {
                plainTextAsBytes = plainText.getBytes(STRING_ENCODING);
            }

            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(password, salt);

            // The following code uses an AES cipher to encrypt the message
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(true, key);
            byte[] encryptedBytes = new byte[cipher.getOutputSize(plainTextAsBytes.length)];
            int length = cipher.processBytes(plainTextAsBytes, 0, plainTextAsBytes.length, encryptedBytes, 0);

            cipher.doFinal(encryptedBytes, length);

            // OpenSSL adds the salt to the encrypted result as "Salted___" +
            // salt in hex.
            // Do the same

            byte result[] = concat(concat(openSSLSaltedBytes, salt), encryptedBytes);

            return Base64.encodeBase64String(result);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not encrypt string '" + plainText + "'", e);
        }
    }

    /**
     * Decrypt text previously encrypted with this class
     * 
     * @param textToDecode
     *            The code to decrypt
     * @param passwordbThe
     *            password to use for decryption
     * @return The decrypted text
     * @throws EncrypterDecrypterException
     */
    public String decrypt(String textToDecode, char[] password) throws EncrypterDecrypterException {
        try {
            final byte[] decodeTextAsBytes = Base64.decodeBase64(textToDecode.getBytes(STRING_ENCODING));

            // extract the salt and bytes to decrypt
            int saltPrefixTextLength = openSSLSaltedBytes.length + SALT_LENGTH;

            byte[] prefixedSalt = new byte[saltPrefixTextLength];

            System.arraycopy(decodeTextAsBytes, 0, prefixedSalt, 0, saltPrefixTextLength);

            byte[] salt = new byte[SALT_LENGTH];

            System.arraycopy(prefixedSalt, openSSLSaltedBytes.length, salt, 0, SALT_LENGTH);

            byte[] cipherBytes = new byte[decodeTextAsBytes.length - saltPrefixTextLength];
            System.arraycopy(decodeTextAsBytes, saltPrefixTextLength, cipherBytes, 0, decodeTextAsBytes.length
                    - saltPrefixTextLength);

            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(password, salt);

            // decrypt the message
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(false, key);

            byte[] decryptedBytes = new byte[cipher.getOutputSize(cipherBytes.length)];
            int length = cipher.processBytes(cipherBytes, 0, cipherBytes.length, decryptedBytes, 0);

            cipher.doFinal(decryptedBytes, length);

            // reconstruct the original string, trimming off any whitespace
            // added by block padding
            String decryptedText = new String(decryptedBytes, STRING_ENCODING).trim();
            return decryptedText;
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not decrypt input string", e);
        }
    }

    /**
     * Concatenate two byte arrays
     */
    private byte[] concat(byte[] arrayA, byte[] arrayB) {
        byte[] result = new byte[arrayA.length + arrayB.length];
        System.arraycopy(arrayA, 0, result, 0, arrayA.length);
        System.arraycopy(arrayB, 0, result, arrayA.length, arrayB.length);

        return result;
    }

    /**
     * get the OpenSSL "Salted__" prefix text as bytes
     * 
     * @return The openSSL salted prefix bytes
     */
    public byte[] getOpenSSLSaltedBytes() {
        return openSSLSaltedBytes;
    }

    /**
     * Get the magic text that starts every OpenSSL encrypted key file
     * 
     * @return
     */
    public String getOpenSSLMagicText() {
        return openSSLMagicText;
    }

}