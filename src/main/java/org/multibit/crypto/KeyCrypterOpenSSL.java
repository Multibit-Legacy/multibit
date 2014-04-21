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

import com.google.bitcoin.core.Utils;
import com.google.bitcoin.crypto.KeyCrypterException;
import org.apache.commons.codec.binary.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.BufferedBlockCipher;
import org.spongycastle.crypto.CipherParameters;
import org.spongycastle.crypto.PBEParametersGenerator;
import org.spongycastle.crypto.engines.AESFastEngine;
import org.spongycastle.crypto.generators.OpenSSLPBEParametersGenerator;
import org.spongycastle.crypto.modes.CBCBlockCipher;
import org.spongycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.spongycastle.crypto.params.ParametersWithIV;

import java.io.UnsupportedEncodingException;
import java.security.SecureRandom;
import java.util.Arrays;

/**
 * This class encrypts and decrypts a string in a manner that is compatible with
 * OpenSSL.
 * 
 * If you encrypt a string with this class you can decrypt it with the OpenSSL
 * command: openssl enc -d -aes-256-cbc -a -in cipher.txt -out plain.txt -pass
 * pass:aTestPassword
 * 
 * where: cipher.txt = file containing the cipher text plain.txt - where you
 * want the plaintext to be saved
 * 
 * substitute your password for "aTestPassword" or remove the "-pass" parameter
 * to be prompted.
 * 
 * @author jim
 * 
 */
public class KeyCrypterOpenSSL {
    private Logger log = LoggerFactory.getLogger(KeyCrypterOpenSSL.class);

    /**
     * The string encoding to use when converting strings to bytes
     */
    public static final String STRING_ENCODING = "UTF-8";

    /**
     * number of times the password & salt are hashed during key creation.
     */
    private static final int NUMBER_OF_ITERATIONS = 1024;

    /**
     * Key length.
     */
    private static final int KEY_LENGTH = 256;

    /**
     * Initialization vector length.
     */
    private static final int IV_LENGTH = 128;

    /**
     * The length of the salt.
     */
    private static final int SALT_LENGTH = 8;

    /**
     * OpenSSL salted prefix text.
     */
    public static final String OPENSSL_SALTED_TEXT = "Salted__";

    /**
     * OpenSSL salted prefix bytes - also used as magic number for encrypted
     * key file.
     */
    public byte[] openSSLSaltedBytes;

    /**
     * Magic text that appears at the beginning of every OpenSSL encrypted file.
     * Used in identifying encrypted key files.
     */
    private String openSSLMagicText = null;

    public static final int NUMBER_OF_CHARACTERS_TO_MATCH_IN_OPENSSL_MAGIC_TEXT = 10;

    private static SecureRandom secureRandom = new SecureRandom();

    public KeyCrypterOpenSSL()  {
        try {
            openSSLSaltedBytes = OPENSSL_SALTED_TEXT.getBytes(STRING_ENCODING);

            openSSLMagicText = Base64.encodeBase64String(
                    KeyCrypterOpenSSL.OPENSSL_SALTED_TEXT.getBytes(KeyCrypterOpenSSL.STRING_ENCODING)).substring(0,
                    KeyCrypterOpenSSL.NUMBER_OF_CHARACTERS_TO_MATCH_IN_OPENSSL_MAGIC_TEXT);

        } catch (UnsupportedEncodingException e) {
            log.error("Could not construct EncrypterDecrypter", e.getMessage());
        }
    }

    /**
     * Get password and generate key and iv.
     * 
     * @param password
     *            The password to use in key generation
     * @param salt
     *            The salt to use in key generation
     * @return The CipherParameters containing the created key
     * @throws Exception
     */
    private CipherParameters getAESPasswordKey(CharSequence password, byte[] salt) throws KeyCrypterException {
        try {
            PBEParametersGenerator generator = new OpenSSLPBEParametersGenerator();
            generator.init(PBEParametersGenerator.PKCS5PasswordToBytes(convertToCharArray(password)), salt, NUMBER_OF_ITERATIONS);

            ParametersWithIV key = (ParametersWithIV) generator.generateDerivedParameters(KEY_LENGTH, IV_LENGTH);

            return key;
        } catch (Exception e) {
            throw new KeyCrypterException("Could not generate key from password of length " + password.length()
                    + " and salt '" + Utils.bytesToHexString(salt), e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits.
     * 
     * @param plainText
     *            The text to encrypt
     * @param password
     *            The password to use for encryption
     * @return The encrypted string
     * @throws KeyCrypterException
     */
    public String encrypt(String plainText, CharSequence password) throws KeyCrypterException {
        try {
            byte[] plainTextAsBytes;
            if (plainText == null) {
                plainTextAsBytes = new byte[0];
            } else {
                plainTextAsBytes = plainText.getBytes(STRING_ENCODING);
            }
            
            byte[] encryptedBytes = encrypt(plainTextAsBytes, password);     
            
            // OpenSSL prefixes the salt bytes + encryptedBytes with Salted___ and then base64 encodes it
            byte[] encryptedBytesPlusSaltedText = concat(openSSLSaltedBytes, encryptedBytes);
            
            return Base64.encodeBase64String(encryptedBytesPlusSaltedText);
        } catch (Exception e) {
            throw new KeyCrypterException("Could not encrypt string '" + plainText + "'", e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits.
     * 
     * @param plainTextAsBytes
     *            The bytes to encrypt
     * @param password
     *            The password to use for encryption
     * @return SALT_LENGTH bytes of salt followed by the encrypted bytes.
     * @throws KeyCrypterException
     */
    public byte[] encrypt(byte[] plainTextAsBytes, CharSequence password) throws KeyCrypterException {
        try {
            // Generate salt - each encryption call has a different salt.
            byte[] salt = new byte[SALT_LENGTH];
            secureRandom.nextBytes(salt);
 
            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(password, salt);

            // The following code uses an AES cipher to encrypt the message.
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(true, key);
            byte[] encryptedBytes = new byte[cipher.getOutputSize(plainTextAsBytes.length)];
            final int processLength = cipher.processBytes(plainTextAsBytes, 0, plainTextAsBytes.length, encryptedBytes, 0);
            final int doFinalLength = cipher.doFinal(encryptedBytes, processLength);

            // The result bytes are the SALT_LENGTH bytes followed by the encrypted bytes.
            return concat(salt, Arrays.copyOf(encryptedBytes, processLength + doFinalLength));
        } catch (Exception e) {
            throw new KeyCrypterException("Could not encrypt bytes '" + Utils.bytesToHexString(plainTextAsBytes) + "'", e);
        }
    }

    /**
     * Decrypt text previously encrypted with this class.
     * 
     * @param textToDecode
     *            The code to decrypt
     * @param password THe password to use
     *            password to use for decryption
     * @return The decrypted text
     * @throws KeyCrypterException
     */
    public String decrypt(String textToDecode, CharSequence password) throws KeyCrypterException {
        try {
            final byte[] decodeTextAsBytes = Base64.decodeBase64(textToDecode.getBytes(STRING_ENCODING));
            
            // Strip off the bytes due to the OPENSSL_SALTED_TEXT prefix text.
            int saltPrefixTextLength = openSSLSaltedBytes.length;
            
            byte[] cipherBytes = new byte[decodeTextAsBytes.length - saltPrefixTextLength];
            System.arraycopy(decodeTextAsBytes, saltPrefixTextLength, cipherBytes, 0, decodeTextAsBytes.length
                    - saltPrefixTextLength);

            byte[] decryptedBytes = decrypt(cipherBytes, password);
            
            return new String(decryptedBytes, STRING_ENCODING).trim();
        } catch (Exception e) {
            throw new KeyCrypterException("Could not decrypt input string", e); 
        }
    }

    /**
     * Decrypt bytes previously encrypted with this class.
     * 
     * @param bytesToDecode
     *            The bytes to decrypt
     * @param password The password to use
     *            password to use for decryption
     * @return The decrypted bytes
     * @throws KeyCrypterException
     */
    public byte[] decrypt(byte[] bytesToDecode, CharSequence password) throws KeyCrypterException {
        try {
            // separate the salt and bytes to decrypt
            byte[] salt = new byte[SALT_LENGTH];

            System.arraycopy(bytesToDecode, 0, salt, 0, SALT_LENGTH);

            byte[] cipherBytes = new byte[bytesToDecode.length - SALT_LENGTH];
            System.arraycopy(bytesToDecode, SALT_LENGTH, cipherBytes, 0, bytesToDecode.length - SALT_LENGTH);

            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(password, salt);

            // decrypt the message
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(false, key);

            byte[] decryptedBytes = new byte[cipher.getOutputSize(cipherBytes.length)];
            final int processLength = cipher.processBytes(cipherBytes, 0, cipherBytes.length, decryptedBytes, 0);
            final int doFinalLength = cipher.doFinal(decryptedBytes, processLength);

            return Arrays.copyOf(decryptedBytes, processLength + doFinalLength);
        } catch (Exception e) {
            throw new KeyCrypterException("Could not decrypt input string", e);
        }
    }

    /**
     * Concatenate two byte arrays.
     */
    private byte[] concat(byte[] arrayA, byte[] arrayB) {
        byte[] result = new byte[arrayA.length + arrayB.length];
        System.arraycopy(arrayA, 0, result, 0, arrayA.length);
        System.arraycopy(arrayB, 0, result, arrayA.length, arrayB.length);

        return result;
    }
    
    /**
     * Convert a CharSequence (which are UTF16) into a char array.
     *
     * Note: a String.getBytes() is not used to avoid creating a String of the password in the JVM.
     */
    private char[] convertToCharArray(CharSequence charSequence) {
        if (charSequence == null) {
            return null;
        }

        char[] charArray = new char[charSequence.length()];
        for(int i = 0; i < charSequence.length(); i++) {
            charArray[i] = charSequence.charAt(i);
        }
        return charArray;
    }

    /**
     * Get the magic text that starts every OpenSSL encrypted String.
     * 
     * @return The magic text that starts every OpenSSL encrypted String
     */
    public String getOpenSSLMagicText() {
        return openSSLMagicText;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Arrays.hashCode(openSSLSaltedBytes);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        return (obj instanceof KeyCrypterOpenSSL);
    }
}