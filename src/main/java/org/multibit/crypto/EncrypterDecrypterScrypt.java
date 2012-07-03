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

import java.security.SecureRandom;

import org.apache.commons.codec.binary.Base64;
import org.spongycastle.crypto.BufferedBlockCipher;
import org.spongycastle.crypto.CipherParameters;
import org.spongycastle.crypto.engines.AESFastEngine;
import org.spongycastle.crypto.modes.CBCBlockCipher;
import org.spongycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.params.ParametersWithIV;

import com.google.bitcoin.core.Utils;
import com.lambdaworks.crypto.SCrypt;

/**
 * This class encrypts and decrypts byte arrays or string using scrypt as the KDF and 
 * AES for the encryption.
 * 
 * @author jim
 * 
 */
public class EncrypterDecrypterScrypt {
    /**
     * The string encoding to use when converting strings to bytes.
     */
    public static final String STRING_ENCODING = "UTF-8";

    /**
     * Key length.
     */
    private static final int KEY_LENGTH = 32; // bytes = 256 bits.

    /**
     * The length of the salt.
     * The salt is the same length as the IV i.e. the same size of an AES block
     */
    private static final int SALT_LENGTH = 16;  // bytes = 128 bits.

    private static SecureRandom secureRandom = new SecureRandom();

    private int n;
    
    private int r;
    
    private int p;
    
    public EncrypterDecrypterScrypt() {
        n = 16384;
        r = 8;
        p = 1;
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
    private ParametersWithIV getAESPasswordKey(byte[] passwordBytes, byte[] salt) throws EncrypterDecrypterException {
        try {
            byte[] keyBytes = SCrypt.scrypt(passwordBytes, salt, n, r, p, KEY_LENGTH);
            
            CipherParameters params = new KeyParameter(keyBytes);
            ParametersWithIV key = new ParametersWithIV(params, salt);
            return key;
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not generate key from password bytes of length " + passwordBytes.length
                    + " and salt '" + Utils.bytesToHexString(salt), e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits.
     * 
     * @param plainText
     *            The text to encrypt
     * @param passwordBytes
     *            The password to use for encryption
     * @return The encrypted string
     * @throws EncrypterDecrypterException
     */
    public String encrypt(String plainText, byte[] passwordBytes) throws EncrypterDecrypterException {
        try {
            byte[] plainTextAsBytes;
            if (plainText == null) {
                plainTextAsBytes = new byte[0];
            } else {
                plainTextAsBytes = plainText.getBytes(STRING_ENCODING);
            }
            
            byte[] encryptedBytes = encrypt(plainTextAsBytes, passwordBytes);     
            
            return Base64.encodeBase64String(encryptedBytes);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not encrypt string '" + plainText + "'", e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits.
     * 
     * @param plainBytes
     *            The bytes to encrypt
     * @param passwordBytes
     *            The password to use for encryption
     * @return SALT_LENGTH bytes of salt followed by the encrypted bytes.
     * @throws EncrypterDecrypterException
     */
    public byte[] encrypt(byte[] plainTextAsBytes, byte[] passwordBytes) throws EncrypterDecrypterException {
        try {
            // Generate salt - each encryption call has a different salt.
            byte[] salt = new byte[SALT_LENGTH];
            secureRandom.nextBytes(salt);
 
            ParametersWithIV key = getAESPasswordKey(passwordBytes, salt);

            // The following code uses an AES cipher to encrypt the message.
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(true, key);
            byte[] encryptedBytes = new byte[cipher.getOutputSize(plainTextAsBytes.length)];
            int length = cipher.processBytes(plainTextAsBytes, 0, plainTextAsBytes.length, encryptedBytes, 0);

            cipher.doFinal(encryptedBytes, length);

            // The result bytes are the SALT_LENGTH bytes followed by the encrypted bytes.
            return concat(salt, encryptedBytes);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not encrypt bytes '" + Utils.bytesToHexString(plainTextAsBytes) + "'", e);
        }
    }

    /**
     * Decrypt text previously encrypted with this class.
     * 
     * @param textToDecode
     *            The code to decrypt
     * @param passwordBytes
     *            The password to use for decryption
     * @return The decrypted text
     * @throws EncrypterDecrypterException
     */
    public String decrypt(String textToDecode, byte[] passwordBytes) throws EncrypterDecrypterException {
        try {
            final byte[] decodeTextAsBytes = Base64.decodeBase64(textToDecode.getBytes(STRING_ENCODING));
            byte[] decryptedBytes = decrypt(decodeTextAsBytes, passwordBytes);
            
            return new String(decryptedBytes, STRING_ENCODING).trim();
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not decrypt input string", e); 
        }
    }

    /**
     * Decrypt bytes previously encrypted with this class.
     * 
     * @param bytesToDecode
     *            The bytes to decrypt
     * @param passwordBytes
     *            The password to use for decryption
     * @return The decrypted bytes
     * @throws EncrypterDecrypterException
     */
    public byte[] decrypt(byte[] bytesToDecode, byte[] passwordBytes) throws EncrypterDecrypterException {
        try {
            // separate the salt and bytes to decrypt
            byte[] salt = new byte[SALT_LENGTH];

            System.arraycopy(bytesToDecode, 0, salt, 0, SALT_LENGTH);

            byte[] cipherBytes = new byte[bytesToDecode.length - SALT_LENGTH];
            System.arraycopy(bytesToDecode, SALT_LENGTH, cipherBytes, 0, bytesToDecode.length - SALT_LENGTH);

            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(passwordBytes, salt);

            // decrypt the message
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(false, key);

            byte[] decryptedBytes = new byte[cipher.getOutputSize(cipherBytes.length)];
            int length = cipher.processBytes(cipherBytes, 0, cipherBytes.length, decryptedBytes, 0);

            cipher.doFinal(decryptedBytes, length);

            return decryptedBytes;
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not decrypt input string", e);
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
}