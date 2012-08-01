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

import java.io.Serializable;
import java.security.SecureRandom;

import org.apache.commons.codec.binary.Base64;
import org.spongycastle.crypto.BufferedBlockCipher;
import org.spongycastle.crypto.engines.AESFastEngine;
import org.spongycastle.crypto.modes.CBCBlockCipher;
import org.spongycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.params.ParametersWithIV;

import com.google.bitcoin.core.Utils;
import com.lambdaworks.crypto.SCrypt;

/**
 * This class encrypts and decrypts byte arrays and strings using scrypt as the KDF and 
 * AES for the encryption.
 * 
 * The format of the encrypted byte data is a byte[] containing:
 *    Initialisation vector     BLOCK_LENGTH bytes    The initialisation vector (also used as the salt).
 *    final block length        1 byte                Cast to int for the number of bytes used in the last block (to enable pad removal).
 *    encrypted data            any length of bytes   The encrypted data.
 * 
 * String data is encrypted by:
 *    1) Converting the String to bytes using the STRING_ENCODING.
 *    2) Encrypting the resultant bytes as above.
 *    3) Converting the encrypted bytes to a String using Base64.
 *    
 * @author jim
 * 
 */
public class EncrypterDecrypterScrypt implements EncrypterDecrypter, Serializable {

    private static final long serialVersionUID = 949662512049152670L;

    /**
     * The string encoding to use when converting strings to bytes.
     */
    public static final String STRING_ENCODING = "UTF-8";

    /**
     * Key length in bytes.
     */
    public static final int KEY_LENGTH = 32; // = 256 bits.

    /**
     * The size of an AES block in bytes.
     * This is also the length of the initialisation vector.
     */
    public static final int BLOCK_LENGTH = 16;  // = 128 bits.

    transient private static SecureRandom secureRandom = new SecureRandom();

    // Scrypt parameters.
    transient private ScryptParameters scryptParameters;
    
    public EncrypterDecrypterScrypt() {
        
    }
    
    /**
     * Encryption/ Decryption using specified parameters.
     * 
     * @param scryptParameters ScryptParameters to use
     */
    public EncrypterDecrypterScrypt(ScryptParameters scryptParameters) {
        this.scryptParameters = scryptParameters;
    }

    /**
     * Generate key.
     * 
     * @param password    The password to use in key generation
     * @return            The KeyParameter containing the created key
     * @throws            EncrypterDecrypterException
     */
    private KeyParameter getAESPasswordKey(char[] password) throws EncrypterDecrypterException {
        try {
            byte[] passwordBytes = convertToByteArray(password); 
            byte[] keyBytes = SCrypt.scrypt(passwordBytes, scryptParameters.getSalt(), scryptParameters.getN(), scryptParameters.getR(), scryptParameters.getP(), KEY_LENGTH);
            return new KeyParameter(keyBytes);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not generate key from password and salt '" + Utils.bytesToHexString(scryptParameters.getSalt()), e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits.
     * 
     * @param plainText        The text to encrypt
     * @param password         The password to use for encryption
     * @return                 The encrypted string
     * @throws                 EncrypterDecrypterException
     */
    @Override
    public String encrypt(String plainText, char[] password) throws EncrypterDecrypterException {
        try {
            byte[] plainTextAsBytes;
            if (plainText == null) {
                plainTextAsBytes = new byte[0];
            } else {
                plainTextAsBytes = plainText.getBytes(STRING_ENCODING);
            }
            
            byte[] encryptedBytes = encrypt(plainTextAsBytes, password);     
            
            return Base64.encodeBase64String(encryptedBytes);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not encrypt string '" + plainText + "'", e);
        }
    }

    /**
     * Password based encryption using AES - CBC 256 bits.
     * 
     * @param plain             The bytes to encrypt
     * @param passwordBytes     The password to use for encryption
     * @return                  IV_LENGTH bytes of iv, followed by one byte indicating final block length, followed by the encrypted bytes.
     * @throws                  EncrypterDecrypterException
     */
    @Override
    public byte[] encrypt(byte[] plainBytes, char[] password) throws EncrypterDecrypterException {
        try {
            // Generate iv - each encryption call has a different iv.
            byte[] iv = new byte[BLOCK_LENGTH];
            secureRandom.nextBytes(iv);
 
            KeyParameter key = getAESPasswordKey(password);
            ParametersWithIV keyWithIv = new ParametersWithIV(key, iv);

            // Encrypt using AES.
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(true, keyWithIv);
            byte[] encryptedBytes = new byte[cipher.getOutputSize(plainBytes.length)];
            int length = cipher.processBytes(plainBytes, 0, plainBytes.length, encryptedBytes, 0);

            cipher.doFinal(encryptedBytes, length);

            // Work out how many bytes in the last block are real data as opposed to padding
            int finalBlockLength = plainBytes.length % BLOCK_LENGTH;
            
            // The result bytes are the BLOCK_LENGTH iv bytes followed by one byte containing the final block length, followed by the encrypted bytes.
            return concat(concat(iv, new byte[]{(byte)finalBlockLength}), encryptedBytes);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not encrypt bytes '" + Utils.bytesToHexString(plainBytes) + "'", e);
        }
    }

    /**
     * Decrypt text previously encrypted with this class.
     * 
     * @param textToDecode    The code to decrypt
     * @param password        The password to use for decryption
     * @return                The decrypted text
     * @throws                EncrypterDecrypterException
     */
    @Override
    public String decrypt(String textToDecode, char[] password) throws EncrypterDecrypterException {
        try {
            final byte[] decodeTextAsBytes = Base64.decodeBase64(textToDecode.getBytes(STRING_ENCODING));
            byte[] decryptedBytes = decrypt(decodeTextAsBytes, password);
            
            return new String(decryptedBytes, STRING_ENCODING);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not decrypt input string", e); 
        }
    }

    /**
     * Decrypt bytes previously encrypted with this class.
     * 
     * @param bytesToDecode    The bytes to decrypt
     * @param password         The password to use for decryption
     * @return                 The decrypted bytes
     * @throws                 EncrypterDecrypterException
     */
    @Override
    public byte[] decrypt(byte[] bytesToDecode, char[] password) throws EncrypterDecrypterException {
        try {
            // Separate the iv, final block length and bytes to decrypt.
            byte[] iv = new byte[BLOCK_LENGTH];
            System.arraycopy(bytesToDecode, 0, iv, 0, BLOCK_LENGTH);

            int finalBlockLength = (int)bytesToDecode[BLOCK_LENGTH];
            
            byte[] cipherBytes = new byte[bytesToDecode.length - BLOCK_LENGTH - 1];
            System.arraycopy(bytesToDecode, BLOCK_LENGTH + 1, cipherBytes, 0, bytesToDecode.length - BLOCK_LENGTH - 1);

            KeyParameter key = getAESPasswordKey(password);
            ParametersWithIV keyWithIv = new ParametersWithIV(key, iv);

            // Decrypt the message.
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(false, keyWithIv);

            byte[] paddedDecryptedBytes = new byte[cipher.getOutputSize(cipherBytes.length)];
            int length = cipher.processBytes(cipherBytes, 0, cipherBytes.length, paddedDecryptedBytes, 0);

            cipher.doFinal(paddedDecryptedBytes, length);

            // Strip off any padding bytes.
            int paddedLength = paddedDecryptedBytes.length;
            int strippedLength = paddedLength - BLOCK_LENGTH + finalBlockLength;
            
            byte[] strippedDecryptedBytes = new byte[strippedLength];
            System.arraycopy(paddedDecryptedBytes, 0, strippedDecryptedBytes, 0, strippedLength);
            
            return strippedDecryptedBytes;
        } catch (Exception e) {
            e.printStackTrace();
            throw new EncrypterDecrypterException("Could not decrypt bytes", e);
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
     * Convert char array (which are UTF16) into a byte array
     */
    private byte[] convertToByteArray(char[] charArray) {
        if (charArray == null) {
            return new byte[0];
        }
        
        byte[] byteArray = new byte[charArray.length << 1];
        for(int i = 0; i < charArray.length; i++) {
            int bytePosition = i << 1;
            byteArray[bytePosition] = (byte) ((charArray[i]&0xFF00)>>8);
            byteArray[bytePosition + 1] = (byte) (charArray[i]&0x00FF);
        }
        return byteArray;
    }

    public ScryptParameters getScryptParameters() {
        return scryptParameters;
    }

    @Override
    public String toString() {
        return "EncrypterDecrypterScrypt [scryptParameters=" + scryptParameters.toString() + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((scryptParameters == null) ? 0 : scryptParameters.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        EncrypterDecrypterScrypt other = (EncrypterDecrypterScrypt) obj;
        if (scryptParameters == null) {
            if (other.scryptParameters != null)
                return false;
        } else if (!scryptParameters.equals(other.scryptParameters))
            return false;
        return true;
    }

    public void setScryptParameters(ScryptParameters scryptParameters) {
        this.scryptParameters = scryptParameters;
    }
}