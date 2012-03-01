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
import java.security.Security;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.crypto.BufferedBlockCipher;
import org.bouncycastle.crypto.CipherParameters;
import org.bouncycastle.crypto.CryptoException;
import org.bouncycastle.crypto.PBEParametersGenerator;
import org.bouncycastle.crypto.digests.SHA1Digest;
import org.bouncycastle.crypto.engines.AESFastEngine;
import org.bouncycastle.crypto.generators.PKCS12ParametersGenerator;
import org.bouncycastle.crypto.modes.CBCBlockCipher;
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.bouncycastle.crypto.params.ParametersWithIV;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

public class EncrypterDecrypter {

    /**
     * bytes used to salt the key
     */
    // private static final byte[] SALT = { (byte) 0xa2, (byte) 0x73, (byte)
    // 0x27, (byte) 0x8c, (byte) 0xb1, (byte) 0xd8, (byte) 0x1f,
    // (byte) 0xc3 };

    /**
     * number of times the password & salt are hashed during key creation
     */
    private static final int NUMBER_OF_ITERATIONS = 1024;

    /**
     * The string encoding to use when converting strings to bytes
     */
    private static final String STRING_ENCODING = "utf-8";

    private byte[] salt;

    public EncrypterDecrypter() {
        // Generate random salt
        SecureRandom secureRandom = new SecureRandom();
        salt = new byte[16];
        secureRandom.nextBytes(salt);
    }

    // Get password to generate symmetric key with
    // (or without IV) To be used in an AES underlying cipher
    private CipherParameters getAESPasswordKey(char[] password) throws Exception {
        PBEParametersGenerator generator = new PKCS12ParametersGenerator(new SHA1Digest());
        generator.init(PBEParametersGenerator.PKCS12PasswordToBytes(password), salt, NUMBER_OF_ITERATIONS);
        // Generate a 256 bit key w/ 128 bit IV
        ParametersWithIV key = (ParametersWithIV) generator.generateDerivedParameters(256, 128);
        // Generate a 128 kit key
        // CipherParameters key =
        // generator.generateDerivedParameters(128);
        return key;
    }

    // Password based encryption using AES
    public String encrypt(String plainText, char[] password) throws EncrypterDecrypterException {
        try {
            final byte[] plainTextAsBytes = plainText.getBytes(STRING_ENCODING);

            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(password);
            // The following code uses an AES cipher to
            // encrypt the message
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(true, key);
            byte[] result = new byte[cipher.getOutputSize(plainTextAsBytes.length)];
            int length = cipher.processBytes(plainTextAsBytes, 0, plainTextAsBytes.length, result, 0);

            cipher.doFinal(result, length);
            return Base64.encodeBase64String(result);
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not encrypt string '" + plainText + "'", e);
        }
    }

    // Password based decryption using AES
    public String decrypt(String cipherText, char[] password) throws EncrypterDecrypterException {
        try {
            final byte[] cipherTextAsBytes = Base64.decodeBase64(cipherText.getBytes(STRING_ENCODING));

            ParametersWithIV key = (ParametersWithIV) getAESPasswordKey(password);
            // The following code uses an AES cipher to
            // decrypt the message
            BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESFastEngine()));
            cipher.init(false, key);
            byte[] decryptedBytes = new byte[cipher.getOutputSize(cipherTextAsBytes.length)];
            int length = cipher.processBytes(cipherTextAsBytes, 0, cipherTextAsBytes.length, decryptedBytes, 0);

            cipher.doFinal(decryptedBytes, length);
            String decryptedText = new String(decryptedBytes, STRING_ENCODING).trim();
            return decryptedText;
        } catch (Exception e) {
            throw new EncrypterDecrypterException("Could not decrypt string '" + cipherText + "'", e);
        }

    }
}