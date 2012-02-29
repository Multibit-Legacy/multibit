package org.multibit.crypto;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.spec.KeySpec;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * Password-based encryption (PKCS #5). 
 * 
 * @see http://www.ietf.org/rfc/rfc2898.txt
 * @see http://stackoverflow.com/questions/992019/java-256bit-aes-encryption/992413#992413
 * @author Steve Clay http://www.mrclay.org/
 */
public class EncrypterDecrypter {

    /**
     * bytes used to salt the key (set before making an instance)
     */
    private static final byte[] SALT = { (byte) 0xc8, (byte) 0x73, (byte) 0x41, (byte) 0x8c, (byte) 0x7e, (byte) 0xd8, (byte) 0xee,
            (byte) 0x89 };

    /**
     * number of times the password & salt are hashed during key creation (set before making an instance)
     */
    private static final int NUMBER_OF_ITERATIONS = 1024;

    /**
     * The key length in bits
     */
    private static final int KEY_LENGTH = 256;

    /**
     * The string encoding to use when converting strings to bytes
     */
    private static final String STRING_ENCODING = "utf-8";

    private static final String GENERAL_CONSTRUCTOR_EXCEPTION_TEXT = "Could not construct EncrypterDecrypter";
    private static final String GENERAL_ENCRYPT_EXCEPTION_TEXT = "Could not encrypt string '";
    private static final String GENERAL_DECRYPT_EXCEPTION_TEXT = "Could not decrypt value object '";

    private SecretKey secretKey = null;
    private Cipher cipher = null;

    /**
     * @param password
     *            The password to use for encryption and decryption
     * @throws EncrypterDecrypterException
     */
    public EncrypterDecrypter(char[] password) throws EncrypterDecrypterException {
        try {
            SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
            KeySpec keySpec = new PBEKeySpec(password, SALT, NUMBER_OF_ITERATIONS, KEY_LENGTH);
            secretKey = new SecretKeySpec(factory.generateSecret(keySpec).getEncoded(), "AES");
            cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        } catch (GeneralSecurityException e) {
            throw new EncrypterDecrypterException(GENERAL_CONSTRUCTOR_EXCEPTION_TEXT, e);
        }
    }

    /**
     * Encrypt a string
     * 
     * @param clearText The text to encrypt
     * @returns encrypterDecrypterValueObject A value object containing the encryption bytes and iv
     * @throws EncrypterDecrypterException
     */
    public EncrypterDecrypterValueObject encrypt(String clearText) throws EncrypterDecrypterException {
        try {
            cipher.init(Cipher.ENCRYPT_MODE, secretKey);

            byte[] clearTextByteArray = clearText.getBytes(STRING_ENCODING);
            return new EncrypterDecrypterValueObject(cipher.getParameters().getParameterSpec(IvParameterSpec.class).getIV(),
                    cipher.doFinal(clearTextByteArray));
        } catch (InvalidKeyException e) {
            throw new EncrypterDecrypterException(GENERAL_ENCRYPT_EXCEPTION_TEXT + clearText + "'", e);
        } catch (IOException e) {
            throw new EncrypterDecrypterException(GENERAL_ENCRYPT_EXCEPTION_TEXT + clearText + "'", e);
        } catch (GeneralSecurityException e) {
            throw new EncrypterDecrypterException(GENERAL_ENCRYPT_EXCEPTION_TEXT + clearText + "'", e);
        }
    }

    /**
     * decrypt an encrypter decrypter value object (IV and cipherText)
     * 
     * @param valueObject The encrypter decrypter value object to decrypt
     * @throws EncrypterDecrypterException
     */
    public String decrypt(EncrypterDecrypterValueObject valueObject) throws EncrypterDecrypterException {
        try {
            cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(valueObject.getIv()));
            byte[] plainTextByteArray = cipher.doFinal(valueObject.getCiphertext());
            return new String(plainTextByteArray, STRING_ENCODING);
        } catch (InvalidKeyException e) {
            throw new EncrypterDecrypterException(GENERAL_DECRYPT_EXCEPTION_TEXT + valueObject.toString() + "'", e);
        } catch (InvalidAlgorithmParameterException e) {
            throw new EncrypterDecrypterException(GENERAL_DECRYPT_EXCEPTION_TEXT + valueObject.toString() + "'", e);
        } catch (GeneralSecurityException e) {
            throw new EncrypterDecrypterException(GENERAL_DECRYPT_EXCEPTION_TEXT + valueObject.toString() + "'", e);
        } catch (UnsupportedEncodingException e) {
            throw new EncrypterDecrypterException(GENERAL_DECRYPT_EXCEPTION_TEXT + valueObject.toString() + "'", e);
        }
    }
}