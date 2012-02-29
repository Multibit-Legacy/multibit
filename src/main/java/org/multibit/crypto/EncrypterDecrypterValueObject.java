package org.multibit.crypto;

import org.apache.commons.codec.binary.Base64;

/**
 * Value object containing initialization vector and cipher text 
 * 
 * Based on JSecureEdit (https://github.com/mrclay/jSecureEdit)
 * 
 * @author mrclay
 */
public class EncrypterDecrypterValueObject {
    private byte[] initializationVector;
    private byte[] cipherText;
    
    public static final String SEPARATOR = " *** ";

    public EncrypterDecrypterValueObject(byte[] iv, byte[] cipherText) {
        this.initializationVector = iv;
        this.cipherText = cipherText;
    }

    public EncrypterDecrypterValueObject(String stringBase64) throws EncrypterDecrypterException {
        int loc = stringBase64.indexOf(SEPARATOR);
        initializationVector = Base64.decodeBase64(stringBase64.substring(0, loc));
        cipherText = Base64.decodeBase64(stringBase64.substring(loc + SEPARATOR.length()));
    }

    /**
     * Get the cipherText
     * @return The ciphertext (in bytes)
     */
    public byte[] getCiphertext() {
        return cipherText;
    }

    /**
     * get the initialization vector
     * @return the initialisation vector
     */
    public byte[] getIv() {
        return initializationVector;
    }

    @Override
    public String toString() {
        return Base64.encodeBase64String(initializationVector) + SEPARATOR + Base64.encodeBase64String(cipherText);
    }
}
