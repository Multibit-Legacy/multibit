package org.multibit.crypto;

import java.io.Serializable;

public interface EncrypterDecrypter extends Serializable {

    public byte[] decrypt(byte[] bytesToDecode, char[] password) throws EncrypterDecrypterException;

    public String decrypt(String textToDecode, char[] password) throws EncrypterDecrypterException;

    public byte[] encrypt(byte[] plainBytes, char[] password) throws EncrypterDecrypterException;

    public String encrypt(String plainText, char[] password) throws EncrypterDecrypterException;
}
