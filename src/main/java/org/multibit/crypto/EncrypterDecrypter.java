package org.multibit.crypto;

import java.io.Serializable;
import org.spongycastle.crypto.params.KeyParameter;

public interface EncrypterDecrypter extends Serializable {

    public KeyParameter deriveKey(char[] password) throws EncrypterDecrypterException;
    
    public byte[] decrypt(EncryptedPrivateKey encryptedBytesToDecode, KeyParameter aesKey) throws EncrypterDecrypterException;

    public EncryptedPrivateKey encrypt(byte[] plainBytes, KeyParameter aesKey) throws EncrypterDecrypterException;

}
