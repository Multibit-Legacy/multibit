package org.multibit.crypto;

import java.math.BigInteger;

import org.multibit.crypto.EncrypterDecrypterScrypt;

import com.google.bitcoin.core.ECKey;

/**
 * Extension of ECKey that is encryptable.
 * 
 * @author jim
 *
 */
public class EncryptableECKey extends ECKey {
    
    private static final long serialVersionUID = -7791522463421293238L;
    
    private EncrypterDecrypter encrypterDecrypter;
    
    private byte[] encryptedPrivateKey;
    
    private boolean isEncrypted;
    
    public EncryptableECKey(EncrypterDecrypter encrypterDecrypter) {
        super();
        init(encrypterDecrypter);
    }

    /**
     * Create a new EncryptableECKey from an existing ECKey
     */
    public EncryptableECKey(ECKey key, EncrypterDecrypter encrypterDecrypter) {
        super(key.getPrivKeyBytes(), key.getPubKey());
        init(encrypterDecrypter);   
    }
 
    private void init(EncrypterDecrypter encrypterDecrypter) {
        this.encrypterDecrypter = encrypterDecrypter;
        
        // A new encryptable ECKey is not encrypted.
        isEncrypted = false;
        encryptedPrivateKey = null;    
    }
    
    public void encrypt(char[] password) {
        // Encrypt the super private keys.
        encryptedPrivateKey = encrypterDecrypter.encrypt(getPrivKeyBytes(), password);
        
        // Clear the super private keys.
        // TODO - put this back in - currently disabled to avoid blank keys being written to disk
        //this.priv = BigInteger.ZERO;
        
        isEncrypted = true;
    }
    
    public void decrypt(char[] password) {
        // Decrypt the private keys.
        byte[] plainTextPrivateKey = encrypterDecrypter.decrypt(encryptedPrivateKey, password);
        
        // Set the plaintext key into the super private key field.
        this.priv = new BigInteger(1, plainTextPrivateKey);
        
        isEncrypted = false;
    }
    
    public boolean isEncrypted() {
        return isEncrypted;
    }

    public byte[] getEncryptedPrivateKey() {
        return encryptedPrivateKey;
    }
}
