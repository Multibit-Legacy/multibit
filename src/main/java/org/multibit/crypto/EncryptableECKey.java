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
    
    private EncrypterDecrypterScrypt encrypterDecrypter;
    
    private byte[] encryptedPrivateKey;
    
    private boolean isEncrypted;
    
    public EncryptableECKey() {
        super();
        init();
    }

    /**
     * Create a new EncryptableECKey from an existing ECKey
     */
    public EncryptableECKey(ECKey key) {
        super(key.getPrivKeyBytes(), key.getPubKey());
        init();   
    }
 
    private void init() {
        encrypterDecrypter = new EncrypterDecrypterScrypt();
        
        // A new encryptable ECKey is not encrypted.
        isEncrypted = false;
        encryptedPrivateKey = null;    
    }
    
    public void encrypt(char[] password) {
        // Encrypt the super private keys.
        encryptedPrivateKey = encrypterDecrypter.encrypt(this.getPrivKeyBytes(), password);
        
        // Clear the super private keys.
        this.priv = BigInteger.ZERO;
        
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
}
