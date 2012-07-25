package org.multibit.crypto;

import java.math.BigInteger;

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
    
    /**
     * Indicates whether the private key is encrypted (true) or not (false).
     * A private key is deemed to be encrypted when there is no private key and the encryptedPrivateKey is non-zero.
     */
    private boolean isEncrypted;
    
    /**
     * Create a new EncryptableECKey.
     * This creates a new private key.
     * 
     * @param encrypterDecrypter
     */
    public EncryptableECKey(EncrypterDecrypter encrypterDecrypter) {
        super();
        init(encrypterDecrypter);
    }

    /**
     * Create a new EncryptableECKey from an existing ECKey
     * @param key ECKey The key to 'wrap' into an EncryptableECKey
     * @param encrypterDecrypter The EncrypterDEcrypter that will be used to encrypt and decrypt the key
     */
    public EncryptableECKey(ECKey key, EncrypterDecrypter encrypterDecrypter) {
        super(key.getPrivKeyBytes(), key.getPubKey());
        init(encrypterDecrypter);   
    }
 
    /**
     * Create a new EncryptableECKey from an existing ECKey and an encrypted private key
     * @param key ECKey The key to 'wrap' into an EncryptableECKey
     * @param encryptedPrivateKey The encrypted bytes of the private key
     * @param encrypterDecrypter The EncrypterDEcrypter that will be used to encrypt and decrypt the key
     */
    public EncryptableECKey(ECKey key, byte[] encryptedPrivateKey, EncrypterDecrypter encrypterDecrypter) {
        super(key.getPrivKeyBytes(), key.getPubKey());
        this.encryptedPrivateKey = encryptedPrivateKey;
        this.encrypterDecrypter = encrypterDecrypter;
               
        // If a nonblank encrypted private key is specified then blank the private keys and mark it as encrypted.
        if (encryptedPrivateKey != null && encryptedPrivateKey.length > 0 ) {
            priv = BigInteger.ZERO;
            isEncrypted = true;
        }
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

    public byte[] getEncryptedPrivateKey() {
        if (encryptedPrivateKey == null) {
            return null;
        }
        
        // Copy the private key to make it unmodifiable.
        byte[] copy = new byte[encryptedPrivateKey.length];
        System.arraycopy(encryptedPrivateKey, 0, copy, 0, encryptedPrivateKey.length);
                  
        return copy;
    }
}
