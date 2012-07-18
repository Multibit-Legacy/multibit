package org.multibit.crypto;

import java.util.ArrayList;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;

/**
 * A Wallet subclass that can be encrypted and decrypted.
 *
 */
public class EncryptableWallet extends Wallet {
    private static final long serialVersionUID = -8274588170052518322L;

    /**
     * Whether the wallet has all its keys encrypted (locked = true) or not.
     */
    private boolean currentlyEncrypted;
 
    public EncryptableWallet(NetworkParameters params) {
        super(params);
    }
    
    /**
     * Encrypt the wallet with the supplied password.
     * @param password
     */
    public void encrypt(char[] password) {
        if (currentlyEncrypted) {
            throw new WalletIsAlreadyEncryptedException("Wallet is already encrypted");
        }

        // Create a new arraylist that will contain the encrypted keys
        ArrayList<ECKey> encryptedKeyChain = new ArrayList<ECKey>();;
        
        for (ECKey key : keychain) {
             if (key instanceof EncryptableECKey) {
                // TODO Check key is not encrypted.
                ((EncryptableECKey)key).encrypt(password);
                encryptedKeyChain.add(key);
            } else {
                // Convert plain ECKey into EncryptableECKey - then encrypt.
                EncryptableECKey newKey = new EncryptableECKey(key, new EncrypterDecrypterScrypt());
                ((EncryptableECKey)newKey).encrypt(password);
                encryptedKeyChain.add(newKey);
            }
        }
        
        synchronized(this) {
            // Swap the encryptableECKeys for the original ones.
            keychain.clear();
            keychain.addAll(encryptedKeyChain);
        
            setWalletType(WalletType.ENCRYPTED);
            currentlyEncrypted = true;
        }
    }

    /**
     * Decrypt the wallet with the supplied password.
     * 
     * @param password
     */
    public void decrypt(char[] password) {
        if (!currentlyEncrypted) {
            throw new WalletIsAlreadyEncryptedException("Wallet is already decrypted");
        }

        // Create a new arraylist that will contain the decrypted keys
        ArrayList<ECKey> decryptedKeyChain = new ArrayList<ECKey>();;
 
        for (ECKey key : keychain) {
            if (key instanceof EncryptableECKey) {
                // TODO Check key is not encrypted.
                ((EncryptableECKey)key).decrypt(password);
                decryptedKeyChain.add(key);
            } else {
                // Convert plain ECKey into EncryptableECKey - does not need decrypting.
                EncryptableECKey newKey = new EncryptableECKey(key, new EncrypterDecrypterScrypt());
                decryptedKeyChain.add(newKey);
            }
        }
        
        synchronized(this) {
            // Swap the encryptableECKeys for the original ones.
            keychain.clear();
            keychain.addAll(decryptedKeyChain);
       
            currentlyEncrypted = false;
        }
    }
   
    /**
     * Remove any encryption on the private keys and change the wallet type to WalletType.UNENCRYPTED.
     * The wallet is no longer encrypted.
     * 
     * @param password
     */
    public void removeEncryption(char[] password) {
        // Remove any encryption on the keys.
        if (isCurrentlyEncrypted()) {
            decrypt(password);
        }
        
        // Change the wallet type to UNENCRYPTED and set it to being unencrypted.
        setWalletType(WalletType.UNENCRYPTED);
        currentlyEncrypted = false;       
    }

    /**
     * Whether the wallet is currently encrypted (=true) or not (=false).
     */
    public boolean isCurrentlyEncrypted() {
        return currentlyEncrypted;
    }
    
    
    /**
     *  Check whether the password can decrypt the first key in the wallet.
     *  
     *  @returns boolean True if password supplied can decrypt the first private key in the wallet, false otherwise.
     */
    public boolean checkPasswordCanDecryptFirstPrivateKey(char[] password) {
        ECKey firstECKey = getKeychain().get(0);

        if (firstECKey != null && firstECKey instanceof EncryptableECKey) {
            EncryptableECKey encryptableECKey = (EncryptableECKey) firstECKey;
            byte[] encryptedBytes = encryptableECKey.getEncryptedPrivateKey();
            // Clone the encrypted bytes.
            byte[] cloneEncrypted = new byte[encryptedBytes.length];
            System.arraycopy(encryptedBytes, 0, cloneEncrypted, 0, encryptedBytes.length);
            EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypterScrypt();
            try {
                encrypterDecrypter.decrypt(cloneEncrypted, password);
                
                // Success.
                return true;
            } catch (EncrypterDecrypterException ede) {
                // The password supplied is incorrect.
                return false;
            }
        }
        
        return false;
    }
}
