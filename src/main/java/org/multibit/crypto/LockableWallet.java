package org.multibit.crypto;

import java.util.ArrayList;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;

/**
 * A Wallet subclass that can be locked and unlocked.
 * 
 *    unlocked - all the wallet keys are unencrypted
 *    locked   - all the wallet keys are encrypted
 *
 */
public class LockableWallet extends Wallet {
    private static final long serialVersionUID = -8274588170052518322L;

    /**
     * Whether the wallet has all its keys encrypted (locked = true) or not.
     */
    private boolean locked;
 
    public LockableWallet(NetworkParameters params) {
        super(params);
    }
    

    public void encrypt(char[] password) {
        // TODO Check wallet is not already encrypted.

        boolean swapNewKeychainForOld = false;
        ArrayList<ECKey> potentialNewKeyChain = new ArrayList<ECKey>();;
        
        for (ECKey key : keychain) {
            if (key instanceof EncryptableECKey) {
                ((EncryptableECKey)key).encrypt(password);
                
                // Use key as is.
                potentialNewKeyChain.add(key);
            } else {
                // Convert ECKey to EncryptableECKey and encrypt.
                EncryptableECKey newKey = new EncryptableECKey(key, new EncrypterDecrypterScrypt());
                newKey.encrypt(password);
                swapNewKeychainForOld = true;
                potentialNewKeyChain.add(newKey);
            }
        }
        
        if (swapNewKeychainForOld) {
            // Swap the encryptableECKeys for the original ones.
            keychain.clear();
            keychain.addAll(potentialNewKeyChain);
        }
        
        setWalletType(WalletType.ENCRYPTED);
        setLocked(true);
    }

    public void decrypt(char[] password) {
        // TODO Check wallet is not already decrypted.
           
        for (ECKey key : keychain) {
            if (key instanceof EncryptableECKey) {
                ((EncryptableECKey)key).decrypt(password);
            }
        }
        
        setLocked(false);
    }
   
    public void removeEncryption(char[] password) {
        // Remove any encryption on the keys.
        if (isLocked()) {
            decrypt(password);
        }
        
        // Change the wallet type to UNENCRYPTED and set it to unlocked.
        // (UNENCRYPTED wallets are never locked).
        setWalletType(WalletType.UNENCRYPTED);
        setLocked(false);       
    }

    public boolean isLocked() {
        return locked;
    }

    public void setLocked(boolean locked) {
        this.locked = locked;
    }
}
