package org.multibit.crypto;

import java.util.ArrayList;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;

/**
 * A class to encrypt and decrypt the wallet keys.
 * It also manages the state of the wallet, namely:
 * 
 *    unlocked - all the wallet keys are unencrypted
 *    locked   - all the wallet keys are encrypted
 *    
 * @author jim
 *
 */

public class WalletEncrypterDecrypter {

    public void encrypt(Wallet wallet, char[] password) {
        // TODO Check wallet is not already encrypted.
        
        ArrayList<ECKey> keychain = wallet.keychain;
        
        boolean swapNewKeychainForOld = false;
        ArrayList<ECKey> potentialNewKeyChain = new ArrayList<ECKey>();;
        
        for (ECKey key : keychain) {
            if (key instanceof EncryptableECKey) {
                ((EncryptableECKey)key).encrypt(password);
                
                // Use key as is.
                potentialNewKeyChain.add(key);
            } else {
                // Convert ECKey to EncryptableECKey and encrypt.
                EncryptableECKey newKey = new EncryptableECKey(key);
                newKey.encrypt(password);
                swapNewKeychainForOld = true;
                potentialNewKeyChain.add(newKey);
            }
        }
        
        if (swapNewKeychainForOld) {
            // Swap the encryptableECKeys for the original ones.
            wallet.keychain.clear();
            wallet.keychain.addAll(potentialNewKeyChain);
        }
        
        wallet.setWalletType(WalletType.ENCRYPTED);
        wallet.setLocked(true);
    }

    public void decrypt(Wallet wallet, char[] password) {
        // TODO Check wallet is not already decrypted.
        
        ArrayList<ECKey> keychain = wallet.keychain;
        
        for (ECKey key : keychain) {
            if (key instanceof EncryptableECKey) {
                ((EncryptableECKey)key).decrypt(password);
            }
        }
        
        wallet.setLocked(false);
    }
   
    public void removeEncryption(Wallet wallet, char[] password) {
        // Remove any encryption on the keys.
        if (wallet.isLocked()) {
            decrypt(wallet, password);
        }
        
        // Change the wallet type to UNENCRYPTED and set it to unlocked.
        // (UNENCRYPTED wallets are never locked).
        wallet.setWalletType(WalletType.UNENCRYPTED);
        wallet.setLocked(false);       
    }
}
