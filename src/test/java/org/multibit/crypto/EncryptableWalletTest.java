/**
 * Copyright 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.multibit.crypto;

import java.security.SecureRandom;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;
import com.google.bitcoin.utils.BriefLogFormatter;

public class EncryptableWalletTest extends TestCase {
    static final NetworkParameters params = NetworkParameters.unitTests();

    private Wallet wallet;
    private ECKey myKey;

    private static char[] PASSWORD1 = "my helicopter contains eels".toCharArray();
    private static char[] WRONG_PASSWORD = "nothing noone nobody nowhere".toCharArray();
    
    private SecureRandom secureRandom;

    @Before
    public void setUp() throws Exception {
        myKey = new ECKey();
        
        secureRandom = new SecureRandom();
        
        byte[] salt = new byte[ScryptParameters.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        ScryptParameters scryptParameters = new ScryptParameters(salt);
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);
        wallet = new Wallet(params, encrypterDecrypter);

        wallet.addKey(myKey);

        BriefLogFormatter.init();
    }

    @Test
    public void testEncryptionDecryption() throws Exception {
        // Check the wallet is initially of WalletType UNENCRYPTED and not currently encrypted
        assertTrue("Wallet is not an unencrypted wallet", wallet.getWalletType() == WalletType.UNENCRYPTED);
        assertTrue("Wallet is currently encrypted but should not be", !wallet.isCurrentlyEncrypted());
        
        // Correct password should not decrypt first private key as wallet is decrypted.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong.1", !wallet.checkPasswordCanDecryptFirstPrivateKey(PASSWORD1));
        
        // Encrypt wallet.
        wallet.encrypt(PASSWORD1);

        // Wallet should now be of type WalletType.UNENCRYPTED and currently encrypted.
        assertTrue("Wallet is not an encrypted wallet", wallet.getWalletType() == WalletType.ENCRYPTED);
        assertTrue("Wallet is not currently encrypted", wallet.isCurrentlyEncrypted());

        // Correct password should decrypt first private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with correct password.2", wallet.checkPasswordCanDecryptFirstPrivateKey(PASSWORD1));

        // Incorrect password should not decrypt first private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password.3", !wallet.checkPasswordCanDecryptFirstPrivateKey(WRONG_PASSWORD));

        // Decrypt wallet.
        wallet.decrypt(PASSWORD1);
        
        // Wallet should now be of type WalletType.ENCRYPTED and not currently encrypted.
        assertTrue("Wallet is not an encrypted wallet", wallet.getWalletType() == WalletType.ENCRYPTED);
        assertTrue("Wallet is currently encrypted but should not be", !wallet.isCurrentlyEncrypted());

        // Correct password should decrypt first private key as wallet is has encrypted bytes.
        // (Even though it is decrypted you could decrypt it again with the correct password safely - though this is not advised).
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong.4", wallet.checkPasswordCanDecryptFirstPrivateKey(PASSWORD1));

        // Incorrect password should not decrypt first private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password.5", !wallet.checkPasswordCanDecryptFirstPrivateKey(WRONG_PASSWORD));

        // Remove the wallet encryption entirely.
        wallet.removeEncryption(PASSWORD1);

        // Wallet should now be of type WalletType.UNENCRYPTED and not currently encrypted.
        assertTrue("Wallet is not an unencrypted wallet", wallet.getWalletType() == WalletType.UNENCRYPTED);
        assertTrue("Wallet is currently encrypted but should not be", !wallet.isCurrentlyEncrypted());

        // Correct password should decrypt first private key as wallet is has encrypted bytes.
        // (Even though it is unencrypted you could decrypt it again with the correct password safely - though this is not advised).
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong when wallet is unencrypted.6", wallet.checkPasswordCanDecryptFirstPrivateKey(PASSWORD1));

        // Incorrect password should not decrypt first private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password.5", !wallet.checkPasswordCanDecryptFirstPrivateKey(WRONG_PASSWORD));
    }
}
