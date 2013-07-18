/**
 * Copyright 2013 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.file;


import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypterScrypt;

public class BackupManagerTest extends TestCase {
    private static final String TEST_FILE_COPY_AND_ENCRYPT = "testFileCopyAndEncrypt";

    private final CharSequence WALLET_PASSWORD = "horatio nelson 123";

    public static final String CIPHER_TESTDATA_DIRECTORY = "cipher";
    public static final String CIPHER_WALLET_VERSION_0_FILENAME = "qwertyuiop-version-0.wallet.cipher";
    public static final String CIPHER_WALLET_VERSION_FF_FILENAME = "qwertyuiop-version-ff.wallet.cipher";
    public static final String CIPHER_WALLET_PASSWORD = "qwertyuiop";
  
    private BitcoinController controller;
    
    @Before
    @Override
    public void setUp() throws Exception {       
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;
    }
     
    @Test
    public void testFileCopyAndEncrypt() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;

        File temporaryWallet = File.createTempFile(TEST_FILE_COPY_AND_ENCRYPT, ".wallet");
        temporaryWallet.deleteOnExit();

        File temporaryWalletCopy = File.createTempFile(TEST_FILE_COPY_AND_ENCRYPT, ".wallet.cipher");
        temporaryWalletCopy.deleteOnExit();
        temporaryWalletCopy.delete();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
       
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_FILE_COPY_AND_ENCRYPT);
        
        // Save the wallet
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Copy the wallet and encrypt the whole file.
        BackupManager.INSTANCE.copyFileAndEncrypt(temporaryWallet, temporaryWalletCopy, WALLET_PASSWORD);
        
        // Read the file back and decrypt it.
        byte[] decryptedWalletBytes = BackupManager.INSTANCE.readFileAndDecrypt(temporaryWalletCopy, WALLET_PASSWORD);
        
        // The decrypted bytes should match what is in the temporaryWallet file.
        byte[] sourceBytes = FileHandler.read(temporaryWallet);
        
        assertEquals("Wrong length of file after encrypt save roundtrip", sourceBytes.length, decryptedWalletBytes.length);
        
        assertTrue("The wallet after the encrypt save roundtrip has changed", Arrays.areEqual(sourceBytes, decryptedWalletBytes));
    }
    
    @Test
    public void checkSaltAndIVLength() {
        // If something changes in the KeyCrypterScrypt it would cause backwards compatibility problems reading and writing
        // the encrypted backup files so check them.
        assertTrue("The salt length seems to have changed from length 8 bytes", BackupManager.EXPECTED_LENGTH_OF_SALT == KeyCrypterScrypt.SALT_LENGTH);
        assertTrue("The initialisation vector length seems to have changed from length 16 bytes", BackupManager.EXPECTED_LENGTH_OF_IV == KeyCrypterScrypt.BLOCK_LENGTH);
    }
    
    @Test
    public void testReadCipherVersion0() throws Exception {  
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + CIPHER_TESTDATA_DIRECTORY;
        
        File walletFile = new File (testDirectory + File.separator + CIPHER_WALLET_VERSION_0_FILENAME);
        
        // Read in the version 0 wallet and decrypt it.
        byte[] walletBytes = BackupManager.INSTANCE.readFileAndDecrypt(walletFile, CIPHER_WALLET_PASSWORD); 

        InputStream walletInputStream = new ByteArrayInputStream(walletBytes);
        Wallet wallet = null;
        try {
            wallet = Wallet.loadFromFileStream(walletInputStream);
        } finally {
            walletInputStream.close();
        }

        // Check the private keys are reborn ok.
        assertNotNull(wallet);
        assertEquals("Wrong number of private keys in decrypted wallet file", 3, wallet.getKeychainSize());
        
        List<ECKey> keys = wallet.getKeychain();
        assertEquals("Wrong private key 0", "0ec932ea4f6b305247c12c0a4fb310d839a688ac0011d69724e6a32bc35fcda8", Utils.bytesToHexString(keys.get(0).getPrivKeyBytes()));
        assertEquals("Wrong private key 1", "02ac2c94e44fc2edab9585111480d6d438ebe8c96faf92e561957a48d2bbdacc", Utils.bytesToHexString(keys.get(1).getPrivKeyBytes()));
        assertEquals("Wrong private key 2", "b01a936b78b6a649ea0ede2182eb73629d5ca3200d36a48b1006eb6729c1bc15", Utils.bytesToHexString(keys.get(2).getPrivKeyBytes()));
    }
    
    @Test
    /**
     * Check that future versions of encrypted files cannot be loaded.
     * (This test wallet has a version of 0xff in it).
     */
    public void testReadCipherVersionff() throws Exception {  
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + CIPHER_TESTDATA_DIRECTORY;
        
        File walletFile = new File (testDirectory + File.separator + CIPHER_WALLET_VERSION_FF_FILENAME);
        
        // Read in the version ff wallet and decrypt it.
        InputStream walletInputStream = null;
        try {
            byte[] walletBytes = BackupManager.INSTANCE.readFileAndDecrypt(walletFile, CIPHER_WALLET_PASSWORD); 

            walletInputStream = new ByteArrayInputStream(walletBytes);

            Wallet.loadFromFileStream(walletInputStream);
            fail("Wallet with a future encrypted file version loaded but it should not");
        } catch(IOException ioe) {
            // Success.
        } finally {
            if (walletInputStream != null) {
                walletInputStream.close();
            }
        }
    }
}
