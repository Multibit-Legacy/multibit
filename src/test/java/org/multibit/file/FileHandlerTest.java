/**
 * Copyright 2012 multibit.org
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
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.junit.Before;
import org.junit.Test;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.protobuf.ByteString;

public class FileHandlerTest extends TestCase {
    private final String WALLET_TESTDATA_DIRECTORY = "wallets";

    private static final String WALLET_FUTURE = "future.wallet";

    private final String TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX = "testCreateUnencryptedProtobuf";

    private final String TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX = "testCreateEncryptedProtobuf";

    private static final String TEST_CREATE_PROTOBUF_PREFIX = "testCreateProtobuf";

    private static final String TEST_WALLET_VERSION_PREFIX = "testCannotFutureWalletVersions";

    private static final String TEST_WALLET_VERSION_2_PREFIX = "testWalletVersion";
    
    private static final String TEST_FILE_ENCRYPT_DECRYPT = "testFileEncryptDecrypt";

    private static final String TEST_SCRYPT_PARAMETERS = "testScryptParameters";
    
    private final CharSequence WALLET_PASSWORD = "horatio nelson 123";

    public static final String CIPHER_TESTDATA_DIRECTORY = "cipher";
    public static final String CIPHER_WALLET_VERSION_0_FILENAME = "qwertyuiop-version-0.wallet.cipher";
    public static final String CIPHER_WALLET_VERSION_FF_FILENAME = "qwertyuiop-version-ff.wallet.cipher";
    public static final String CIPHER_WALLET_PASSWORD = "qwertyuiop";

    private SecureRandom secureRandom;
    
    private BitcoinController controller;
    private FileHandler fileHandler;
    
    @Before
    @Override
    public void setUp() throws Exception {
        secureRandom = new SecureRandom();
        
        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();
        
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;
        fileHandler = new FileHandler(controller);
    }
    
    @Test
    public void testCreateProtobufUnencryptedWallet() throws IOException {
        File temporaryWallet = File.createTempFile(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new unencrypted (vanilla) protobuf wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX);
        
        // Check the wallet status before it is written out and reborn.
        assertEquals(MultiBitWalletVersion.PROTOBUF, perWalletModelData.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not UNENCRYPTED when it should be", perWalletModelData.getWallet().getEncryptionType() == EncryptionType.UNENCRYPTED);
        
        // Save the wallet and then read it back in.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfoData.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded and is still protobuf and unencrypted.
        // Note - when reborn it is reborn as an EncryptableWallet.
        WalletData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX, perWalletModelDataReborn.getWalletDescription());
        assertEquals(2, perWalletModelDataReborn.getWallet().getKeychain().size());

        assertEquals(MultiBitWalletVersion.PROTOBUF, perWalletModelDataReborn.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not UNENCRYPTED when it should be", perWalletModelDataReborn.getWallet().getEncryptionType() == EncryptionType.UNENCRYPTED);

        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }
    
    @Test
    public void testCreateProtobufEncryptedWallet() throws Exception {
        // Create an encrypted wallet.
        File temporaryWallet = File.createTempFile(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        KeyCrypterScrypt initialKeyCrypter = new KeyCrypterScrypt();
        System.out.println("InitialKeyCrypter = " + initialKeyCrypter);
        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), initialKeyCrypter);
        assertEquals(MultiBitWalletVersion.PROTOBUF_ENCRYPTED, newWallet.getVersion());
        
        ECKey newKey = new ECKey();
 
        // Copy the private key bytes for checking later.
        byte[] originalPrivateKeyBytes1 = new byte[32];
        System.arraycopy(newKey.getPrivKeyBytes(), 0, originalPrivateKeyBytes1, 0, 32);
        System.out.println("EncryptableECKeyTest - Original private key 1 = " + Utils.bytesToHexString(originalPrivateKeyBytes1));
 
        newKey = newKey.encrypt(newWallet.getKeyCrypter(), newWallet.getKeyCrypter().deriveKey(WALLET_PASSWORD));
        newWallet.addKey(newKey);

        newKey = new ECKey();
 
        byte[] originalPrivateKeyBytes2 = new byte[32];
        System.arraycopy(newKey.getPrivKeyBytes(), 0, originalPrivateKeyBytes2, 0, 32);
        System.out.println("EncryptableECKeyTest - Original private key 2 = " + Utils.bytesToHexString(originalPrivateKeyBytes2));

        newKey = newKey.encrypt(newWallet.getKeyCrypter(), newWallet.getKeyCrypter().deriveKey(WALLET_PASSWORD));
        newWallet.addKey(newKey);
       
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX);
        
        // Check the wallet status before it is written out and reborn.
        assertEquals(MultiBitWalletVersion.PROTOBUF_ENCRYPTED, perWalletModelData.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not ENCRYPTED when it should be", perWalletModelData.getWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Get the keys of the wallet and check that all the keys are encrypted.
        Collection<ECKey> keys = newWallet.getKeychain();
        for (ECKey key : keys) {
            assertTrue("Key is not encrypted when it should be", key.isEncrypted());
        }
        
        // Save the wallet and read it back in again.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfoData.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded and is still protobuf and encrypted.
        WalletData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX, perWalletModelDataReborn.getWalletDescription());
        assertEquals(2, perWalletModelDataReborn.getWallet().getKeychain().size());

        assertEquals(MultiBitWalletVersion.PROTOBUF_ENCRYPTED, perWalletModelDataReborn.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not of type ENCRYPTED when it should be", perWalletModelDataReborn.getWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
     
        // Get the keys out the reborn wallet and check that all the keys are encrypted.
        Collection<ECKey> rebornEncryptedKeys = perWalletModelDataReborn.getWallet().getKeychain();
        for (ECKey key : rebornEncryptedKeys) {
            assertTrue("Key is not encrypted when it should be", key.isEncrypted());
        }
        
        System.out.println("Reborn KeyCrypter = " + perWalletModelDataReborn.getWallet().getKeyCrypter());
        
        // Decrypt the reborn wallet.
        perWalletModelDataReborn.getWallet().decrypt(perWalletModelDataReborn.getWallet().getKeyCrypter().deriveKey(WALLET_PASSWORD));

        // Get the keys out the reborn wallet and check that all the keys match.
        Collection<ECKey> rebornKeys = perWalletModelDataReborn.getWallet().getKeychain();
        
        assertEquals("Wrong number of keys in reborn wallet", 2, rebornKeys.size());
        
        Iterator<ECKey> iterator = rebornKeys.iterator();
        ECKey firstRebornKey = iterator.next();
        assertTrue("firstRebornKey should now de decrypted but is not", !firstRebornKey.isEncrypted());
        // The reborn unencrypted private key bytes should match the original private key.
        byte[] firstRebornPrivateKeyBytes = firstRebornKey.getPrivKeyBytes();
        System.out.println("FileHandlerTest - Reborn decrypted first private key = " + Utils.bytesToHexString(firstRebornPrivateKeyBytes));

        for (int i = 0; i < firstRebornPrivateKeyBytes.length; i++) {
            assertEquals("Byte " + i + " of the reborn first private key did not match the original", originalPrivateKeyBytes1[i], firstRebornPrivateKeyBytes[i]);
        }
        
        ECKey secondRebornKey = iterator.next();
        assertTrue("secondRebornKey should now de decrypted but is not", !secondRebornKey.isEncrypted());
        // The reborn unencrypted private key bytes should match the original private key.
        byte[] secondRebornPrivateKeyBytes = secondRebornKey.getPrivKeyBytes();
        System.out.println("FileHandlerTest - Reborn decrypted second private key = " + Utils.bytesToHexString(secondRebornPrivateKeyBytes));

        for (int i = 0; i < secondRebornPrivateKeyBytes.length; i++) {
            assertEquals("Byte " + i + " of the reborn second private key did not match the original", originalPrivateKeyBytes2[i], secondRebornPrivateKeyBytes[i]);
        }
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }

    @Test
    public void testDefaultScryptParameters() throws Exception {
        // Create an encrypted wallet with default scrypt parameters.
        File temporaryWallet = File.createTempFile(TEST_SCRYPT_PARAMETERS + "1", ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        KeyCrypter testKeyCrypter = new KeyCrypterScrypt();
        
        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), testKeyCrypter);
        ECKey newKey = new ECKey();
        newKey = newKey.encrypt(newWallet.getKeyCrypter(), newWallet.getKeyCrypter().deriveKey(WALLET_PASSWORD));
        newWallet.addKey(newKey);
       
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_SCRYPT_PARAMETERS);
        
        // Save the wallet and read it back in again.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        String walletInfoFileAsString = WalletInfoData.createWalletInfoFilename(newWalletFilename);
        File walletInfoFile = new File(walletInfoFileAsString);

        // Load the wallet and check the default scrypt parameters
        WalletData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        
        KeyCrypter rebornEncrypterDecrypter = perWalletModelDataReborn.getWallet().getKeyCrypter();
        assertNotNull("There was no encrypterDecrypter after round trip", rebornEncrypterDecrypter);

        assertTrue("EncrypterDecrypter was not an EncrypterDecrypterScrypt", rebornEncrypterDecrypter instanceof KeyCrypterScrypt);
        
        KeyCrypterScrypt rebornEncrypterDecrypterScrypt = (KeyCrypterScrypt)rebornEncrypterDecrypter;
        assertEquals("Wrong N parameter", 16384, rebornEncrypterDecrypterScrypt.getScryptParameters().getN());
        assertEquals("Wrong R parameter", 8, rebornEncrypterDecrypterScrypt.getScryptParameters().getR());
        assertEquals("Wrong P parameter", 1, rebornEncrypterDecrypterScrypt.getScryptParameters().getP());
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }

    @Test
    public void testNonDefaultScryptParameters() throws Exception {
        // Non default scrypt parameters.
        int n = 32768;
        int r = 8;
        int p = 3;
        
        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder()
        .setSalt(ByteString.copyFrom(salt)).setN(n).setR(r).setP(p);
        ScryptParameters scryptParameters = scryptParametersBuilder.build();

        KeyCrypter testKeyCrypter = new KeyCrypterScrypt(scryptParameters);
        
        // Create an encrypted wallet with nondefault scrypt parameters.
        File temporaryWallet = File.createTempFile(TEST_SCRYPT_PARAMETERS + "2", ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), testKeyCrypter);
        ECKey newKey = new ECKey();
        newKey = newKey.encrypt(newWallet.getKeyCrypter(), newWallet.getKeyCrypter().deriveKey(WALLET_PASSWORD));
        newWallet.addKey(newKey);
        
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_SCRYPT_PARAMETERS + "2");
        
        // Save the wallet and read it back in again.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        String walletInfoFileAsString = WalletInfoData.createWalletInfoFilename(newWalletFilename);
        File walletInfoFile = new File(walletInfoFileAsString);

        // Load the wallet and check the default scrypt parameters
        WalletData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        
        KeyCrypter rebornKeyCrypter = perWalletModelDataReborn.getWallet().getKeyCrypter();
        assertNotNull("There was no keyCrypter after round trip", rebornKeyCrypter);

        assertTrue("EncrypterDecrypter was not an KeyCrypterScrypt", rebornKeyCrypter instanceof KeyCrypterScrypt);
        
        KeyCrypterScrypt rebornKeyCrypterScrypt = (KeyCrypterScrypt)rebornKeyCrypter;
        assertEquals("Wrong N parameter", n, rebornKeyCrypterScrypt.getScryptParameters().getN());
        assertEquals("Wrong R parameter", r, rebornKeyCrypterScrypt.getScryptParameters().getR());
        assertEquals("Wrong P parameter", p, rebornKeyCrypterScrypt.getScryptParameters().getP());
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }
    
    private void deleteWalletAndCheckDeleted(WalletData perWalletModelData, File walletFile, File walletInfoFile) {
        // Delete wallet and check it is deleted.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelData);
        assertTrue(!walletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
    
    
    @Test
    public void testCannotLoadOrSaveFutureWalletVersions() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;

        File temporaryWallet = File.createTempFile(TEST_WALLET_VERSION_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet with a future wallet version
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.FUTURE);
        
        perWalletModelData.setWalletInfo(walletInfo);
       
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_PROTOBUF_PREFIX);
        
        // Should not be able to save a wallet version from the future.
        try {
            controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);
            fail("Could save a wallet version from the future but should not be able to");
        } catch (WalletVersionException wve) {
            // Expected result.
        } 

        // Check a wallet from the future cannot be loaded.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String futureWalletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_FUTURE;

        try {
            File futureWalletFile = new File(futureWalletName);
            fileHandler.loadFromFile(futureWalletFile);
            fail("Could load a wallet version from the future but should not be able to");
        } catch (WalletVersionException wve) {
            // Expected result.
        }
    }
    
    @Test
    public void testWalletVersion2a() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;

        File temporaryWallet = File.createTempFile(TEST_WALLET_VERSION_2_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        WalletData perWalletModelData = new WalletData();
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF);
        
        perWalletModelData.setWalletInfo(walletInfo);
       
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_WALLET_VERSION_2_PREFIX);
        
        // Save the wallet
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the version gets round tripped.
        WalletData perWalletModelDataReborn = fileHandler.loadFromFile(new File(newWalletFilename));
        assertNotNull(perWalletModelDataReborn);
        
        WalletInfoData rebornWalletInfo = perWalletModelDataReborn.getWalletInfo();
        assertEquals("Wallet version was not roundtripped", MultiBitWalletVersion.PROTOBUF, rebornWalletInfo.getWalletVersion());;
    }
    
    @Test
    public void testWalletVersion2b() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;

        File temporaryWallet = File.createTempFile(TEST_WALLET_VERSION_2_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        newKey = new ECKey();
        newWallet.getKeychain().add(newKey);
        WalletData perWalletModelData = new WalletData();
        // The wallet info incorrectly states it is encrypted but the wallet is not encrypted.
        WalletInfoData walletInfo = new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
       
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_WALLET_VERSION_2_PREFIX);
        
        // Save the wallet
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the version gets round tripped.
        WalletData perWalletModelDataReborn = fileHandler.loadFromFile(new File(newWalletFilename));
        assertNotNull(perWalletModelDataReborn);
        
        WalletInfoData rebornWalletInfo = perWalletModelDataReborn.getWalletInfo();
        assertEquals("Wallet version was incorrect.", MultiBitWalletVersion.PROTOBUF, rebornWalletInfo.getWalletVersion());;
    }
    
    @Test
    public void testFileCopyAndEncrypt() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        controller = controllers.bitcoinController;

        File temporaryWallet = File.createTempFile(TEST_FILE_ENCRYPT_DECRYPT, ".wallet");
        temporaryWallet.deleteOnExit();

        File temporaryWalletCopy = File.createTempFile(TEST_FILE_ENCRYPT_DECRYPT, ".wallet.cipher");
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
        perWalletModelData.setWalletDescription(TEST_FILE_ENCRYPT_DECRYPT);
        
        // Save the wallet
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Copy the wallet and encrypt the whole file.
        FileHandler.copyFileAndEncrypt(temporaryWallet, temporaryWalletCopy, WALLET_PASSWORD);
        
        // Read the file back and decrypt it.
        byte[] decryptedWalletBytes = FileHandler.readFileAndDecrypt(temporaryWalletCopy, WALLET_PASSWORD);
        
        // The decrypted bytes should match what is in the temporaryWallet file.
        byte[] sourceBytes = FileHandler.read(temporaryWallet);
        
        assertEquals("Wrong length of file after encrypt save roundtrip", sourceBytes.length, decryptedWalletBytes.length);
        
        assertTrue("The wallet after the encrypt save roundtrip has changed", Arrays.areEqual(sourceBytes, decryptedWalletBytes));
    }
    
    @Test
    public void checkSaltAndIVLength() {
        // If something changes in the KeyCrypterScrypt it would cause backwards compatibility problems reading and writing
        // the encrypted backup files so check them.
        assertTrue("The salt length seems to have changed from length 8 bytes", FileHandler.EXPECTED_LENGTH_OF_SALT == KeyCrypterScrypt.SALT_LENGTH);
        assertTrue("The initialisation vector length seems to have changed from length 16 bytes", FileHandler.EXPECTED_LENGTH_OF_IV == KeyCrypterScrypt.BLOCK_LENGTH);
    }
    
    @Test
    public void testReadCipherVersion0() throws Exception {  
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + CIPHER_TESTDATA_DIRECTORY;
        
        File walletFile = new File (testDirectory + File.separator + CIPHER_WALLET_VERSION_0_FILENAME);
        
        // Read in the version 0 wallet and decrypt it.
        byte[] walletBytes = FileHandler.readFileAndDecrypt(walletFile, CIPHER_WALLET_PASSWORD); 

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
            byte[] walletBytes = FileHandler.readFileAndDecrypt(walletFile, CIPHER_WALLET_PASSWORD); 

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
