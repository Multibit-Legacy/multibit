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

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.ArrayList;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;
import org.multibit.Constants;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import com.google.bitcoin.crypto.EncrypterDecrypter;
import com.google.bitcoin.crypto.EncrypterDecrypterScrypt;
import com.google.bitcoin.crypto.ScryptParameters;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import com.google.bitcoin.core.WalletMajorVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.EncryptionType;
import com.google.bitcoin.core.WalletVersionException;

public class FileHandlerTest extends TestCase {
    private final String WALLET_TESTDATA_DIRECTORY = "wallets";

    private final String WALLET_PROTOBUF1 = "protobuf1.wallet";

    private static final String WALLET_FUTURE = "future.wallet";

    private final String WALLET_SERIALISED1 = "serialised1.wallet";
    private final BigInteger WALLET_SERIALISED1_BALANCE = new BigInteger("6700000");;

    private final String WALLET_SERIALISED2 = "serialised2.wallet";
    private final BigInteger WALLET_SERIALISED2_BALANCE = new BigInteger("2000000");;


    private final String TEST_CREATE_SERIALISED_PREFIX = "testCreateSerialised";

    private final String TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX = "testCreateUnencryptedProtobuf";

    private final String TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX = "testCreateEncryptedProtobuf";

    private static final String TEST_CREATE_PROTOBUF_PREFIX = "testCreateProtobuf";

    private static final String TEST_WALLET_VERSION_PREFIX = "testCannotLoadOrSaveFutureWalletVersions";

    private static final String TEST_MINOR_WALLET_VERSION_PREFIX = "testMinorWalletVersion";

    private static final String TEST_SCRYPT_PARAMETERS = "testScryptParameters";
    
    private final char[] WALLET_PASSWORD = "horatio nelson 123".toCharArray();

    private SecureRandom secureRandom;
    
    private MultiBitController controller;
    private FileHandler fileHandler;
    
    private EncrypterDecrypter encrypterDecrypter;

    @Before
    @Override
    public void setUp() throws Exception {
        secureRandom = new SecureRandom();
        
        byte[] salt = new byte[ScryptParameters.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        ScryptParameters scryptParameters = new ScryptParameters(salt);
        encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);
        
        controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);
        controller.setLocaliser(localiser);
        controller.setModel(model);
        fileHandler = new FileHandler(controller);
    }
    
    @Test
    public void testLoadSerialised1() throws IOException {
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_SERIALISED1;

        File walletFile = new File(walletName);
        walletFile.createNewFile();

        assertTrue(walletFile.exists());
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);

        assertNotNull(perWalletModelData);

        assertEquals(WALLET_SERIALISED1_BALANCE, perWalletModelData.getWallet().getBalance());
        
        // Check wallet type.
        assertTrue("Wallet type is WalletType.ENCRYPTED but it should not be", perWalletModelData.getWallet().getEncryptionType() == EncryptionType.UNENCRYPTED);
    }

    @Test
    public void testLoadSerialised2() throws IOException {
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_SERIALISED2;

        File walletFile = new File(walletName);

        walletFile.createNewFile();

        assertTrue(walletFile.exists());
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);

        assertNotNull(perWalletModelData);

        assertEquals(WALLET_SERIALISED2_BALANCE, perWalletModelData.getWallet().getBalance());
        
        // Check wallet type.
        assertTrue("Wallet type is WalletType.ENCRYPTED but it should not be", perWalletModelData.getWallet().getEncryptionType() == EncryptionType.UNENCRYPTED);
    }
    
    @Test
    public void testCreateSerialisedWallet() throws IOException {
        File temporaryWallet = File.createTempFile(TEST_CREATE_SERIALISED_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new serialised wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        newKey = new ECKey();
        newWallet.keychain.add(newKey);
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.SERIALIZED);
        
        perWalletModelData.setWalletInfo(walletInfo);
       
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_SERIALISED_PREFIX);
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded and is still serialised.
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_SERIALISED_PREFIX, perWalletModelDataReborn.getWalletDescription());
        assertEquals(2, perWalletModelDataReborn.getWallet().keychain.size());

        assertEquals(WalletMajorVersion.SERIALIZED, perWalletModelDataReborn.getWalletInfo().getWalletMajorVersion());
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }
    
    @Test
    public void testCreateProtobufUnencryptedWallet() throws IOException {
        File temporaryWallet = File.createTempFile(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new unencrypted (vanilla) protobuf wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        newKey = new ECKey();
        newWallet.keychain.add(newKey);
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX);
        
        // Check the wallet status before it is written out and reborn.
        assertEquals(WalletMajorVersion.PROTOBUF_ENCRYPTED, perWalletModelData.getWalletInfo().getWalletMajorVersion());
        assertTrue("Wallet is not UNENCRYPTED when it should be", perWalletModelData.getWallet().getEncryptionType() == EncryptionType.UNENCRYPTED);
        
        // Save the wallet and then read it back in.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded and is still protobuf and unencrypted.
        // Note - when reborn it is reborn as an EncryptableWallet.
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX, perWalletModelDataReborn.getWalletDescription());
        assertEquals(2, perWalletModelDataReborn.getWallet().keychain.size());

        assertEquals(WalletMajorVersion.PROTOBUF_ENCRYPTED, perWalletModelDataReborn.getWalletInfo().getWalletMajorVersion());
        assertTrue("Wallet is not UNENCRYPTED when it should be", perWalletModelDataReborn.getWallet().getEncryptionType() == EncryptionType.UNENCRYPTED);
        
        assertTrue("Wallet isCurrentlyEncrypted when it should not be", !perWalletModelDataReborn.getWallet().isCurrentlyEncrypted());
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }
    
    @Test
    public void testCreateProtobufEncryptedWallet() throws IOException {
        // Create an encrypted wallet.
        File temporaryWallet = File.createTempFile(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), encrypterDecrypter);
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);

        // Copy the private key bytes for checking later.
        byte[] originalPrivateKeyBytes1 = new byte[32];
        System.arraycopy(newKey.getPrivKeyBytes(), 0, originalPrivateKeyBytes1, 0, 32);
        System.out.println("EncryptableECKeyTest - Original private key 1 = " + Utils.bytesToHexString(originalPrivateKeyBytes1));
        
        newKey = new ECKey();
        newWallet.keychain.add(newKey);

        byte[] originalPrivateKeyBytes2 = new byte[32];
        System.arraycopy(newKey.getPrivKeyBytes(), 0, originalPrivateKeyBytes2, 0, 32);
        System.out.println("EncryptableECKeyTest - Original private key 2 = " + Utils.bytesToHexString(originalPrivateKeyBytes2));

        newWallet.encrypt(newWallet.getEncrypterDecrypter().deriveKey(WALLET_PASSWORD));
        
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX);
        
        // Check the wallet status before it is written out and reborn.
        assertEquals(WalletMajorVersion.PROTOBUF_ENCRYPTED, perWalletModelData.getWalletInfo().getWalletMajorVersion());
        assertTrue("Wallet is not ENCRYPTED when it should be", perWalletModelData.getWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        
        assertTrue("Wallet isCurrentlyEncrypted is false when it should be true", perWalletModelData.getWallet().isCurrentlyEncrypted());

        // Get the keys of the wallet and check that all the keys are encrypted.
        ArrayList<ECKey> keys = newWallet.getKeychain();
        for (ECKey key : keys) {
            assertTrue("Key is not encrypted when it should be", key.isEncrypted());
        }
        
        // Save the wallet and read it back in again.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded and is still protobuf and encrypted.
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX, perWalletModelDataReborn.getWalletDescription());
        assertEquals(2, perWalletModelDataReborn.getWallet().keychain.size());

        assertEquals(WalletMajorVersion.PROTOBUF_ENCRYPTED, perWalletModelDataReborn.getWalletInfo().getWalletMajorVersion());
        assertTrue("Wallet is not of type ENCRYPTED when it should be", perWalletModelDataReborn.getWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        
        assertTrue("Wallet isCurrentlyEncrypted is false when it should be true", perWalletModelDataReborn.getWallet().isCurrentlyEncrypted());
        
        // Get the keys out the reborn wallet and check that all the keys are encrypted.
        ArrayList<ECKey> rebornEncryptedKeys = perWalletModelDataReborn.getWallet().getKeychain();
        for (ECKey key : rebornEncryptedKeys) {
            assertTrue("Key is not encrypted when it should be", key.isEncrypted());
        }
        
        // Decrypt the reborn wallet.
        perWalletModelDataReborn.getWallet().decrypt(perWalletModelDataReborn.getWallet().getEncrypterDecrypter().deriveKey(WALLET_PASSWORD));

        // Get the keys out the reborn wallet and check that all the keys match.
        ArrayList<ECKey> rebornKeys = perWalletModelDataReborn.getWallet().getKeychain();
        
        assertEquals("Wrong number of keys in reborn wallet", 2, rebornKeys.size());
        
        ECKey firstRebornKey = rebornKeys.get(0);
        assertTrue("firstRebornKey should now de decrypted but is not", !firstRebornKey.isEncrypted());
        // The reborn unencrypted private key bytes should match the original private key.
        byte[] firstRebornPrivateKeyBytes = firstRebornKey.getPrivKeyBytes();
        System.out.println("FileHandlerTest - Reborn decrypted first private key = " + Utils.bytesToHexString(firstRebornPrivateKeyBytes));

        for (int i = 0; i < firstRebornPrivateKeyBytes.length; i++) {
            assertEquals("Byte " + i + " of the reborn first private key did not match the original", originalPrivateKeyBytes1[i], firstRebornPrivateKeyBytes[i]);
        }
        
        ECKey secondRebornKey = rebornKeys.get(1);
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
    public void testDefaultScryptParameters() throws IOException {
        // Create an encrypted wallet with default scrypt parameters.
        File temporaryWallet = File.createTempFile(TEST_SCRYPT_PARAMETERS + "1", ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), encrypterDecrypter);
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);

        newWallet.encrypt(newWallet.getEncrypterDecrypter().deriveKey(WALLET_PASSWORD));
        
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_SCRYPT_PARAMETERS);
        
        // Save the wallet and read it back in again.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);
        File walletInfoFile = new File(walletInfoFileAsString);

        // Load the wallet and check the default scrypt parameters
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        
        EncrypterDecrypter rebornEncrypterDecrypter = perWalletModelDataReborn.getWallet().getEncrypterDecrypter();
        assertNotNull("There was no encrypterDecrypter after round trip", rebornEncrypterDecrypter);

        assertTrue("EncrypterDecrypter was not an EncrypterDecrypterScrypt", rebornEncrypterDecrypter instanceof EncrypterDecrypterScrypt);
        
        EncrypterDecrypterScrypt rebornEncrypterDecrypterScrypt = (EncrypterDecrypterScrypt)rebornEncrypterDecrypter;
        assertEquals("Wrong N parameter", ScryptParameters.DEFAULT_N, rebornEncrypterDecrypterScrypt.getScryptParameters().getN());
        assertEquals("Wrong R parameter", ScryptParameters.DEFAULT_R, rebornEncrypterDecrypterScrypt.getScryptParameters().getR());
        assertEquals("Wrong P parameter", ScryptParameters.DEFAULT_P, rebornEncrypterDecrypterScrypt.getScryptParameters().getP());
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }

    @Test
    public void testNonDefaultScryptParameters() throws IOException {
        // Non default scrypt parameters.
        int n = 32768;
        int r = 8;
        int p = 3;
        
        byte[] salt = new byte[ScryptParameters.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        ScryptParameters scryptParameters = new ScryptParameters(salt, n, r, p);
        EncrypterDecrypter testEncrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);
        
        // Create an encrypted wallet with nondefault scrypt parameters.
        File temporaryWallet = File.createTempFile(TEST_SCRYPT_PARAMETERS + "2", ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), testEncrypterDecrypter);
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);

        newWallet.encrypt(newWallet.getEncrypterDecrypter().deriveKey(WALLET_PASSWORD));
        
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_SCRYPT_PARAMETERS + "2");
        
        // Save the wallet and read it back in again.
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);
        File walletInfoFile = new File(walletInfoFileAsString);

        // Load the wallet and check the default scrypt parameters
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        
        EncrypterDecrypter rebornEncrypterDecrypter = perWalletModelDataReborn.getWallet().getEncrypterDecrypter();
        assertNotNull("There was no encrypterDecrypter after round trip", rebornEncrypterDecrypter);

        assertTrue("EncrypterDecrypter was not an EncrypterDecrypterScrypt", rebornEncrypterDecrypter instanceof EncrypterDecrypterScrypt);
        
        EncrypterDecrypterScrypt rebornEncrypterDecrypterScrypt = (EncrypterDecrypterScrypt)rebornEncrypterDecrypter;
        assertEquals("Wrong N parameter", n, rebornEncrypterDecrypterScrypt.getScryptParameters().getN());
        assertEquals("Wrong R parameter", r, rebornEncrypterDecrypterScrypt.getScryptParameters().getR());
        assertEquals("Wrong P parameter", p, rebornEncrypterDecrypterScrypt.getScryptParameters().getP());
        
        deleteWalletAndCheckDeleted(perWalletModelDataReborn, newWalletFile, walletInfoFile);
    }
    
    private void deleteWalletAndCheckDeleted(PerWalletModelData perWalletModelData, File walletFile, File walletInfoFile) {
        // Delete wallet and check it is deleted.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelData);
        assertTrue(!walletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
    
    public void testIsSerialisdWallet() throws Exception {
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String serialisedWalletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
        + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_SERIALISED1;

        File serialisedWalletFile = new File(serialisedWalletName);;

        FileHandler fileHandler = new FileHandler(controller);
        assertTrue(fileHandler.isWalletSerialised(serialisedWalletFile));

        String protobufWalletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
        + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_PROTOBUF1;

        File protobufWalletFile = new File(protobufWalletName);;
        assertTrue(!fileHandler.isWalletSerialised(protobufWalletFile));
    }
    
    
    @Test
    public void testCannotLoadOrSaveFutureWalletVersions() throws IOException {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        FileHandler fileHandler = new FileHandler(controller);

        File temporaryWallet = File.createTempFile(TEST_WALLET_VERSION_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet with a future wallet version
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        newKey = new ECKey();
        newWallet.keychain.add(newKey);
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.FUTURE);
        
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
    public void testMinorWalletVersion() throws IOException {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        FileHandler fileHandler = new FileHandler(controller);

        File temporaryWallet = File.createTempFile(TEST_MINOR_WALLET_VERSION_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet with minor wallet set.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        newKey = new ECKey();
        newWallet.keychain.add(newKey);
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletMajorVersion.PROTOBUF_ENCRYPTED);
        
        perWalletModelData.setWalletInfo(walletInfo);
       
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_MINOR_WALLET_VERSION_PREFIX);
        
        // Save the wallet
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the major and minor version gets round tripped.
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(new File(newWalletFilename));
        assertNotNull(perWalletModelDataReborn);
        
        WalletInfo rebornWalletInfo = perWalletModelDataReborn.getWalletInfo();
        assertEquals("Wallet major version was not roundtripped", WalletMajorVersion.PROTOBUF_ENCRYPTED, rebornWalletInfo.getWalletMajorVersion());;
    }

}
