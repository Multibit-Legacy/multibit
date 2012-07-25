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
import org.multibit.crypto.EncrypterDecrypter;
import org.multibit.crypto.EncrypterDecrypterScrypt;
import org.multibit.crypto.ScryptParameters;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;

public class FileHandlerTest extends TestCase {
    private final String WALLET_TESTDATA_DIRECTORY = "wallets";

    private final String WALLET_PROTOBUF1 = "protobuf1.wallet";

    private final String WALLET_SERIALISED1 = "serialised1.wallet";
    private final BigInteger WALLET_SERIALISED1_BALANCE = new BigInteger("6700000");;

    private final String WALLET_SERIALISED2 = "serialised2.wallet";
    private final BigInteger WALLET_SERIALISED2_BALANCE = new BigInteger("2000000");;


    private final String TEST_CREATE_SERIALISED_PREFIX = "testCreateSerialised";

    private final String TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX = "testCreateUnencryptedProtobuf";

    private final String TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX = "testCreateEncryptedProtobuf";
    
    private final char[] WALLET_PASSWORD = "horatio nelson 123".toCharArray();

    private SecureRandom secureRandom;
    
    private EncrypterDecrypter encrypterDecrypter;

    @Before
    public void setUp() throws Exception {
        secureRandom = new SecureRandom();
        
        byte[] salt = new byte[ScryptParameters.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        ScryptParameters scryptParameters = new ScryptParameters(salt);
        encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);
    }
    
    @Test
    public void testLoadSerialised1() throws IOException {
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        FileHandler fileHandler = new FileHandler(controller);

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
        assertTrue("Wallet type is WalletType.ENCRYPTED but it should not be", perWalletModelData.getWallet().getWalletType() == WalletType.UNENCRYPTED);
    }

    @Test
    public void testLoadSerialised2() throws IOException {
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        FileHandler fileHandler = new FileHandler(controller);

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
        assertTrue("Wallet type is WalletType.ENCRYPTED but it should not be", perWalletModelData.getWallet().getWalletType() == WalletType.UNENCRYPTED);
    }
    
    @Test
    public void testCreateSerialisedWallet() throws IOException {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        FileHandler fileHandler = new FileHandler(controller);

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
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletVersion.SERIALIZED);
        
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

        assertEquals(WalletVersion.SERIALIZED, perWalletModelDataReborn.getWalletInfo().getWalletVersion());
        
        // Delete wallet.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelDataReborn);
        assertTrue(!newWalletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
    
    @Test
    public void testCreateProtobufUnencryptedWallet() throws IOException {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        FileHandler fileHandler = new FileHandler(controller);

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
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletVersion.PROTOBUF);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_UNENCRYPTED_PROTOBUF_PREFIX);
        
        // Check the wallet status before it is written out and reborn.
        assertEquals(WalletVersion.PROTOBUF, perWalletModelData.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not UNENCRYPTED when it should be", perWalletModelData.getWallet().getWalletType() == WalletType.UNENCRYPTED);
        
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

        assertEquals(WalletVersion.PROTOBUF, perWalletModelDataReborn.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not UNENCRYPTED when it should be", perWalletModelDataReborn.getWallet().getWalletType() == WalletType.UNENCRYPTED);
        
        assertTrue("Wallet isCurrentlyEncrypted when it should not be", !perWalletModelDataReborn.getWallet().isCurrentlyEncrypted());
        
        // Delete wallet.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelDataReborn);
        assertTrue(!newWalletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
    
    @Test
    public void testCreateProtobufEncryptedWallet() throws IOException {
        MultiBitController controller = new MultiBitController();
        @SuppressWarnings("unused")
        MultiBitModel model = new MultiBitModel(controller);
        FileHandler fileHandler = new FileHandler(controller);

        // Create an encrypted wallet.
        File temporaryWallet = File.createTempFile(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        Wallet newWallet = new Wallet(NetworkParameters.prodNet(), encrypterDecrypter);
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        newKey = new ECKey();
        newWallet.keychain.add(newKey);
        
        newWallet.encrypt(WALLET_PASSWORD);
        
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletVersion.PROTOBUF);
        
        perWalletModelData.setWalletInfo(walletInfo);
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_ENCRYPTED_PROTOBUF_PREFIX);
        
        // Check the wallet status before it is written out and reborn.
        assertEquals(WalletVersion.PROTOBUF, perWalletModelData.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not ENCRYPTED when it should be", perWalletModelData.getWallet().getWalletType() == WalletType.ENCRYPTED);
        
        assertTrue("Wallet isCurrentlyEncrypted is false when it should be true", perWalletModelData.getWallet().isCurrentlyEncrypted());

        // Get the keys of the wallet and check that all the keys are EncryptableKeys and encrypted.
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

        assertEquals(WalletVersion.PROTOBUF, perWalletModelDataReborn.getWalletInfo().getWalletVersion());
        assertTrue("Wallet is not ENCRYPTED when it should be", perWalletModelDataReborn.getWallet().getWalletType() == WalletType.ENCRYPTED);
        
        assertTrue("Wallet isCurrentlyEncrypted is false when it should be true", perWalletModelDataReborn.getWallet().isCurrentlyEncrypted());
        
        // Get the keys out the reborn wallet and check that all the keys are EncryptableKeys and encrypted.
        ArrayList<ECKey> rebornKeys = perWalletModelDataReborn.getWallet().getKeychain();
        for (ECKey key : rebornKeys) {
            assertTrue("Key is not encrypted when it should be", key.isEncrypted());
        }
        
        // Delete wallet.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelDataReborn);
        assertTrue(!newWalletFile.exists());
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
}
