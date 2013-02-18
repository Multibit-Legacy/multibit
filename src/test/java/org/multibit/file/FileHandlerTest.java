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

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Constants;
import org.multibit.Localiser;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;


public class FileHandlerTest extends TestCase {
    private static final String WALLET_TESTDATA_DIRECTORY = "wallets";

    private static final String WALLET_PROTOBUF1 = "protobuf1.wallet";

    private static final String WALLET_FUTURE = "future.wallet";

    private static final String WALLET_TEST1 = "test1.wallet";
    private static final BigInteger WALLET_TEST1_BALANCE = new BigInteger("6700000");;

    private static final String WALLET_TEST2 = "test2.wallet";
    private static final BigInteger WALLET_TEST2_BALANCE = new BigInteger("2000000");;

    private static final String TEST_CREATE_AND_DELETE1_WALLET_PREFIX = "testCreateAndDelete1";
    
    private static final String TEST_CREATE_SERIALISED_PREFIX = "testCreateSerialised";

    private static final String TEST_CREATE_PROTOBUF_PREFIX = "testCreateProtobuf";
    
    private static final String TEST_WALLET_VERSION_PREFIX = "testCannotLoadOrSaveFutureWalletVersions";

    @Test
    public void testLoadTest1() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        final FileHandler fileHandler = new FileHandler(controller);

        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_TEST1;

        File walletFile = new File(walletName);
        walletFile.createNewFile();

        assertTrue(walletFile.exists());
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);

        assertNotNull(perWalletModelData);

        assertEquals(WALLET_TEST1_BALANCE, perWalletModelData.getWallet().getBalance());
    }

    @Test
    public void testLoadTest2() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        final FileHandler fileHandler = new FileHandler(controller);

        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_TEST2;

        File walletFile = new File(walletName);

        walletFile.createNewFile();

        assertTrue(walletFile.exists());
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);

        assertNotNull(perWalletModelData);

        assertEquals(WALLET_TEST2_BALANCE, perWalletModelData.getWallet().getBalance());
    }

    @Test
    public void testCreateAndDeleteWallet1() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        final FileHandler fileHandler = new FileHandler(controller);

        File temporaryWallet = File.createTempFile(TEST_CREATE_AND_DELETE1_WALLET_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new wallet.
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        perWalletModelData.setWalletInfo(new WalletInfo(newWalletFilename, WalletVersion.PROTOBUF));
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_AND_DELETE1_WALLET_PREFIX);
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded.
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_AND_DELETE1_WALLET_PREFIX, perWalletModelDataReborn.getWalletDescription());

        // Delete wallet.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelDataReborn);
        assertTrue(!newWalletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
    
    @Test
    public void testCreateSerialisedWallet() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        final FileHandler fileHandler = new FileHandler(controller);

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
    public void testCreateProtobufWallet() throws IOException {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        final FileHandler fileHandler = new FileHandler(controller);

        File temporaryWallet = File.createTempFile(TEST_CREATE_PROTOBUF_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();

        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // Create a new protobuf wallet.
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
        perWalletModelData.setWalletDescription(TEST_CREATE_PROTOBUF_PREFIX);
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // Check the wallet and wallet info file exists.
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());

        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());

        // Check wallet can be loaded and is still protobuf.
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_PROTOBUF_PREFIX, perWalletModelDataReborn.getWalletDescription());
        assertEquals(2, perWalletModelDataReborn.getWallet().keychain.size());

        assertEquals(WalletVersion.PROTOBUF, perWalletModelDataReborn.getWalletInfo().getWalletVersion());
        
        // Delete wallet.
        fileHandler.deleteWalletAndWalletInfo(perWalletModelDataReborn);
        assertTrue(!newWalletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
    
    public void testIsSerialisdWallet() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;

        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String serialisedWalletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
        + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_TEST1;

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
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        final FileHandler fileHandler = new FileHandler(controller);

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
        WalletInfo walletInfo = new WalletInfo(newWalletFilename, WalletVersion.FUTURE);
        
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
}
