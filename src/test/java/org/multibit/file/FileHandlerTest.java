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
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;

public class FileHandlerTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(FileHandlerTest.class);

    public static final String WALLET_TESTDATA_DIRECTORY = "wallets";

    public static final String WALLET_TEST1 = "test1.wallet";
    public static final BigInteger WALLET_TEST1_BALANCE = new BigInteger("6700000");;

    public static final String WALLET_TEST2 = "test2.wallet";
    public static final BigInteger WALLET_TEST2_BALANCE = new BigInteger("2000000");;

    public static final String TEST_CREATE_AND_DELETE1_WALLET_PREFIX = "testCreateAndDelete1";

    @Test
    public void testLoadTest1() throws IOException {
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        FileHandler fileHandler = new FileHandler(controller);

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
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        FileHandler fileHandler = new FileHandler(controller);

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
        MultiBitController controller = new MultiBitController();
        MultiBitModel model = new MultiBitModel(controller);
        FileHandler fileHandler = new FileHandler(controller);

        File temporaryWallet = File.createTempFile(TEST_CREATE_AND_DELETE1_WALLET_PREFIX, ".wallet");
        temporaryWallet.deleteOnExit();
        
        String newWalletFilename = temporaryWallet.getAbsolutePath();

        // create a new wallet
        Wallet newWallet = new Wallet(NetworkParameters.prodNet());
        ECKey newKey = new ECKey();
        newWallet.keychain.add(newKey);
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        perWalletModelData.setWalletInfo(new WalletInfo(newWalletFilename));
        perWalletModelData.setWallet(newWallet);
        perWalletModelData.setWalletFilename(newWalletFilename);
        perWalletModelData.setWalletDescription(TEST_CREATE_AND_DELETE1_WALLET_PREFIX);
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // check the wallet and wallet info file exists
        File newWalletFile = new File(newWalletFilename);
        assertTrue(newWalletFile.exists());
        
        String walletInfoFileAsString = WalletInfo.createWalletInfoFilename(newWalletFilename);

        File walletInfoFile = new File(walletInfoFileAsString);
        assertTrue(walletInfoFile.exists());
 
        // check wallet can be loaded
        PerWalletModelData perWalletModelDataReborn = fileHandler.loadFromFile(newWalletFile);
        assertNotNull(perWalletModelDataReborn);
        assertEquals(BigInteger.ZERO, perWalletModelDataReborn.getWallet().getBalance());
        assertEquals(TEST_CREATE_AND_DELETE1_WALLET_PREFIX, perWalletModelDataReborn.getWalletDescription());
        
        // delete wallet
        fileHandler.deleteWalletAndWalletInfo(perWalletModelDataReborn);
        assertTrue(!newWalletFile.exists());
        assertTrue(!walletInfoFile.exists());
    }
}
