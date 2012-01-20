/**
 * Copyright 2011 multibit.org
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
package org.multibit.network;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Constants;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FileHandlerTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(FileHandlerTest.class);

    public static final String WALLET_TESTDATA_DIRECTORY = "wallets";

    public static final String WALLET_TEST1 = "test1.wallet";
    public static final BigInteger WALLET_TEST1_BALANCE = new BigInteger("6700000");;

    public static final String WALLET_TEST2 = "test2.wallet";
    public static final BigInteger WALLET_TEST2_BALANCE = new BigInteger("2000000");;

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

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator + WALLET_TESTDATA_DIRECTORY + File.separator
                + WALLET_TEST1;

        File walletFile = new File(walletName);
        try {
            walletFile.createNewFile();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            log.error(e.getMessage(), e);

        }
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

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator + WALLET_TESTDATA_DIRECTORY + File.separator
                + WALLET_TEST2;

        File walletFile = new File(walletName);
        try {
            walletFile.createNewFile();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            log.error(e.getMessage(), e);

        }
        assertTrue(walletFile.exists());
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);

        assertNotNull(perWalletModelData);

        assertEquals(WALLET_TEST2_BALANCE, perWalletModelData.getWallet().getBalance());
    }
}
