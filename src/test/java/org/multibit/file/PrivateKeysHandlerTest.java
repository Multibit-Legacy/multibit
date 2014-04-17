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



import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import junit.framework.TestCase;
import org.junit.Test;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;



public class PrivateKeysHandlerTest extends TestCase {
    public static final String PRIVATE_KEYS_TESTDATA_DIRECTORY = "privateKeys";
    public static final String WALLETS_TESTDATA_DIRECTORY = "wallets";
    public static final String BCI_TESTDATA_DIRECTORY = "BCI-backups";

    public static final String TEST1_WALLET_FILE = "test1.wallet";
    public static final String TEST1_PRIVATE_KEYS_FILE = "test1.key";
    public static final String EXPECTED_TEST1_PRIVATE_KEYS_FILE = "expectedTest1.key";

    public static final CharSequence ENCRYPTED_TEST1_PASSWORD = "password";
    public static final String ENCRYPTED_TEST1_PRIVATE_KEYS_FILE = "encryptedTest1.key";

    public static final String[] EXPECTED_ADDRESSES_FOR_TEST1_WALLET = new String[]{"15ZLe7GCAfdTTMMkbm38KTtahq9y549rB2", 
        "1KPNYRuDJoBexHAcCwuA5EhGdzoVHTRNTX", "162zJokk8matsjGGGmyJTCBLTDc3juRxEs", "13FHXieWVDMMPuVgx9mRYmMEJTRrrSU3Ct",
        "166ofzumkuBB8gpDqd3usn3PypRXA4wTS6", "1NcfaCrfNTRMBhCrF8uw8W6U6sRWYAH6QK", "13T5wgZj4VsWx5np4L2NNkWR8bLxzYz3b6"};

    @Test
    public void testExport() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;

        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(NetworkParameters.prodNet());
        assertNotNull(privateKeysHandler);
        
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testWalletFile = testDirectory + File.separator + TEST1_WALLET_FILE;
        String testPrivateKeysFile = testDirectory + File.separator + TEST1_PRIVATE_KEYS_FILE;
        String expectedPrivateKeysFile = testDirectory + File.separator + EXPECTED_TEST1_PRIVATE_KEYS_FILE;

        // load up the test wallet
        FileHandler fileHandler = new FileHandler(controller);
        Wallet testWallet = fileHandler.loadFromFile(new File(testWalletFile)).getWallet();

        assertNotNull(testWallet);

        // write private keys file for wallet - no blockchain - no passwords
        privateKeysHandler.exportPrivateKeys(new File(testPrivateKeysFile), testWallet, null, false, null, null);

        // read in the created private keys file and the expected one and
        // compare
        String expectedFileContents = readFile(new File(expectedPrivateKeysFile));
        String actualFileContents = readFile(new File(testPrivateKeysFile));

        assertEquals(expectedFileContents, actualFileContents);
    }
    
    @Test
    public void testImport() throws Exception {
        NetworkParameters prodNet = NetworkParameters.prodNet();
        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(prodNet);
        assertNotNull(privateKeysHandler);
        
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String expectedPrivateKeysFile = testDirectory + File.separator + EXPECTED_TEST1_PRIVATE_KEYS_FILE;

        Collection<PrivateKeyAndDate> parsedPrivateKeysAndDates = privateKeysHandler.readInPrivateKeys(new File(expectedPrivateKeysFile), null);
        
        //System.out.println("PrivateKeysHandlerTest#testImport parsedPrivateKeysAndDates = '" + parsedPrivateKeysAndDates + "'");
        assertNotNull(parsedPrivateKeysAndDates);
        assertEquals(7, parsedPrivateKeysAndDates.size());
        
        int count = 0;
        for (PrivateKeyAndDate privateKeyAndDate : parsedPrivateKeysAndDates) {
            assertEquals(EXPECTED_ADDRESSES_FOR_TEST1_WALLET[count], privateKeyAndDate.getKey().toAddress(prodNet).toString());
            count++;
        }
    }

    private String readFile(File inputFile) throws IOException {
        StringBuffer contents = new StringBuffer();
        BufferedReader reader = null;
        String lineSeparator = System.getProperty("line.separator");
        try {
            reader = new BufferedReader(new FileReader(inputFile));

            String text = null;

            // repeat until all lines is read
            while ((text = reader.readLine()) != null) {
                contents.append(text).append(lineSeparator);
            }
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
        return contents.toString();
    }
}
