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
package org.multibit.viewsystem.swing.action;

import java.io.File;
import java.io.IOException;

import javax.swing.JPasswordField;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;
import org.multibit.viewsystem.swing.view.CreateNewReceivingAddressDialog;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;

public class CreateNewReceivingAddressSubmitActionTest extends TestCase {
    public static final String PRIVATE_KEYS_TESTDATA_DIRECTORY = "privateKeys";
    public static final String WALLETS_TESTDATA_DIRECTORY = "wallets";

    public static final String TEST1_WALLET_FILE = "test1.wallet";
    public static final String TEST1_PRIVATE_KEYS_FILE = "test1.key";
    public static final String EXPECTED_TEST1_PRIVATE_KEYS_FILE = "expectedTest1.key";
    
    public static final String[] EXPECTED_ADDRESSES_FOR_TEST1_WALLET = new String[]{"15ZLe7GCAfdTTMMkbm38KTtahq9y549rB2", 
        "1KPNYRuDJoBexHAcCwuA5EhGdzoVHTRNTX", "162zJokk8matsjGGGmyJTCBLTDc3juRxEs", "13FHXieWVDMMPuVgx9mRYmMEJTRrrSU3Ct",
        "166ofzumkuBB8gpDqd3usn3PypRXA4wTS6", "1NcfaCrfNTRMBhCrF8uw8W6U6sRWYAH6QK", "13T5wgZj4VsWx5np4L2NNkWR8bLxzYz3b6"};

    public static final String BLOCKCHAIN_NO_PASSWORD_WALLET_FILE = "blockchain_test_nopassword.json";
    public static final String BLOCKCHAIN_NO_PASSWORD = "";
    public static final String[] EXPECTED_ADDRESSES_FOR_BLOCKCHAIN_WALLET = new String[]{"1HyWjW2gfp8NPG2jj3399DBA1kxQP5SYa9"};

    public static final String BLOCKCHAIN_MAIN_PASSWORD = "1234567890";
    public static final String BLOCKCHAIN_SECOND_PASSWORD = "123456789";

    public static final String BLOCKCHAIN_SINGLE_PASSWORD_WALLET_FILE = "blockchain_test.aes.json";
    public static final String BLOCKCHAIN_DOUBLE_PASSWORD_WALLET_FILE = "blockchain_test_double_encrypted.aes.json";
    
    @Test
    public void testAddReceivingAddressesWithNonEncryptedWallet() throws Exception {   
        // Create MultiBit controller.
        MultiBitController controller = createController();
        
        // Create a new wallet and put it in the model as the active wallet.
        createNewActiveWallet(controller, "testAddReceivingAddresses");

        // Create a new CreateNewReceivingAddressSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        
        JPasswordField walletPasswordField = new JPasswordField();
        CreateNewReceivingAddressDialog createNewDialog = new CreateNewReceivingAddressDialog(controller, null, null);
        CreateNewReceivingAddressSubmitAction createNewAction = new CreateNewReceivingAddressSubmitAction(controller, createNewDialog, walletPasswordField, NetworkParameters.prodNet());

        assertNotNull("createNewAction was not created successfully", createNewAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        
        // Execute the createNewAction - by default the createNewDialog sould be set to add one key.
        createNewAction.actionPerformed(null);
        assertEquals("Wrong number of keys after addition of default number of keys", 2, controller.getModel().getActiveWallet().getKeychain().size());    
        
        // Add one address by selecting on the combo box.
        createNewDialog.getNumberOfAddresses().setSelectedItem(new Integer(1));
        createNewAction.actionPerformed(null);
        assertEquals("Wrong number of keys after addition of 1 key", 3, controller.getModel().getActiveWallet().getKeychain().size());
        
        // Add five addresses by selecting on the combo box.
        createNewDialog.getNumberOfAddresses().setSelectedItem(new Integer(5));
        createNewAction.actionPerformed(null);
        assertEquals("Wrong number of keys after addition of 5 keys", 8, controller.getModel().getActiveWallet().getKeychain().size());   
        
        // Add twenty addresses by selecting on the combo box.
        createNewDialog.getNumberOfAddresses().setSelectedItem(new Integer(20));
        createNewAction.actionPerformed(null);
        assertEquals("Wrong number of keys after addition of 20 keys", 28, controller.getModel().getActiveWallet().getKeychain().size());  
        
        // Add one hundresd addresses by selecting on the combo box.
        createNewDialog.getNumberOfAddresses().setSelectedItem(new Integer(100));
        createNewAction.actionPerformed(null);
        assertEquals("Wrong number of keys after addition of 100 keys", 128, controller.getModel().getActiveWallet().getKeychain().size());    
    }
    
    private MultiBitController createController() {
       MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);
        
        controller.setLocaliser(localiser);
        controller.setModel(model);
        
        return controller;
    }
    
    private void createNewActiveWallet(MultiBitController controller, String descriptor) throws SecurityException, IOException {
        Wallet wallet = new Wallet(NetworkParameters.prodNet());
        wallet.getKeychain().add(new ECKey());

        PerWalletModelData perWalletModelData = new PerWalletModelData();
        perWalletModelData.setWallet(wallet);
        File directory = new File(".");
        String currentPath = directory.getCanonicalPath();

        // Write out the wallet and read it back in to satisfy the wallet modification date checking
        String walletFile = currentPath + File.separator + descriptor + ".wallet";
        perWalletModelData.setWalletInfo(new WalletInfo(walletFile, WalletVersion.PROTOBUF));
        perWalletModelData.setWalletFilename(walletFile);
        perWalletModelData.setWalletDescription(descriptor);
        controller.getModel().addAndMakeActiveWallet(perWalletModelData);
        FileHandler fileHandler = new FileHandler(controller);
        fileHandler.savePerWalletModelData(perWalletModelData, true);
        fileHandler.loadFromFile(new File(walletFile));

        assertEquals("The test wallet " + descriptor + " was not created properly", walletFile, controller.getModel().getActiveWalletFilename());
    }
}
