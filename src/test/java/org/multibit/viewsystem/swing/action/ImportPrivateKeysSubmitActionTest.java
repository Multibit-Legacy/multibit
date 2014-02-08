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

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.PrivateKeysHandlerTest;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.viewsystem.swing.view.panels.ImportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import com.google.dogecoin.core.ECKey;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;

public class ImportPrivateKeysSubmitActionTest extends TestCase {   
    
    private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";
    private static final String EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN = "No import file was chosen. Nothing to do.";
    private static final String EXPECTED_PRIVATE_KEY_UNLOCK_FAILED = "The private keys unlock failed. The error was \"Could not decrypt input string\". ";
    private static final String EXPECTED_IMPORTING_PRIVATE_KEYS = "Importing private keys...";
    private static final String EXPECTED_IMPORTED_PRIVATE_KEYS = "Importing private keys... completed successfully";
     
    private static final int DELAY_TO_COMPLETE_IMPORT = 5000; // milliseconds
    
    public static final CharSequence WALLET_PASSWORD = "the unbelievable lightness of being";

    @Test
    public void testImportUnencryptedPrivateKeysWithUnencryptedWallet() throws Exception { 
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new unencrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportUnencryptedPrivateKeysWithUnencryptedWallet", false, null);

        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Switch off blockchain replay after key import.
        importAction.setPerformReplay(false);

        assertNotNull("importAction was not created successfully", importAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is enabled when it should not be", !importPanel.isWalletPasswordFieldEnabled());

         // Execute - this is with an unencrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after default import execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText1());    
        
        // Set the input file name - this is an unencrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testUnencryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testUnencryptedPrivateKeysFile);
 
        // Execute = this should actually import the export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after unencrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText1()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText1()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after unencrypted import should have completed", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText1());    
              
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);

        // Check the label on the original key is still there.
        assertEquals("The label on the original address was not correct after import", ActionTestUtils.LABEL_OF_ADDRESS_ADDED, controller.getModel().getActivePerWalletModelData().getWalletInfo().getReceivingAddresses().get(0).getLabel());
    }
    
    @Test
    public void testImportEncryptedPrivateKeysWithUnencryptedWallet() throws Exception { 
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportUnencryptedPrivateKeysWithUnEncryptedWallet", false, null);

        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Switch off blockchain replay after key import.
        importAction.setPerformReplay(false);

        assertNotNull("importAction was not created successfully", importAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is enabled when it should not be", !importPanel.isWalletPasswordFieldEnabled());
        
         // Execute - this is with an unencrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after default import execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText1());    
        
        // Set the input file name - this is an encrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testEncryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.ENCRYPTED_TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testEncryptedPrivateKeysFile);
 
        // Execute - this should now complain that it could not decrypt the encrypted key export file.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after no password set execute", EXPECTED_PRIVATE_KEY_UNLOCK_FAILED, importPanel.getMessageText1());     
 
        // Set the import file password.
        importPanel.setImportFilePassword(PrivateKeysHandlerTest.ENCRYPTED_TEST1_PASSWORD);
 
        // Execute = this should actually import the encrypted export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after encrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText1()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText1()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after encrypted import is good to go", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText1());    
               
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);

        // Check the label on the original key is still there.
        assertEquals("The label on the original address was not correct after import", ActionTestUtils.LABEL_OF_ADDRESS_ADDED, controller.getModel().getActivePerWalletModelData().getWalletInfo().getReceivingAddresses().get(0).getLabel());
    }
    
    @Test
    public void testImportUnencryptedPrivateKeysWithEncryptedWallet() throws Exception { 
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new encrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportUnencryptedPrivateKeysWithUnencryptedWallet", true, WALLET_PASSWORD);

        assertTrue("Wallet is not encrypted when it should be",  controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        
        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Switch off blockchain replay after key import.
        importAction.setPerformReplay(false);

        assertNotNull("importAction was not created successfully", importAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is not enabled when it should be", importPanel.isWalletPasswordFieldEnabled());

         // Execute - this is with an encrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message1 after default export execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText1());     
      
        // Set the input file name - this is an unencrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testUnencryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testUnencryptedPrivateKeysFile);

        // Execute - this is with an encrypted wallet but no wallet password set.
        importAction.actionPerformed(null);
 
        assertEquals("Wrong message after default import execute", EXPECTED_ENTER_THE_WALLET_PASSWORD, importPanel.getMessageText1());    
        
        // Set the wallet password.
        importPanel.setWalletPassword(WALLET_PASSWORD);

        // Execute = this should actually import the export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after unencrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText1()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText1()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after unencrypted import should have completed", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText1());    
              
        assertTrue("Wallet is not encrypted when it should be",  controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        
        // Decrypt wallet to enable private key comparision
        controller.getModel().getActiveWallet().decrypt(controller.getModel().getActiveWallet().getKeyCrypter().deriveKey(WALLET_PASSWORD));
        
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);

        // Check the label on the original key is still there.
        assertEquals("The label on the original address was not correct after import", ActionTestUtils.LABEL_OF_ADDRESS_ADDED, controller.getModel().getActivePerWalletModelData().getWalletInfo().getReceivingAddresses().get(0).getLabel());
    }
    
    @Test
    public void testImportEncryptedPrivateKeysWithEncryptedWallet() throws Exception { 
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportUnencryptedPrivateKeysWithUnEncryptedWallet", true, WALLET_PASSWORD);

        assertTrue("Wallet is not encrypted when it should be",  controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        
        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Switch off blockchain replay after key import.
        importAction.setPerformReplay(false);

        assertNotNull("importAction was not created successfully", importAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is not enabled when it should be", importPanel.isWalletPasswordFieldEnabled());
        
         // Execute - this is with an encrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after default import execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText1());    
        
        // Set the input file name - this is an encrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testEncryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.ENCRYPTED_TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testEncryptedPrivateKeysFile);
 
        // Execute - this is with an encrypted wallet but no wallet password set.
        importAction.actionPerformed(null);
 
        assertEquals("Wrong message after default import execute", EXPECTED_ENTER_THE_WALLET_PASSWORD, importPanel.getMessageText1());    
        
        // Set the wallet password.
        importPanel.setWalletPassword(WALLET_PASSWORD);

        // Execute - this should now complain that it could not decrypt the encrypted key export file.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after no password set execute", EXPECTED_PRIVATE_KEY_UNLOCK_FAILED, importPanel.getMessageText1());     
 
        // Set the import file password.
        importPanel.setImportFilePassword(PrivateKeysHandlerTest.ENCRYPTED_TEST1_PASSWORD);
 
        // Execute = this should actually import the encrypted export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after encrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText1()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText1()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after encrypted import is good to go", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText1());    
             
        assertTrue("Wallet is not encrypted when it should be",  controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        
        // Decrypt wallet to enable private key comparision.
        controller.getModel().getActiveWallet().decrypt(controller.getModel().getActiveWallet().getKeyCrypter().deriveKey(WALLET_PASSWORD));
        
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);

        // Check the label on the original key is still there.
        assertEquals("The label on the original address was not correct after import", ActionTestUtils.LABEL_OF_ADDRESS_ADDED, controller.getModel().getActivePerWalletModelData().getWalletInfo().getReceivingAddresses().get(0).getLabel());
    }
    
    @Test
    public void testNoWalletSelected() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;

        // This test runs against an empty PerWalletModelDataList.
        assertTrue("There was an active wallet when there should not be", controller.getModel().thereIsNoActiveWallet());

        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPrivateKeysPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importPrivateKeysSubmitAction = importPrivateKeysPanel.getImportPrivateKeysSubmitAction();

        assertNotNull("importPrivateKeysSubmitAction was not created successfully", importPrivateKeysSubmitAction);

        // Execute.
        importPrivateKeysSubmitAction.actionPerformed(null);
        Object[] messages = MessageManager.INSTANCE.getMessages().toArray();
        assertTrue("There were no messages but there should have been", messages != null && messages.length > 0);
        assertEquals("Wrong message after receive dogecoin confirm with no active wallet", ResetTransactionsSubmitActionTest.EXPECTED_NO_WALLET_IS_SELECTED, ((Message)messages[messages.length - 1]).getText());
    }
    
    private void checkEveryExpectedKeyIsPresent(BitcoinController controller) {
        // Check every key on the expected list is now on the wallet.
        for (int i = 0; i < PrivateKeysHandlerTest.EXPECTED_ADDRESSES_FOR_TEST1_WALLET.length; i++) {
            boolean foundIt = false;
            for (ECKey ecKey : controller.getModel().getActiveWallet().getKeychain()) {
                if (PrivateKeysHandlerTest.EXPECTED_ADDRESSES_FOR_TEST1_WALLET[i].equals(ecKey.toAddress(controller.getModel().getNetworkParameters()).toString())) {
                    foundIt = true;
                    break;
                }
            }
            
            if (!foundIt) {
                fail("The address " + PrivateKeysHandlerTest.EXPECTED_ADDRESSES_FOR_TEST1_WALLET[i] + " was not found on the wallet after an import.");
            }
        }
    }
}
