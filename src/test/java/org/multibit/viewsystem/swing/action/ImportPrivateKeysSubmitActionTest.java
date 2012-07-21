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
import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncryptableWallet;
import org.multibit.file.PrivateKeysHandlerTest;
import org.multibit.viewsystem.swing.view.ImportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import com.google.bitcoin.core.ECKey;

public class ImportPrivateKeysSubmitActionTest extends TestCase {   
    
    private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";
    private static final String EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN = "No import file was chosen. Nothing to do.";
    private static final String EXPECTED_PRIVATE_KEY_UNLOCK_FAILED = "The private keys unlock failed. The error was \"Could not decrypt input string\". ";
    private static final String EXPECTED_IMPORTING_PRIVATE_KEYS = "Importing private keys...";
    private static final String EXPECTED_IMPORTED_PRIVATE_KEYS = "Importing private keys... completed successfully";
     
    private static final int DELAY_TO_COMPLETE_IMPORT = 1000; // milliseconds
    
    //public static final char[] IMPORT_FILE_PASSWORD = "the quick brown fox jumps over the lazy dog 0123456789".toCharArray();
    public static final char[] WALLET_PASSWORD = "the unbelievable lightness of being".toCharArray();
    public static final char[] WRONG_PASSWORD = "this is the wrong password".toCharArray();
    
    @Test
    public void testImportUnencryptedPrivateKeysWithUnencryptedWallet() throws Exception { 
        // Create MultiBit controller.
        MultiBitController controller = ActionTestUtils.createController();
        
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
        
         // Execute - this is with an unencrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after default import execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText());    
        
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
        assertTrue("Wrong message after unencrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after unencrypted import should have completed", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText());    
              
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);
    }
    
    @Test
    public void testImportEncryptedPrivateKeysWithUnencryptedWallet() throws Exception { 
        // Create MultiBit controller.
        MultiBitController controller = ActionTestUtils.createController();
        
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
        
         // Execute - this is with an unencrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after default import execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText());    
        
        // Set the input file name - this is an encrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testEncryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.ENCRYPTED_TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testEncryptedPrivateKeysFile);
 
        // Execute - this should now complain that it could not decrypt the encrypted key export file.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after no password set execute", EXPECTED_PRIVATE_KEY_UNLOCK_FAILED, importPanel.getMessageText());     
 
        // Set the import file password.
        importPanel.setImportFilePassword(PrivateKeysHandlerTest.ENCRYPTED_TEST1_PASSWORD);
 
        // Execute = this should actually import the encrypted export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after encrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after encrypted import is good to go", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText());    
               
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);
    }
    
    @Test
    public void testImportUnencryptedPrivateKeysWithEncryptedWallet() throws Exception { 
        // Create MultiBit controller.
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new encrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportUnencryptedPrivateKeysWithUnencryptedWallet", true, WALLET_PASSWORD);

        assertTrue("Wallet is not encrypted when it should be",  ((EncryptableWallet)controller.getModel().getActiveWallet()).isCurrentlyEncrypted());
        
        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Switch off blockchain replay after key import.
        importAction.setPerformReplay(false);

        assertNotNull("importAction was not created successfully", importAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        
         // Execute - this is with an encrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message1 after default export execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText());     
      
        // Set the input file name - this is an unencrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testUnencryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testUnencryptedPrivateKeysFile);

        // Execute - this is with an encrypted wallet but no wallet password set.
        importAction.actionPerformed(null);
 
        assertEquals("Wrong message after default import execute", EXPECTED_ENTER_THE_WALLET_PASSWORD, importPanel.getMessageText());    
        
        // Set the wallet password.
        importPanel.setWalletPassword(WALLET_PASSWORD);

        // Execute = this should actually import the export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after unencrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after unencrypted import should have completed", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText());    
              
        assertTrue("Wallet is not encrypted when it should be",  ((EncryptableWallet)controller.getModel().getActiveWallet()).isCurrentlyEncrypted());
        
        // Decrypt wallet to enable private key comparision
        ((EncryptableWallet)controller.getModel().getActiveWallet()).decrypt(WALLET_PASSWORD);
        
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);
    }
    
    @Test
    public void testImportEncryptedPrivateKeysWithEnencryptedWallet() throws Exception { 
        // Create MultiBit controller.
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportUnencryptedPrivateKeysWithUnEncryptedWallet", true, WALLET_PASSWORD);

        assertTrue("Wallet is not encrypted when it should be",  ((EncryptableWallet)controller.getModel().getActiveWallet()).isCurrentlyEncrypted());
        
        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Switch off blockchain replay after key import.
        importAction.setPerformReplay(false);

        assertNotNull("importAction was not created successfully", importAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        
         // Execute - this is with an encrypted wallet and default settings.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after default import execute", EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN, importPanel.getMessageText());    
        
        // Set the input file name - this is an encrypted key export file.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
        String testEncryptedPrivateKeysFile = testDirectory + File.separator + PrivateKeysHandlerTest.ENCRYPTED_TEST1_PRIVATE_KEYS_FILE;

        importPanel.setOutputFilename(testEncryptedPrivateKeysFile);
 
        // Execute - this is with an encrypted wallet but no wallet password set.
        importAction.actionPerformed(null);
 
        assertEquals("Wrong message after default import execute", EXPECTED_ENTER_THE_WALLET_PASSWORD, importPanel.getMessageText());    
        
        // Set the wallet password.
        importPanel.setWalletPassword(WALLET_PASSWORD);

        // Execute - this should now complain that it could not decrypt the encrypted key export file.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after no password set execute", EXPECTED_PRIVATE_KEY_UNLOCK_FAILED, importPanel.getMessageText());     
 
        // Set the import file password.
        importPanel.setImportFilePassword(PrivateKeysHandlerTest.ENCRYPTED_TEST1_PASSWORD);
 
        // Execute = this should actually import the encrypted export file.
        importAction.actionPerformed(null);
        
        // The import is on on its own thread so it may or may not have completed straight after the action is performed.
        assertTrue("Wrong message after encrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS.equals(importPanel.getMessageText()) || EXPECTED_IMPORTED_PRIVATE_KEYS.equals(importPanel.getMessageText()));    

        // Wait a while and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after encrypted import is good to go", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText());    
             
        assertTrue("Wallet is not encrypted when it should be",  ((EncryptableWallet)controller.getModel().getActiveWallet()).isCurrentlyEncrypted());
        
        // Decrypt wallet to enable private key comparision.
        ((EncryptableWallet)controller.getModel().getActiveWallet()).decrypt(WALLET_PASSWORD);
        
        // Check every key on the expected list is now on the wallet.
        checkEveryExpectedKeyIsPresent(controller);
    }
    
    private void checkEveryExpectedKeyIsPresent(MultiBitController controller) {
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
