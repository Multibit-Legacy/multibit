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
import java.util.Collection;
import java.util.UUID;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Constants;
import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.file.PrivateKeyAndDate;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.file.PrivateKeysHandlerTest;
import org.multibit.viewsystem.swing.view.ExportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.ImportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import com.google.bitcoin.core.Utils;

public class ImportPrivateKeysSubmitActionTest extends TestCase {   
    
    private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";
    private static final String EXPECTED_NO_IMPORT_FILE_WAS_CHOSEN = "No import file was chosen. Nothing to do.";
    private static final String EXPECTED_PRIVATE_KEY_UNLOCK_FAILED = "The private keys unlock failed. The error was \"Could not decrypt input string\". ";
    private static final String EXPECTED_PASSWORDS_DO_NOT_MATCH = "The password and repeat password do not match";
    private static final String EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING = "Could not decrypt input string";
    private static final String EXPECTED_IMPORTING_PRIVATE_KEYS = "Importing private keys...";
    private static final String EXPECTED_IMPORTED_PRIVATE_KEYS = "Importing private keys... completed successfully";
     
    private static final int DELAY_TO_COMPLETE_IMPORT = 1000; // milliseconds
    
    //public static final char[] IMPORT_FILE_PASSWORD = "the quick brown fox jumps over the lazy dog 0123456789".toCharArray();
    public static final char[] WALLET_PASSWORD = "the unbelievable lightness of being".toCharArray();
    public static final char[] WRONG_PASSWORD = "this is the wrong password".toCharArray();
    
    @Test
    public void testImportPrivateKeysWithNonEncryptedWallet() throws Exception { 
        // Create MultiBit controller.
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testImportPrivateKeysWithNonEncryptedWallet", false, null);

        // Create a new ImportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ImportPrivateKeysPanel importPanel = new ImportPrivateKeysPanel(controller, null);
        ImportPrivateKeysSubmitAction importAction = importPanel.getImportPrivateKeysSubmitAction();
        
        // Swich off blockchain replay after key import.
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
 
        // Execute = this should actually import the unencrypted export file.
        importAction.actionPerformed(null);
        assertEquals("Wrong message after unencrypted import is good to go", EXPECTED_IMPORTING_PRIVATE_KEYS, importPanel.getMessageText());    
 
        // Wait a few seconds and the message should be that it has completed the import
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after unencrypted import should have completed", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText());    
        
        // Check the key export file contents have been added to the wallet.
//        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename2), null);
//        assertEquals("Wrong number of keys read in from unencrypted export file", 1, privateKeyAndDates.size());
//        assertEquals("Wrong private key read in from unencrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().get(0).getPrivKeyBytes()), 
//                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));   
                
        // Set the export file to be an encrypted key export file.
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

        // Wait a while and the message should be that it has completed the import.
        Thread.sleep(DELAY_TO_COMPLETE_IMPORT);
        assertEquals("Wrong message after unencrypted import should have completed", EXPECTED_IMPORTED_PRIVATE_KEYS, importPanel.getMessageText());    
        
        
        // Check the key export file contents have been added to the wallet.
    }
    
//    @Test
//    public void testImportPrivateKeysWithEncryptedWallet() throws Exception { 
//        // Create MultiBit controller.
//        MultiBitController controller = ActionTestUtils.createController();
//        
//        // Create a new encrypted wallet and put it in the model as the active wallet.
//        ActionTestUtils.createNewActiveWallet(controller, "testImportPrivateKeysWithEncryptedWallet", true, WALLET_PASSWORD);
//
//        // Create a new ExportPrivateKeysSubmitAction to test.
//        FontSizer.INSTANCE.initialise(controller);
//        ExportPrivateKeysPanel exportPanel = new ExportPrivateKeysPanel(controller, null);
//        ExportPrivateKeysSubmitAction exportAction = exportPanel.getExportPrivateKeySubmitAction();
//
//        assertNotNull("exportAction was not created successfully", exportAction);
//        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
//        
//        // Execute - this is with an encrypted wallet and default settings. (No wallet password set).
//        exportAction.actionPerformed(null);
//        assertEquals("Wrong message1 after default export execute", EXPECTED_ENTER_THE_WALLET_PASSWORD, exportPanel.getMessageText1());    
//        assertEquals("Wrong message2 after default export execute", "", exportPanel.getMessageText2().trim());    
//        
//        // Set the wallet password.
//        exportPanel.setWalletPassword(WALLET_PASSWORD);
//
//        // Execute - this should now complain that no export file is set.
//        exportAction.actionPerformed(null);
//        assertEquals("Wrong message1 after password set execute", EXPECTED_YOU_MUST_SELECT_AN_OUTPUT_FILE, exportPanel.getMessageText1());    
//        assertEquals("Wrong message2 after password set execute", "", exportPanel.getMessageText2().trim());    
//
//        // Set the output file name.
//        String outputFilename1 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
//        exportPanel.setOutputFilename(outputFilename1);
// 
//        // Execute - this should now complain that no export file password is set (as password protect export file is selected by default).
//        exportAction.actionPerformed(null);
//        assertEquals("Wrong message1 after no password set execute", EXPECTED_ENTER_THE_EXPORT_FILE_PASSWORD, exportPanel.getMessageText1());    
//        assertEquals("Wrong message2 after no password set execute", "", exportPanel.getMessageText2().trim());    
// 
//        // Set the first export file password.
//        exportPanel.setExportPassword(EXPORT_FILE_PASSWORD);
//        
//        // Execute = this is with one only of the export file passwords set.
//        exportAction.actionPerformed(null);
//        assertEquals("Wrong message1 after no password set execute", EXPECTED_PASSWORDS_DO_NOT_MATCH, exportPanel.getMessageText1());    
//        assertEquals("Wrong message2 after no password set execute", "", exportPanel.getMessageText2().trim());    
//        
//        // Set the repeat export file password.
//        exportPanel.setRepeatExportPassword(EXPORT_FILE_PASSWORD);
//        
//        // Check the export file currently does not exist.
//        assertTrue("Encrypted export file exists when it should not", !(new File(outputFilename1)).exists());
//       
//        // Execute = this should actually write the encrypted export file.
//        exportAction.actionPerformed(null);
//        assertTrue("Encrypted export file does not exist when it should", (new File(outputFilename1)).exists());
//        assertEquals("Wrong message1 after encrypted export is good to go", EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED, exportPanel.getMessageText1());    
//        assertEquals("Wrong message2 after encrypted export is good to go", EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY, exportPanel.getMessageText2().trim());  
//        
//        // Try to read in the encrypted exported private key file with no export file password - this should fail.
//        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
//        Collection<PrivateKeyAndDate> privateKeyAndDates = null; 
//        try {
//            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), null);
//            fail("An encrypted export file was read in with no export file password. Fail.");
//        } catch (EncrypterDecrypterException ede) {
//            // This is what should happen.
//            assertTrue("Unexpected exception thrown when decoding export file with no export file password", ede.getMessage().indexOf(EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING) > -1);
//        }
//        
//        // Try to read in the encrypted exported private key file with the wrong export file password - this should fail.
//        try {
//            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), WRONG_PASSWORD);
//            fail("An encrypted export file was read in with the wrong export file password. Fail.");
//        } catch (EncrypterDecrypterException ede) {
//            // This is what should happen.
//            assertTrue("Unexpected exception thrown when decoding export file with wrong export file password", ede.getMessage().indexOf(EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING) > -1);
//        }
//        
//        // Read in the encrypted exported private key file with the correct export file password.
//        privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
//        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), EXPORT_FILE_PASSWORD);
//        assertEquals("Wrong number of keys read in from encrypted export file", 1, privateKeyAndDates.size());
//        assertEquals("Wrong private key read in from encrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().get(0).getPrivKeyBytes()), 
//                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));  
//        
//        // Set the export file password protect radio to output unencrypted.
//        exportPanel.getDoNotPasswordProtect().setSelected(true);
//        
//        // Set the wallet password.
//        exportPanel.setWalletPassword(WALLET_PASSWORD);
//
//        // Set the output file name.
//        String outputFilename2 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
//        exportPanel.setOutputFilename(outputFilename2);
//
//        // Check the export file currently does not exist.
//        assertTrue("Unencrypted export file exists when it should not", !(new File(outputFilename2)).exists());
//       
//        // Execute = this should actually write the unencrypted export file.
//        exportAction.actionPerformed(null);
//        assertTrue("Unencrypted export file does not exist when it should", (new File(outputFilename2)).exists());
//        assertEquals("Wrong message1 after unencrypted export is good to go", EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED, exportPanel.getMessageText1());    
//        assertEquals("Wrong message2 after unencrypted export is good to go", EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY, exportPanel.getMessageText2().trim());  
//
//        // Read in the unencrypted exported private key file.
//        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename2), null);
//        assertEquals("Wrong number of keys read in from unencrypted export file", 1, privateKeyAndDates.size());
//        
//        // TODO - this will start failing when the private keys are cleared from Wallets on encrypt. (After persistence is done).
//        // Then will need to decrypt wallet first before getting private key out for comparision.
//        assertEquals("Wrong private key read in from unencrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().get(0).getPrivKeyBytes()), 
//                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));          
//    }
}
