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
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.BitcoinControllerTest;
import org.multibit.controller.SimpleWalletBusyListener;
import org.multibit.file.PrivateKeyAndDate;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.ExportPrivateKeysPanel;

import com.google.dogecoin.core.Utils;
import com.google.dogecoin.crypto.EncryptedPrivateKey;
import com.google.dogecoin.crypto.KeyCrypter;
import com.google.dogecoin.crypto.KeyCrypterException;
import org.multibit.CreateControllers;


public class ExportPrivateKeysSubmitActionTest extends TestCase {   
    
    private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";
    private static final String EXPECTED_YOU_MUST_SELECT_AN_OUTPUT_FILE = "You must select an output file";
    private static final String EXPECTED_ENTER_THE_EXPORT_FILE_PASSWORD = "Enter the password you want to use for the export file";
    private static final String EXPECTED_PASSWORDS_DO_NOT_MATCH = "The password and repeat password do not match";
    private static final String EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING = "Could not decrypt input string";
    private static final String EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED = "The private keys were exported.";
    private static final String EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY = "The export file could be read in correctly and the private keys match the wallet contents";
     
    public static final CharSequence EXPORT_FILE_PASSWORD = "the quick brown fox jumps over the lazy dog 0123456789";
    public static final CharSequence WALLET_PASSWORD = "the unbelievable lightness of being";
    public static final CharSequence WRONG_PASSWORD = "this is the wrong password";
    
    @Test
    public void testExportPrivateKeysWithNonEncryptedWallet() throws Exception { 
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testExportPrivateKeysWithNonEncryptedWallet", false, null);

        // Hook up a wallet busy listener.
        SimpleWalletBusyListener walletBusyListener = new SimpleWalletBusyListener();
        controller.registerWalletBusyListener(walletBusyListener);
        
        // Create a new ExportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ExportPrivateKeysPanel exportPanel = new ExportPrivateKeysPanel(controller, null);
        ExportPrivateKeysSubmitAction exportAction = exportPanel.getExportPrivateKeySubmitAction();
 
        assertNotNull("exportAction was not created successfully", exportAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is enabled when it should not be", !exportPanel.isWalletPasswordFieldEnabled());
       
         // Execute - this is with an unencrypted wallet and default settings.
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after default export execute", EXPECTED_YOU_MUST_SELECT_AN_OUTPUT_FILE, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after default export execute", "", exportPanel.getMessageText2().trim());    
        
        // Set the output file name.
        String outputFilename1 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
        exportPanel.setOutputFilename(outputFilename1);
 
        // Execute - this should now complain that no export file password is set (as password protect export file is selected by default).
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after no password set execute", EXPECTED_ENTER_THE_EXPORT_FILE_PASSWORD, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after no password set execute", "", exportPanel.getMessageText2().trim());    
 
        // Set the first export file password.
        exportPanel.setExportPassword(EXPORT_FILE_PASSWORD);
        
        // Execute = this is with one only of the export file passwords set.
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after no password set execute", EXPECTED_PASSWORDS_DO_NOT_MATCH, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after no password set execute", "", exportPanel.getMessageText2().trim());    
        
        // Set the repeat export file password.
        exportPanel.setRepeatExportPassword(EXPORT_FILE_PASSWORD);
        
        // Check the export file currently does not exist.
        assertTrue("Encrypted export file exists when it should not", !(new File(outputFilename1)).exists());
       
        // Execute = this should actually write the encrypted export file.
        exportAction.actionPerformed(null);
        
        BitcoinControllerTest.waitForWalletNotBusy(walletBusyListener);
        
        assertTrue("Encrypted export file does not exist when it should", (new File(outputFilename1)).exists());
        assertEquals("Wrong message1 after encrypted export is good to go", EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after encrypted export is good to go", EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY, exportPanel.getMessageText2().trim());  
        
        // Try to read in the encrypted exported private key file with no password - this should fail.
        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
        Collection<PrivateKeyAndDate> privateKeyAndDates = null; 
        try {
            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), null);
            fail("An encrypted export file was read in with no password. Fail.");
        } catch (KeyCrypterException kce) {
            // This is what should happen.
            assertTrue("Unexpected exception thrown when decoding export file with no password", kce.getMessage().indexOf(EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING) > -1);
        }
        
        // Try to read in the encrypted exported private key file with the wrong password - this should fail.
        try {
            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), WRONG_PASSWORD);
            fail("An encrypted export file was read in with the wrong password. Fail.");
        } catch (KeyCrypterException kce) {
            // This is what should happen.
            assertTrue("Unexpected exception thrown when decoding export file with wrong password", kce.getMessage().indexOf(EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING) > -1);
        }
        
        // Read in the encrypted exported private key file with the correct password.
        privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), EXPORT_FILE_PASSWORD);
        assertEquals("Wrong number of keys read in from encrypted export file", 1, privateKeyAndDates.size());
        assertEquals("Wrong private key read in from encrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().iterator().next().getPrivKeyBytes()), 
                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));  
        
        // Set the export file password protect radio to output unencrypted.
        exportPanel.getDoNotPasswordProtect().setSelected(true);
        
        // Set the output file name.
        String outputFilename2 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
        exportPanel.setOutputFilename(outputFilename2);

        // Check the export file currently does not exist.
        assertTrue("Unencrypted export file exists when it should not", !(new File(outputFilename2)).exists());
       
        // Execute = this should actually write the unencrypted export file.
        exportAction.actionPerformed(null);
        
        BitcoinControllerTest.waitForWalletNotBusy(walletBusyListener);
        
        assertTrue("Unencrypted export file does not exist when it should", (new File(outputFilename2)).exists());
        assertEquals("Wrong message1 after unencrypted export is good to go", EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after unencrypted export is good to go", EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY, exportPanel.getMessageText2().trim());  

        // Read in the unencrypted exported private key file.
        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename2), null);
        assertEquals("Wrong number of keys read in from unencrypted export file", 1, privateKeyAndDates.size());
        assertEquals("Wrong private key read in from unencrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().iterator().next().getPrivKeyBytes()), 
                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));          
    }
    
    @Test
    public void testExportPrivateKeysWithEncryptedWallet() throws Exception { 
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new encrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testExportPrivateKeysWithEncryptedWallet", true, WALLET_PASSWORD);

        // Hook up a wallet busy listener.
        SimpleWalletBusyListener walletBusyListener = new SimpleWalletBusyListener();
        controller.registerWalletBusyListener(walletBusyListener);

        // Remember the private keys for the key - for comparision later.
        // Copy the private key bytes for checking later.
        EncryptedPrivateKey encryptedPrivateKey = controller.getModel().getActiveWallet().getKeychain().get(0).getEncryptedPrivateKey();
        KeyCrypter keyCrypter = controller.getModel().getActiveWallet().getKeyCrypter();
        byte[] originalPrivateKeyBytes = keyCrypter.decrypt(encryptedPrivateKey, keyCrypter.deriveKey(WALLET_PASSWORD));

        // Create a new ExportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ExportPrivateKeysPanel exportPanel = new ExportPrivateKeysPanel(controller, null);
        ExportPrivateKeysSubmitAction exportAction = exportPanel.getExportPrivateKeySubmitAction();

        assertNotNull("exportAction was not created successfully", exportAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is not enabled when it should be", exportPanel.isWalletPasswordFieldEnabled());
        
        // Execute - this is with an encrypted wallet and default settings. (No wallet password set).
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after default export execute", EXPECTED_ENTER_THE_WALLET_PASSWORD, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after default export execute", "", exportPanel.getMessageText2().trim());    
        
        // Set the wallet password.
        exportPanel.setWalletPassword(WALLET_PASSWORD);

        // Execute - this should now complain that no export file is set.
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after password set execute", EXPECTED_YOU_MUST_SELECT_AN_OUTPUT_FILE, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after password set execute", "", exportPanel.getMessageText2().trim());    

        // Set the output file name.
        String outputFilename1 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
        exportPanel.setOutputFilename(outputFilename1);
 
        // Execute - this should now complain that no export file password is set (as password protect export file is selected by default).
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after no password set execute", EXPECTED_ENTER_THE_EXPORT_FILE_PASSWORD, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after no password set execute", "", exportPanel.getMessageText2().trim());    
 
        // Set the first export file password.
        exportPanel.setExportPassword(EXPORT_FILE_PASSWORD);
        
        // Execute = this is with one only of the export file passwords set.
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after no password set execute", EXPECTED_PASSWORDS_DO_NOT_MATCH, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after no password set execute", "", exportPanel.getMessageText2().trim());    
        
        // Set the repeat export file password.
        exportPanel.setRepeatExportPassword(EXPORT_FILE_PASSWORD);
        
        // Check the export file currently does not exist.
        assertTrue("Encrypted export file exists when it should not", !(new File(outputFilename1)).exists());
       
        // Execute = this should actually write the encrypted export file.
        exportAction.actionPerformed(null);
        
        BitcoinControllerTest.waitForWalletNotBusy(walletBusyListener);
        
        assertTrue("Encrypted export file does not exist when it should", (new File(outputFilename1)).exists());
        assertEquals("Wrong message1 after encrypted export is good to go", EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after encrypted export is good to go", EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY, exportPanel.getMessageText2().trim());  
        
        // Try to read in the encrypted exported private key file with no export file password - this should fail.
        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
        Collection<PrivateKeyAndDate> privateKeyAndDates = null; 
        try {
            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), null);
            fail("An encrypted export file was read in with no export file password. Fail.");
        } catch (KeyCrypterException kce) {
            // This is what should happen.
            assertTrue("Unexpected exception thrown when decoding export file with no export file password", kce.getMessage().indexOf(EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING) > -1);
        }
        
        // Try to read in the encrypted exported private key file with the wrong export file password - this should fail.
        try {
            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), WRONG_PASSWORD);
            fail("An encrypted export file was read in with the wrong export file password. Fail.");
        } catch (KeyCrypterException kce) {
            // This is what should happen.
            assertTrue("Unexpected exception thrown when decoding export file with wrong export file password", kce.getMessage().indexOf(EXPECTED_COULD_NOT_DECRYPT_INPUT_STRING) > -1);
        }
        
        // Read in the encrypted exported private key file with the correct export file password.
        privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), EXPORT_FILE_PASSWORD);
        assertEquals("Wrong number of keys read in from encrypted export file", 1, privateKeyAndDates.size());
        
        assertEquals("Wrong private key read in from encrypted export file", Utils.bytesToHexString(originalPrivateKeyBytes), 
                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));  
        
        // Set the export file password protect radio to output unencrypted.
        exportPanel.getDoNotPasswordProtect().setSelected(true);
        
        // Set the wallet password.
        exportPanel.setWalletPassword(WALLET_PASSWORD);

        // Set the output file name.
        String outputFilename2 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
        exportPanel.setOutputFilename(outputFilename2);

        // Check the export file currently does not exist.
        assertTrue("Unencrypted export file exists when it should not", !(new File(outputFilename2)).exists());
       
        // Execute = this should actually write the unencrypted export file.
        exportAction.actionPerformed(null);
        
        BitcoinControllerTest.waitForWalletNotBusy(walletBusyListener);
        
        assertTrue("Unencrypted export file does not exist when it should", (new File(outputFilename2)).exists());
        assertEquals("Wrong message1 after unencrypted export is good to go", EXPECTED_THE_PRIVATE_KEYS_WERE_EXPORTED, exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after unencrypted export is good to go", EXPECTED_THE_EXPORT_FILE_COULD_BE_READ_IN_CORRECTLY, exportPanel.getMessageText2().trim());  

        // Read in the unencrypted exported private key file.
        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename2), null);
        assertEquals("Wrong number of keys read in from unencrypted export file", 1, privateKeyAndDates.size());
        
        assertEquals("Wrong private key read in from unencrypted export file", Utils.bytesToHexString(originalPrivateKeyBytes), 
                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));          
    }
    
    @Test
    public void testNoWalletSelected() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;

        // This test runs against an empty PerWalletModelDataList.
        assertTrue("There was an active wallet when there should not be", controller.getModel().thereIsNoActiveWallet());

        // Create a new ExportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ExportPrivateKeysPanel exportPrivateKeysPanel = new ExportPrivateKeysPanel(controller, null);
        ExportPrivateKeysSubmitAction exportPrivateKeysSubmitAction = exportPrivateKeysPanel.getExportPrivateKeySubmitAction();

        assertNotNull("exportPrivateKeysSubmitAction was not created successfully", exportPrivateKeysSubmitAction);

        // Execute.
        exportPrivateKeysSubmitAction.actionPerformed(null);
        Object[] messages = MessageManager.INSTANCE.getMessages().toArray();
        assertTrue("There were no messages but there should have been", messages != null && messages.length > 0);
        assertEquals("Wrong message after receive dogecoin confirm with no active wallet", ResetTransactionsSubmitActionTest.EXPECTED_NO_WALLET_IS_SELECTED, ((Message)messages[messages.length - 1]).getText());
    }
}
