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
import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.file.PrivateKeyAndDate;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.viewsystem.swing.view.ExportPrivateKeysPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import com.google.bitcoin.core.Utils;

public class ExportPrivateKeysSubmitActionTest extends TestCase {   
    
    public static final char[] TEST_PASSWORD1 = "the quick brown fox jumps over the lazy dog 0123456789".toCharArray();
    public static final char[] WRONG_PASSWORD = "this is the wrong password".toCharArray();
    
    @Test
    public void testExportPrivateKeysWithNonEncryptedWallet() throws Exception { 
        // Check if headless.
//        if (GraphicsEnvironment.isHeadless()) {
//            return;
//        }
        
        // Create MultiBit controller
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testExportPrivateKeysWithNonEncryptedWallet", false, null);

        // Create a new ExportPrivateKeysSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ExportPrivateKeysPanel exportPanel = new ExportPrivateKeysPanel(controller, null);
        ExportPrivateKeysSubmitAction exportAction = exportPanel.getExportPrivateKeySubmitAction();

        assertNotNull("exportAction was not created successfully", exportAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        
         // Execute - this is with an unencrypted wallet and default settings.
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after default export press", "You must select an output file", exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after default export press", "", exportPanel.getMessageText2().trim());    
        
        // Set the output file name.
        String outputFilename1 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
        exportPanel.setOutputFilename(outputFilename1);
 
        // Execute - this should now complain that no export file password is set (as password protect export file is selected by default).
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after no password set press", "Enter the password you want to use for the export file", exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after no password set press", "", exportPanel.getMessageText2().trim());    
 
        // Set the first export file password
        exportPanel.setExportPassword(TEST_PASSWORD1);
        
        // Execute = this is with one only of the export file passwords set.
        exportAction.actionPerformed(null);
        assertEquals("Wrong message1 after no password set press", "The password and repeat password do not match", exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after no password set press", "", exportPanel.getMessageText2().trim());    
        
        // Set the repeat export file password
        exportPanel.setRepeatExportPassword(TEST_PASSWORD1);
        // Check the export file currently does not exist.
        assertTrue("Export file exists when it should not", !(new File(outputFilename1)).exists());
       
        // Execute = this should actually write the encrypted export file.
        exportAction.actionPerformed(null);
        assertTrue("Export file does not exist when it should", (new File(outputFilename1)).exists());
        assertEquals("Wrong message1 after export is good to go", "The private keys were exported.", exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after export is good to go", "The export file could be read in correctly and the private keys match the wallet contents", exportPanel.getMessageText2().trim());  
        
        // Try to read in the encrypted exported private key file with no password - this should fail.
        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
        Collection<PrivateKeyAndDate> privateKeyAndDates = null; 
        try {
            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), null);
            fail("An encrypted export file was read in with no password. Fail.");
        } catch (EncrypterDecrypterException ede) {
            // This is what should happen.
            assertTrue("Unexpected exception thrown when decoding export file with no password", ede.getMessage().indexOf("Could not decrypt input string") > -1);
        }
        
        // Try to read in the encrypted exported private key file with the wrong password - this should fail.
        try {
            privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), WRONG_PASSWORD);
            fail("An encrypted export file was read in with the wrong password. Fail.");
        } catch (EncrypterDecrypterException ede) {
            // This is what should happen.
            assertTrue("Unexpected exception thrown when decoding export file with no password", ede.getMessage().indexOf("Could not decrypt input string") > -1);
        }
        
        // Read in the encrypted exported private key file with the correct password.
        privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename1), TEST_PASSWORD1);
        assertEquals("Wrong number of keys read in from encrypted export file", 1, privateKeyAndDates.size());
        assertEquals("Wrong private key read in from encrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().get(0).getPrivKeyBytes()), 
                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));  
        
        // Set the export file password protect radio to output unencrypted
        exportPanel.getDoNotPasswordProtect().setSelected(true);
        
        // Set the output file name.
        String outputFilename2 = controller.getModel().getActiveWalletFilename() + "-" + UUID.randomUUID().toString() + ".key";
        exportPanel.setOutputFilename(outputFilename2);

        // Execute = this should actually write the unencrypted export file.
        exportAction.actionPerformed(null);
        assertTrue("Export file does not exist when it should", (new File(outputFilename2)).exists());
        assertEquals("Wrong message1 after export is good to go", "The private keys were exported.", exportPanel.getMessageText1());    
        assertEquals("Wrong message2 after export is good to go", "The export file could be read in correctly and the private keys match the wallet contents", exportPanel.getMessageText2().trim());  

        // Read in the unencrypted exported private key file
        privateKeyAndDates = privateKeysHandler.readInPrivateKeys(new File(outputFilename2), null);
        assertEquals("Wrong number of keys read in from encrypted export file", 1, privateKeyAndDates.size());
        assertEquals("Wrong private key read in from encrypted export file", Utils.bytesToHexString(controller.getModel().getActiveWallet().getKeychain().get(0).getPrivKeyBytes()), 
                Utils.bytesToHexString(privateKeyAndDates.iterator().next().getKey().getPrivKeyBytes()));          
    }
}
