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

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.view.CreateNewReceivingAddressPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

public class CreateNewReceivingAddressSubmitActionTest extends TestCase {      
    public static final char[] TEST_PASSWORD1 = "my hovercraft has eels".toCharArray();
    
    private int TIME_TO_WAIT_FOR_ONE_KEY = 200; // milliseconds
    
    @Test
    public void testAddReceivingAddressesWithNonEncryptedWallet() throws Exception {       
        // Create MultiBit controller
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithNonEncryptedWallet", false, null);

        // Create a new CreateNewReceivingAddressSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        CreateNewReceivingAddressPanel createNewPanel = new CreateNewReceivingAddressPanel(controller, null, null);
        CreateNewReceivingAddressSubmitAction createNewAction = createNewPanel.getCreateNewReceivingAddressSubmitAction();

        assertNotNull("createNewAction was not created successfully", createNewAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet password is enabled when it should not be", !createNewPanel.isWalletPasswordFieldEnabled());
        assertNull("The last private key backup file was not null", createNewAction.getLastPrivateKeysBackupFile());
        
        // Execute the createNewAction - by default the createNewDialog should be set to add one key.
        createNewAction.actionPerformed(null);
        
        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);
        
        assertEquals("Wrong number of keys after addition of default number of keys", 2, controller.getModel().getActiveWallet().getKeychain().size());    

        assertNull("The last private key backup file was not null - a backup should not be created for non-encrypted wallets.", createNewAction.getLastPrivateKeysBackupFile());

        // Add one address by selecting on the combo box.
        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(1));
        createNewAction.actionPerformed(null);
        
        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);

        assertEquals("Wrong number of keys after addition of 1 key", 3, controller.getModel().getActiveWallet().getKeychain().size());
        
        // Add five addresses by selecting on the combo box.
        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(5));
        createNewAction.actionPerformed(null);
        
        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY * 5);

        assertEquals("Wrong number of keys after addition of 5 keys", 8, controller.getModel().getActiveWallet().getKeychain().size());   
        
        // Add twenty addresses by selecting on the combo box.
        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(20));
        createNewAction.actionPerformed(null);
        
        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY * 20);

        assertEquals("Wrong number of keys after addition of 20 keys", 28, controller.getModel().getActiveWallet().getKeychain().size());  
        
        // Add one hundred addresses by selecting on the combo box.
        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(100));
        createNewAction.actionPerformed(null);

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY * 100);

        assertEquals("Wrong number of keys after addition of 100 keys", 128, controller.getModel().getActiveWallet().getKeychain().size());    
    }
    
    @Test
    public void testAddReceivingAddressesWithEncryptedWallet() throws Exception {   
        // Create MultiBit controller.
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new encrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithEncryptedWallet", true, TEST_PASSWORD1);

        // Create a new CreateNewReceivingAddressSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        CreateNewReceivingAddressPanel createNewPanel = new CreateNewReceivingAddressPanel(controller, null, null);
        CreateNewReceivingAddressSubmitAction createNewAction = createNewPanel.getCreateNewReceivingAddressSubmitAction();
        assertTrue("Wallet password is not enabled when it should be", createNewPanel.isWalletPasswordFieldEnabled());
        assertNotNull("createNewAction was not created successfully", createNewAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet is not encrypted but it should be", controller.getModel().getActiveWallet().isCurrentlyEncrypted());
        assertNull("The last private key backup file was not null", createNewAction.getLastPrivateKeysBackupFile());

        // Execute the createNewAction - by default the createNewDialog sould be set to add one key.
        // However as there is no wallet password supplied it will not add the key.
        createNewAction.actionPerformed(null);
        
        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);

        assertEquals("Wrong number of keys after addition of default number of keys with no wallet password", 1, controller.getModel().getActiveWallet().getKeychain().size());    
        
        // Check there is a message that the wallet password is required.
        assertEquals("No message to enter wallet password", "Enter the wallet password", createNewPanel.getMessageText());
        
        // Enter an incorrect password. There should be a message to the user.
        createNewPanel.setWalletPassword("This is the wrong password");

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);

        createNewAction.actionPerformed(null);

        // Check there is a message that the wallet password isincorrect.
        assertEquals("No message to that wallet password is incorrect", "The wallet password is incorrect", createNewPanel.getMessageText());

        // Set the correct wallet password.
        createNewPanel.setWalletPassword(new String(TEST_PASSWORD1));
        
        // The new private key should now add correctly.
        createNewAction.actionPerformed(null);

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);

        assertEquals("Wrong number of keys after addition of default number of keys with wallet password", 2, controller.getModel().getActiveWallet().getKeychain().size()); 

//        assertNotNull("The last private key backup file was null.", createNewAction.getLastPrivateKeysBackupFile());
//        assertTrue("The private key backup file does not exist",  createNewAction.getLastPrivateKeysBackupFile().exists());
//        PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());
//        Verification verification = privateKeysHandler.verifyExportFile(createNewAction.getLastPrivateKeysBackupFile(), controller.getModel().getActiveWallet(), 
//                controller.getMultiBitService().getChain(), true, TEST_PASSWORD1, TEST_PASSWORD1);
//        assertTrue("The backed up private keys did not match the wallet", verification.isCorrect());

        // Add twenty addresses by selecting on the combo box.
        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(20));
        createNewPanel.setWalletPassword(new String(TEST_PASSWORD1));
        createNewAction.actionPerformed(null);

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY * 40);

        assertEquals("Wrong number of keys after addition of 20 keys", 22, controller.getModel().getActiveWallet().getKeychain().size());  

        // The added private keys should be encrypted with the same password as the wallet password.
        // Thus a decrypt should work fine.
        controller.getModel().getActiveWallet().decrypt(controller.getModel().getActiveWallet().getEncrypterDecrypter().deriveKey(TEST_PASSWORD1));
    }
}
