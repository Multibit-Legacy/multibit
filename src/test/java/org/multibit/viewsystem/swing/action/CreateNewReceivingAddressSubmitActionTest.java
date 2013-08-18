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
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.view.panels.CreateNewReceivingAddressPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.CreateControllers;

public class CreateNewReceivingAddressSubmitActionTest extends TestCase {      
    public static final CharSequence TEST_PASSWORD1 = "my hovercraft has eels";
    
    private int TIME_TO_WAIT_FOR_ONE_KEY = 1000; // milliseconds
   
    @Test
    public void testAddReceivingAddressesWithEncryptedWallet() throws Exception {   
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        BitcoinController controller = controllers.bitcoinController;
        
        // Create a new encrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithEncryptedWallet", true, TEST_PASSWORD1);

        // Create a new CreateNewReceivingAddressSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        CreateNewReceivingAddressPanel createNewPanel = new CreateNewReceivingAddressPanel(controller, null, null);
        CreateNewReceivingAddressSubmitAction createNewAction = createNewPanel.getCreateNewReceivingAddressSubmitAction();
        assertTrue("Wallet password is not enabled when it should be", createNewPanel.isWalletPasswordFieldEnabled());
        assertNotNull("createNewAction was not created successfully", createNewAction);
        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
        assertTrue("Wallet is not encrypted but it should be", controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
        assertNull("The last private key backup file was not null", createNewAction.getLastPrivateKeysBackupFile());
        System.out.println("ping 1");
        // Execute the createNewAction - by default the createNewDialog sould be set to add one key.
        // However as there is no wallet password supplied it will not add the key.
        createNewAction.actionPerformed(null);
        System.out.println("ping 2");
        
        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);

        assertEquals("Wrong number of keys after addition of default number of keys with no wallet password", 1, controller.getModel().getActiveWallet().getKeychain().size());    
        
        // Check there is a message that the wallet password is required.
        assertEquals("No message to enter wallet password", "Enter the wallet password", createNewPanel.getMessageText());
        
        // Enter an incorrect password. There should be a message to the user.
        createNewPanel.setWalletPassword("This is the wrong password");

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);
        System.out.println("ping 3");

        createNewAction.actionPerformed(null);
        System.out.println("ping 4");

        // Check there is a message that the wallet password isincorrect.
        assertEquals("No message to that wallet password is incorrect", "The wallet password is incorrect", createNewPanel.getMessageText());

        // Set the correct wallet password.
        createNewPanel.setWalletPassword(TEST_PASSWORD1.toString());
        System.out.println("ping 5");

        // The new private key should now add correctly.
        createNewAction.actionPerformed(null);
        System.out.println("ping 6");

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY);

        assertEquals("Wrong number of keys after addition of default number of keys with wallet password", 2, controller.getModel().getActiveWallet().getKeychain().size()); 

        // Add twenty addresses by selecting on the combo box.
        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(20));
        createNewPanel.setWalletPassword(TEST_PASSWORD1.toString());
        System.out.println("ping 7");

        createNewAction.actionPerformed(null);
        System.out.println("ping 8");

        Thread.sleep(TIME_TO_WAIT_FOR_ONE_KEY * 40);

        assertEquals("Wrong number of keys after addition of 20 keys", 22, controller.getModel().getActiveWallet().getKeychain().size());  

        // The added private keys should be encrypted with the same password as the wallet password.
        // Thus a decrypt should work fine.
        controller.getModel().getActiveWallet().decrypt(controller.getModel().getActiveWallet().getKeyCrypter().deriveKey(TEST_PASSWORD1));
    }
}
