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
import org.multibit.crypto.EncryptableWallet;
import org.multibit.viewsystem.swing.view.CreateNewReceivingAddressPanel;
import org.multibit.viewsystem.swing.view.SendBitcoinConfirmDialog;
import org.multibit.viewsystem.swing.view.components.FontSizer;

public class SendBitcoinNowSubmitActionTest extends TestCase {      
    public static final char[] TEST_PASSWORD1 = "my hovercraft has eels".toCharArray();
    
    private static final int DELAY_TO_COMPLETE_SEND = 4000; // milliseconds
    

    @Test
    public void testSendBitcoinWithNonEncryptedWallet() throws Exception {       
        // Create MultiBit controller
        MultiBitController controller = ActionTestUtils.createController();
        
        // Create a new wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithNonEncryptedWallet", false, null);

        // Create a new SendBitcoinNowSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        SendBitcoinConfirmDialog sendBitcoinConfirmDialog= new SendBitcoinConfirmDialog(controller, null);
        SendBitcoinNowAction sendBitcoinNowAction = sendBitcoinConfirmDialog.getSendBitcoinNowAction();

        assertNotNull("sendBitcoinNowAction was not created successfully", sendBitcoinNowAction);
        
        // Set the action up to use test parameters and succeed-on-send.
        sendBitcoinNowAction.setTestParameters(true, true);
        
        // Execute the sendBitcoinNowAction - this should give the sending message or sent message.
        sendBitcoinNowAction.actionPerformed(null);
        assertTrue("Wrong message - expecting sending/sent on messageText1, was '" + sendBitcoinConfirmDialog.getMessageText1() + "'", "Sending bitcoin...".equals(sendBitcoinConfirmDialog.getMessageText1()) ||
                "Your bitcoin were sent successfully.".equals(sendBitcoinConfirmDialog.getMessageText1()));    
        assertEquals("Wrong message - expecting sending on messageText2", " ", sendBitcoinConfirmDialog.getMessageText2());    
        
        // Wait a while and the message should be that it has completed the send.
        Thread.sleep(DELAY_TO_COMPLETE_SEND);

        // Bitcoins should now be sent
        assertEquals("Wrong message - expecting success on messageText1", "Your bitcoin were sent successfully.", sendBitcoinConfirmDialog.getMessageText1());    
        assertEquals("Wrong message - expecting success on messageText2", " ", sendBitcoinConfirmDialog.getMessageText2());    
  
        // Set the action up to use test parameters and fail-on-send.
        sendBitcoinNowAction.setTestParameters(true, false);
        
        // Execute the sendBitcoinNowAction - this should give the sending or failed message.
        sendBitcoinNowAction.actionPerformed(null);
        assertTrue("Wrong message - expecting sending/sent on messageText1 was '" + sendBitcoinConfirmDialog.getMessageText1() + "'", " ".equals(sendBitcoinConfirmDialog.getMessageText1()) || "Sending bitcoin...".equals(sendBitcoinConfirmDialog.getMessageText1()) ||
                "The send of your bitcoin failed.".equals(sendBitcoinConfirmDialog.getMessageText1()));    
        assertEquals("Wrong message - expecting sending on messageText2", " test - send failed", sendBitcoinConfirmDialog.getMessageText2());    
        
        // Wait a while and the message should be that it has failed the send.
        Thread.sleep(DELAY_TO_COMPLETE_SEND);

        assertEquals("Wrong message - expecting success on messageText1", "The send of your bitcoin failed.", sendBitcoinConfirmDialog.getMessageText1());    
        assertEquals("Wrong message - expecting success on messageText2", " test - send failed", sendBitcoinConfirmDialog.getMessageText2());    
  
    }
    
//    @Test
//    public void testAddReceivingAddressesWithEncryptedWallet() throws Exception {   
//        // Create MultiBit controller.
//        MultiBitController controller = ActionTestUtils.createController();
//        
//        // Create a new encrypted wallet and put it in the model as the active wallet.
//        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithEncryptedWallet", true, TEST_PASSWORD1);
//
//        // Create a new CreateNewReceivingAddressSubmitAction to test.
//        FontSizer.INSTANCE.initialise(controller);
//        CreateNewReceivingAddressPanel createNewPanel = new CreateNewReceivingAddressPanel(controller, null, null);
//        CreateNewReceivingAddressSubmitAction createNewAction = createNewPanel.getCreateNewReceivingAddressSubmitAction();
//
//        assertNotNull("createNewAction was not created successfully", createNewAction);
//        assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
//        
//        // Execute the createNewAction - by default the createNewDialog sould be set to add one key.
//        // However as there is no wallet password supplied it will not add the key.
//        createNewAction.actionPerformed(null);
//        assertEquals("Wrong number of keys after addition of default number of keys with no wallet password", 1, controller.getModel().getActiveWallet().getKeychain().size());    
//        
//        // Check there is a message that the wallet password is required.
//        assertEquals("No message to enter wallet password", "Enter the wallet password", createNewPanel.getMessageText());
//        
//        // Enter an incorrect password. There should be a message to the user.
//        createNewPanel.setWalletPassword("This is the wrong password");
//        createNewAction.actionPerformed(null);
//
//        // Check there is a message that the wallet password isincorrect.
//        assertEquals("No message to that wallet password is incorrect", "The wallet password is incorrect", createNewPanel.getMessageText());
//
//        // Set the correct wallet password.
//        createNewPanel.setWalletPassword(new String(TEST_PASSWORD1));
//        
//        // The new private key should now add correctly.
//        createNewAction.actionPerformed(null);
//        assertEquals("Wrong number of keys after addition of default number of keys with wallet password", 2, controller.getModel().getActiveWallet().getKeychain().size()); 
//
//        // Add twenty addresses by selecting on the combo box.
//        createNewPanel.getNumberOfAddresses().setSelectedItem(new Integer(20));
//        createNewPanel.setWalletPassword(new String(TEST_PASSWORD1));
//        createNewAction.actionPerformed(null);
//        assertEquals("Wrong number of keys after addition of 20 keys", 22, controller.getModel().getActiveWallet().getKeychain().size());  
//
//        // The added private keys should be encrypted with the same password as the wallet password.
//        // Thus a decrypt should work fine.
//        ((EncryptableWallet)controller.getModel().getActiveWallet()).decrypt(TEST_PASSWORD1);
//    }
}
