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
import org.multibit.CreateControllers;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

public class SendBitcoinNowSubmitActionTest extends TestCase {      
    private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";

    private static final String EXPECTED_TEST_SEND_FAILED_ERROR = " test - send failed";

    private static final String EXPECTED_SEND_FAILED = "The send of your bitcoin failed.";

    private static final String EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY = "Your bitcoin were sent successfully.";

    private static final String EXPECTED_SENDING_BITCOIN = "Sending bitcoin...";

    public static final CharSequence TEST_PASSWORD1 = "my hovercraft has eels";
    public static final CharSequence WALLET_PASSWORD = "testing testing 123";

    private static final int DELAY_TO_COMPLETE_OPERATION = 12000; // milliseconds
    private static final int DELAY_TO_UPDATE_MESSAGES = 4000; // milliseconds
    
    @Test
    public void testSendBitcoinWithNonEncryptedWallet() throws Exception {       
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        MultiBitController controller = controllers.multiBitController;
        
        // Create a new unencrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithNonEncryptedWallet", false, null);

        // Create a new SendBitcoinNowSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        SendBitcoinConfirmPanel sendBitcoinConfirmPanel= new SendBitcoinConfirmPanel(controller, null, null);
        SendBitcoinNowAction sendBitcoinNowAction = sendBitcoinConfirmPanel.getSendBitcoinNowAction();

        assertNotNull("sendBitcoinNowAction was not created successfully", sendBitcoinNowAction);
        assertTrue("Wallet password was enabled when it should not be", !sendBitcoinConfirmPanel.isWalletPasswordFieldEnabled());
        
        // Set the action up to use test parameters and succeed-on-send.
        sendBitcoinNowAction.setTestParameters(true, true);
        
        // Execute - this should give the sending message or sent message.
        sendBitcoinNowAction.actionPerformed(null);
        assertTrue("Wrong message - expecting sending/sent on messageText1, was '" + sendBitcoinConfirmPanel.getMessageText1() + "'", "".equals(sendBitcoinConfirmPanel.getMessageText1().trim()) || EXPECTED_SENDING_BITCOIN.equals(sendBitcoinConfirmPanel.getMessageText1()) ||
                EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY.equals(sendBitcoinConfirmPanel.getMessageText1()));    
        assertEquals("Wrong message - expecting sending on messageText2", "", sendBitcoinConfirmPanel.getMessageText2().trim());    
        
        // Wait a while and the message should be that it has completed the send.
        Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

        // Bitcoins should now be sent
        assertEquals("Wrong message - expecting success on messageText1", EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY, sendBitcoinConfirmPanel.getMessageText1());    
        assertEquals("Wrong message - expecting success on messageText2", "", sendBitcoinConfirmPanel.getMessageText2().trim());    
  
        // Set the action up to use test parameters and fail-on-send.
        sendBitcoinNowAction.setTestParameters(true, false);
        Thread.sleep(DELAY_TO_UPDATE_MESSAGES);
        
        // Execute - this should give the sending or failed message.
        sendBitcoinNowAction.actionPerformed(null);
        Thread.sleep(DELAY_TO_UPDATE_MESSAGES);
        assertTrue("Wrong message - expecting sending/sent on messageText1 was '" + sendBitcoinConfirmPanel.getMessageText1() + "'", "".equals(sendBitcoinConfirmPanel.getMessageText1().trim()) || EXPECTED_SENDING_BITCOIN.equals(sendBitcoinConfirmPanel.getMessageText1()) ||
                EXPECTED_SEND_FAILED.equals(sendBitcoinConfirmPanel.getMessageText1()));    
        assertEquals("Wrong message - expecting sending on messageText2", EXPECTED_TEST_SEND_FAILED_ERROR, sendBitcoinConfirmPanel.getMessageText2());    
        
        // Wait a while and the message should be that it has failed the send.
        Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

        assertEquals("Wrong message - expecting success on messageText1", EXPECTED_SEND_FAILED, sendBitcoinConfirmPanel.getMessageText1());    
        assertEquals("Wrong message - expecting success on messageText2", EXPECTED_TEST_SEND_FAILED_ERROR, sendBitcoinConfirmPanel.getMessageText2());    
    }
    
    @Test
    public void testSendBitcoinWithEncryptedWallet() throws Exception {       
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        MultiBitController controller = controllers.multiBitController;
        
        // Create a new encrypted wallet and put it in the model as the active wallet.
        ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithNonEncryptedWallet", true, WALLET_PASSWORD);

        // Create a new SendBitcoinNowSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        SendBitcoinConfirmPanel sendBitcoinConfirmPanel= new SendBitcoinConfirmPanel(controller, null, null);
        SendBitcoinNowAction sendBitcoinNowAction = sendBitcoinConfirmPanel.getSendBitcoinNowAction();

        assertNotNull("sendBitcoinNowAction was not created successfully", sendBitcoinNowAction);
        assertTrue("Wallet password was disabled when it should not be", sendBitcoinConfirmPanel.isWalletPasswordFieldEnabled());
        
        // Set the action up to use test parameters and succeed-on-send.
        sendBitcoinNowAction.setTestParameters(true, true);
        
        // Execute - this should complain that the wallet password is not set.
        sendBitcoinNowAction.actionPerformed(null);
        // Wait a while and the message should be that it has completed the send.
        Thread.sleep(DELAY_TO_UPDATE_MESSAGES);

        assertTrue("Wrong message - expecting no wallet password on messageText1, was '" + sendBitcoinConfirmPanel.getMessageText1() + "'", EXPECTED_ENTER_THE_WALLET_PASSWORD.equals(sendBitcoinConfirmPanel.getMessageText1()));
        
        // Set the wallet password.
        sendBitcoinConfirmPanel.setWalletPassword(WALLET_PASSWORD);
        
        // Execute - this should give the sending message or sent message.
        sendBitcoinNowAction.actionPerformed(null);
        assertTrue("Wrong message - expecting sending/sent on messageText1, was '" + sendBitcoinConfirmPanel.getMessageText1() + "'", "".equals(sendBitcoinConfirmPanel.getMessageText1().trim()) || EXPECTED_SENDING_BITCOIN.equals(sendBitcoinConfirmPanel.getMessageText1()) ||
                EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY.equals(sendBitcoinConfirmPanel.getMessageText1()));    
        assertEquals("Wrong message - expecting sending on messageText2", "", sendBitcoinConfirmPanel.getMessageText2().trim());    
        
        // Wait a while and the message should be that it has completed the send.
        Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

        // Bitcoins should now be sent
        assertEquals("Wrong message - expecting success on messageText1", EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY, sendBitcoinConfirmPanel.getMessageText1());    
        assertEquals("Wrong message - expecting success on messageText2", "", sendBitcoinConfirmPanel.getMessageText2().trim());    
  
        // Set the action up to use test parameters and fail-on-send.
        sendBitcoinNowAction.setTestParameters(true, false);

        // Execute - this should complain that the wallet password is not set.
        sendBitcoinNowAction.actionPerformed(null);
        // Wait a while and the message should be that it has completed the send.
        Thread.sleep(DELAY_TO_UPDATE_MESSAGES);

        assertTrue("Wrong message - expecting no wallet password on messageText1, was '" + sendBitcoinConfirmPanel.getMessageText1() + "'", EXPECTED_ENTER_THE_WALLET_PASSWORD.equals(sendBitcoinConfirmPanel.getMessageText1()));
        
        // Set the wallet password.
        sendBitcoinConfirmPanel.setWalletPassword(WALLET_PASSWORD);
        
        // Execute - this should give the sending or failed message.
        sendBitcoinNowAction.actionPerformed(null);
        assertTrue("Wrong message - expecting sending/failed on messageText1 was '" + sendBitcoinConfirmPanel.getMessageText1() + "'", "".equals(sendBitcoinConfirmPanel.getMessageText1().trim()) || EXPECTED_SENDING_BITCOIN.equals(sendBitcoinConfirmPanel.getMessageText1()) ||
                EXPECTED_SEND_FAILED.equals(sendBitcoinConfirmPanel.getMessageText1()));    
        assertTrue("Wrong message - expecting blank/errormessage on messageText2 was '" + sendBitcoinConfirmPanel.getMessageText2() + "'", "".equals(sendBitcoinConfirmPanel.getMessageText2().trim()) || EXPECTED_TEST_SEND_FAILED_ERROR.equals(sendBitcoinConfirmPanel.getMessageText2()) ||
                EXPECTED_SEND_FAILED.equals(sendBitcoinConfirmPanel.getMessageText1()));    
        
        // Wait a while and the message should be that it has failed the send.
        Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

        assertEquals("Wrong message - expecting success on messageText1", EXPECTED_SEND_FAILED, sendBitcoinConfirmPanel.getMessageText1());    
        assertEquals("Wrong message - expecting success on messageText2", EXPECTED_TEST_SEND_FAILED_ERROR, sendBitcoinConfirmPanel.getMessageText2());    
  
    }
}
