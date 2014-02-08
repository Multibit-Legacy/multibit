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
import org.junit.Before;
import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;

public class SendBitcoinNowActionTest extends TestCase {

    private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";
    private static final String EXPECTED_TEST_SEND_FAILED_ERROR = " test - send failed";
    private static final String EXPECTED_SEND_FAILED = "The send of your dogecoin failed.";
    private static final String EXPECTED_THE_WALLET_IS_BUSY = "The wallet is busy with the task \"\"";
    private static final String EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY = "Your dogecoin were sent successfully.";
    private static final String EXPECTED_SENDING_BITCOIN = "Sending dogecoin...";
    public static final CharSequence TEST_PASSWORD1 = "my hovercraft has eels";
    public static final CharSequence WALLET_PASSWORD = "testing testing 123";
    private static final int DELAY_TO_COMPLETE_OPERATION = 12000; // milliseconds
    private static final int DELAY_TO_UPDATE_MESSAGES = 4000; // milliseconds
    
    private File multiBitDirectory;

    private static final Logger log = LoggerFactory.getLogger(SendBitcoinNowActionTest.class);

    private BitcoinController controller;

    @Before
    @Override
    public void setUp() throws IOException {
        // Get the system property runFunctionalTest to see if the functional
        // tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            multiBitDirectory = createMultiBitRuntime();

            // set the application data directory to be the one we just created
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);

            // Create MultiBit controller.
            final CreateControllers.Controllers controllers = CreateControllers.createControllers(applicationDataDirectoryLocator);
            controller = controllers.bitcoinController;

            log.debug("Creating Bitcoin service");
            // create the MultiBitService that connects to the dogecoin network
            MultiBitService multiBitService = new MultiBitService(controller);
            controller.setMultiBitService(multiBitService);

            // Add the simple view system (no Swing).
            SimpleViewSystem simpleViewSystem = new SimpleViewSystem();
            controllers.coreController.registerViewSystem(simpleViewSystem);

            // MultiBit runtime is now setup and running
            // Wait for a peer connection.
            log.debug("Waiting for peer connection. . . ");
            while (!simpleViewSystem.isOnline()) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            log.debug("Now online.");

            // Wait a little longer to get a second connection.
            try {
                Thread.sleep(4000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
    @Test
    public void testSendBitcoinWithNonEncryptedWallet() throws Exception {
        // Get the system property runFunctionalTest to see if the functional
        // tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            // Create a new unencrypted wallet and put it in the model as the
            // active wallet.
            ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithNonEncryptedWallet", false, null);

            // Create a new SendBitcoinNowSubmitAction to test.
            FontSizer.INSTANCE.initialise(controller);
            SendBitcoinConfirmPanel sendBitcoinConfirmPanel = new SendBitcoinConfirmPanel(controller, null, null, null);
            SendBitcoinNowAction sendBitcoinNowAction = sendBitcoinConfirmPanel.getSendBitcoinNowAction();

            assertNotNull("sendBitcoinNowAction was not created successfully", sendBitcoinNowAction);
            assertTrue("Wallet password was enabled when it should not be", !sendBitcoinConfirmPanel.isWalletPasswordFieldEnabled());

            // Set the action up to use test parameters and succeed-on-send.
            sendBitcoinNowAction.setTestParameters(true, true);

            // Execute - this should give the sending message or sent message.
            sendBitcoinNowAction.actionPerformed(null);

            // Wait a while and the message should be that it has completed the
            // send.
            Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

            // Bitcoins should now be sent
            assertEquals("Wrong message - expecting success on messageText1", EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY,
                    sendBitcoinConfirmPanel.getMessageText1());
            assertEquals("Wrong message - expecting success on messageText2", "", sendBitcoinConfirmPanel.getMessageText2().trim());

            // Set the action up to use test parameters and fail-on-send.
            sendBitcoinNowAction.setTestParameters(true, false);

            // Wait for peer connections
            Thread.sleep(6000);

            // Execute - this should give the sending or failed message.
            sendBitcoinNowAction.actionPerformed(null);
            Thread.sleep(DELAY_TO_UPDATE_MESSAGES);
            assertTrue(
                    "Wrong message - expecting sending/sent on messageText1 was '" + sendBitcoinConfirmPanel.getMessageText1()
                    + "'",
                    "".equals(sendBitcoinConfirmPanel.getMessageText1().trim())
                    || EXPECTED_SENDING_BITCOIN.equals(sendBitcoinConfirmPanel.getMessageText1())
                    || EXPECTED_SEND_FAILED.equals(sendBitcoinConfirmPanel.getMessageText1()));
            assertEquals("Wrong message - expecting sending on messageText2", EXPECTED_TEST_SEND_FAILED_ERROR,
                    sendBitcoinConfirmPanel.getMessageText2());

            // Wait a while and the message should be that it has failed the send.
            Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

            assertEquals("Wrong message - expecting success on messageText1", EXPECTED_SEND_FAILED,
                    sendBitcoinConfirmPanel.getMessageText1());
            assertEquals("Wrong message - expecting success on messageText2", EXPECTED_TEST_SEND_FAILED_ERROR,
                    sendBitcoinConfirmPanel.getMessageText2());
        }
    }

    @Test
    public void testSendBitcoinWithEncryptedWallet() throws Exception {
        // Get the system property runFunctionalTest to see if the functional
        // tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            // Create a new encrypted wallet and put it in the model as the
            // active wallet.
            ActionTestUtils.createNewActiveWallet(controller, "testAddReceivingAddressesWithNonEncryptedWallet", true,
                    WALLET_PASSWORD);

            // Create a new SendBitcoinNowSubmitAction to test.
            FontSizer.INSTANCE.initialise(controller);
            SendBitcoinConfirmPanel sendBitcoinConfirmPanel = new SendBitcoinConfirmPanel(controller, null, null, null);
            SendBitcoinNowAction sendBitcoinNowAction = sendBitcoinConfirmPanel.getSendBitcoinNowAction();

            assertNotNull("sendBitcoinNowAction was not created successfully", sendBitcoinNowAction);
            assertTrue("Wallet password was disabled when it should not be", sendBitcoinConfirmPanel.isWalletPasswordFieldEnabled());

            // Set the action up to use test parameters and succeed-on-send.
            sendBitcoinNowAction.setTestParameters(true, true);

            // Wait for peer connections
            Thread.sleep(6000);

            // Execute - this should complain that the wallet password is not
            // set.
            sendBitcoinNowAction.actionPerformed(null);
            // Wait a while and the message should be that it has completed the
            // send.
            Thread.sleep(DELAY_TO_UPDATE_MESSAGES);

            assertTrue(
                    "Wrong message - expecting no wallet password on messageText1, was '"
                    + sendBitcoinConfirmPanel.getMessageText1() + "'",
                    EXPECTED_ENTER_THE_WALLET_PASSWORD.equals(sendBitcoinConfirmPanel.getMessageText1()));

            // Set the wallet password.
            sendBitcoinConfirmPanel.setWalletPassword(WALLET_PASSWORD);

            // Execute
            sendBitcoinNowAction.actionPerformed(null);

            // Wait a while and the message should be that it has completed the
            // send.
            Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

            // Bitcoins should now be sent
            assertEquals("Wrong message - expecting success on messageText1, sendBitcoinConfirmPanel = " + System.identityHashCode(sendBitcoinConfirmPanel), EXPECTED_YOUR_BITCOIN_WERE_SENT_SUCCESSFULLY,
                    sendBitcoinConfirmPanel.getMessageText1());
            assertEquals("Wrong message - expecting success on messageText2", "", sendBitcoinConfirmPanel.getMessageText2().trim());

            // Set the action up to use test parameters and fail-on-send.
            sendBitcoinNowAction.setTestParameters(true, false);

            // Execute - this should complain that the wallet password is not
            // set.
            sendBitcoinNowAction.actionPerformed(null);
            // Wait a while and the message should be that it has completed the
            // send.
            Thread.sleep(DELAY_TO_UPDATE_MESSAGES);

            assertTrue(
                    "Wrong message - expecting no wallet password on messageText1, was '"
                    + sendBitcoinConfirmPanel.getMessageText1() + "'",
                    EXPECTED_ENTER_THE_WALLET_PASSWORD.equals(sendBitcoinConfirmPanel.getMessageText1()));

            // Set the wallet password.
            sendBitcoinConfirmPanel.setWalletPassword(WALLET_PASSWORD);

            // Execute - this should give the sending or failed message.
            sendBitcoinNowAction.actionPerformed(null);
            assertTrue(
                    "Wrong message - expecting sending/failed/ wallet busy on messageText1 was '" + sendBitcoinConfirmPanel.getMessageText1()
                    + "'",
                    "".equals(sendBitcoinConfirmPanel.getMessageText1().trim())
                    || EXPECTED_SENDING_BITCOIN.equals(sendBitcoinConfirmPanel.getMessageText1())
                    || EXPECTED_SEND_FAILED.equals(sendBitcoinConfirmPanel.getMessageText1())
                    || EXPECTED_THE_WALLET_IS_BUSY.equals(sendBitcoinConfirmPanel.getMessageText1()));
            assertTrue(
                    "Wrong message - expecting blank/errormessage on messageText2 was '"
                    + sendBitcoinConfirmPanel.getMessageText2() + "'",
                    "".equals(sendBitcoinConfirmPanel.getMessageText2().trim())
                    || EXPECTED_TEST_SEND_FAILED_ERROR.equals(sendBitcoinConfirmPanel.getMessageText2())
                    || EXPECTED_SEND_FAILED.equals(sendBitcoinConfirmPanel.getMessageText1()));

            // Wait a while and the message should be that it has failed the send.
            Thread.sleep(DELAY_TO_COMPLETE_OPERATION);

            assertEquals("Wrong message - expecting success on messageText1", EXPECTED_SEND_FAILED,
                    sendBitcoinConfirmPanel.getMessageText1());
            assertEquals("Wrong message - expecting success on messageText2", EXPECTED_TEST_SEND_FAILED_ERROR,
                    sendBitcoinConfirmPanel.getMessageText2());
        }
    }
    
    /**
     * Create a working, portable runtime of MultiBit in a temporary directory.
     * 
     * @return the temporary directory the multibit runtime has been created in
     */
    private File createMultiBitRuntime() throws IOException {
        File multiBitDirectory = FileHandler.createTempDirectory("multibit");
        String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

        System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

        // Create an empty multibit.properties.
        File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multidoge.properties");
        multibitProperties.createNewFile();
        multibitProperties.deleteOnExit();

        // Copy in the checkpoints stored in git - this is in source/main/resources/.
        File multibitCheckpoints = new File(multiBitDirectoryPath + File.separator + "multidoge.checkpoints");
        FileHandler.copyFile(new File("./src/main/resources/multidoge.checkpoints"), multibitCheckpoints);
        multibitCheckpoints.deleteOnExit();

        return multiBitDirectory;
    }
}
