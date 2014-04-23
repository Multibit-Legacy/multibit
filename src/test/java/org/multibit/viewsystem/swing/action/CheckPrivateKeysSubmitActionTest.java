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

import com.google.bitcoin.core.Wallet;
import junit.framework.TestCase;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.junit.Test;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.file.PrivateKeysHandlerTest;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.CheckPrivateKeysPanel;

import java.io.File;

public class CheckPrivateKeysSubmitActionTest extends TestCase {

  public static final CharSequence WALLET_PASSWORD = "the unbelievable lightness of being";
  public static final CharSequence WRONG_WALLET_PASSWORD = "wrong password";
  private static final String EXPECTED_ENTER_THE_WALLET_PASSWORD = "Enter the wallet password";
  private static final String EXPECTED_WRONG_WALLET_PASSWORD = "The wallet password is incorrect";
  private static final String EXPECTED_CHECKED_OK = "All private keys are OK. All private keys match their receiving addresses for the wallet \"testCheckPrivateKeysWithEncryptedWallet\".";

  private static final String BAD_WALLET_FILE = "badPrivateKeys.wallet";
  private static final String BAD_WALLET_CORRECT_PASSWORD = "password";
  private static final String EXPECTED_FAILED_CHECK = "Private key check FAIL for the wallet \"badPrivateKeys *password\". There are 5 private keys that do not match their receiving addresses.";

  @Test
  public void testCheckPrivateKeysWithGoodEncryptedWallet() throws Exception {
    // Create MultiBit controller.
    final CreateControllers.Controllers controllers = CreateControllers.createControllers();
    BitcoinController controller = controllers.bitcoinController;

    // Create a new wallet and put it in the model as the active wallet.
    ActionTestUtils.createNewActiveWallet(controller, "testCheckPrivateKeysWithEncryptedWallet", true, WALLET_PASSWORD);

    assertTrue("Wallet is not encrypted when it should be", controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

    // Create a new CheckPrivateKeysSubmitAction to test.
    FontSizer.INSTANCE.initialise(controller);
    CheckPrivateKeysPanel checkPanel = new CheckPrivateKeysPanel(controller);
    CheckPrivateKeysSubmitAction checkAction = checkPanel.getCheckPrivateKeysSubmitAction();

    assertNotNull("checkAction was not created successfully", checkAction);
    assertEquals("Wrong number of keys at wallet creation", 1, controller.getModel().getActiveWallet().getKeychain().size());
    assertTrue("Wallet password is not enabled when it should be", checkPanel.isWalletPasswordFieldEnabled());

    // Execute = this should fail as the password is not set yet
    checkAction.actionPerformed(null);
    assertEquals("Wrong message after execute with missing password", EXPECTED_ENTER_THE_WALLET_PASSWORD, checkPanel.getMessageText1());

    // Set the wrong wallet password.
    checkPanel.setWalletPassword(WRONG_WALLET_PASSWORD);

    // Execute = this should fail as the password is wrong
    checkAction.actionPerformed(null);
    assertEquals("Wrong message after execute with wrong password", EXPECTED_WRONG_WALLET_PASSWORD, checkPanel.getMessageText1());

    // Set the correct wallet password.
    checkPanel.setWalletPassword(WALLET_PASSWORD);

    // Execute = this should actually check the wallet private keys (and pass the check OK)
    checkAction.actionPerformed(null);

    assertEquals("Wrong message after good private key check", EXPECTED_CHECKED_OK, checkPanel.getMessageText1());
  }

  @Test
  public void testCheckPrivateKeysWithBadEncryptedWallet() throws Exception {
    // Create MultiBit controller.
    final CreateControllers.Controllers controllers = CreateControllers.createControllers();
    BitcoinController controller = controllers.bitcoinController;

    File directory = new File(".");
    String currentPath = directory.getAbsolutePath();

    String testDirectory = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
            + PrivateKeysHandlerTest.PRIVATE_KEYS_TESTDATA_DIRECTORY;
    String badWalletFile = testDirectory + File.separator + BAD_WALLET_FILE;

    // load up the bad wallet
    FileHandler fileHandler = new FileHandler(controller);
    Wallet badWallet = fileHandler.loadFromFile(new File(badWalletFile)).getWallet();

    assertNotNull(badWallet);

    // Create a new CheckPrivateKeysSubmitAction to test.
    FontSizer.INSTANCE.initialise(controller);
    CheckPrivateKeysPanel checkPanel = new CheckPrivateKeysPanel(controller);
    CheckPrivateKeysSubmitAction checkAction = checkPanel.getCheckPrivateKeysSubmitAction();

    assertNotNull("checkAction was not created successfully", checkAction);
    assertEquals("Wrong number of keys at wallet creation", 6, controller.getModel().getActiveWallet().getKeychain().size());
    assertTrue("Wallet password is not enabled when it should be", checkPanel.isWalletPasswordFieldEnabled());

    // Set the correct wallet password.
    checkPanel.setWalletPassword(BAD_WALLET_CORRECT_PASSWORD);

    // Execute = this should actually check the wallet private keys (and pass the check OK)
    checkAction.actionPerformed(null);

    assertEquals("Wrong message after failed private key check", EXPECTED_FAILED_CHECK, checkPanel.getMessageText1());
  }
}
