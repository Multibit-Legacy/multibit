/**
 * Copyright 2013 multibit.org
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

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.viewsystem.swing.view.panels.CheckPrivateKeysPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.params.KeyParameter;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.math.BigInteger;
import java.nio.CharBuffer;
import java.util.List;

/**
 * This {@link javax.swing.Action} checks private keys against the bitcoin addresses
 */
public class CheckPrivateKeysSubmitAction extends MultiBitSubmitAction implements WalletBusyListener {

  private static final Logger log = LoggerFactory.getLogger(CheckPrivateKeysSubmitAction.class);

  private static final long serialVersionUID = 1923333087598757765L;

  public static final String MESSAGE_WINDOW_SEPARATOR = "----------------------------------------------------------------";

  private CheckPrivateKeysPanel checkPrivateKeysPanel;

  /**
   * Creates a new {@link org.multibit.viewsystem.swing.action.CheckPrivateKeysSubmitAction}.
   */
  public CheckPrivateKeysSubmitAction(BitcoinController bitcoinController,
                                      CheckPrivateKeysPanel checkPrivateKeysPanel, ImageIcon icon) {
    super(bitcoinController, "checkPrivateKeysAction.text", "showCheckPrivateKeysAction.tooltip", "showCheckPrivateKeysAction.mnemonicKey", icon);
    this.checkPrivateKeysPanel = checkPrivateKeysPanel;

    // This action is a WalletBusyListener.
    super.bitcoinController.registerWalletBusyListener(this);
    walletBusyChange(super.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
  }

  /**
   * Check the private keys against the bitcoin addresses
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    if (abort()) {
      return;
    }

    if (checkPrivateKeysPanel == null) {
      return;
    }

    CharSequence walletPassword = null;
    if (checkPrivateKeysPanel.getWalletPasswordField() != null) {
      walletPassword = CharBuffer.wrap(checkPrivateKeysPanel.getWalletPasswordField().getPassword());

      if (bitcoinController.getModel().getActiveWallet().isEncrypted()) {
        if (walletPassword.length() == 0) {
          checkPrivateKeysPanel.setMessageText1(controller.getLocaliser().getString(
                  "showExportPrivateKeysAction.youMustEnterTheWalletPassword"));
          checkPrivateKeysPanel.setMessageText2(" ");
          return;
        }

        if (!bitcoinController.getModel().getActiveWallet().checkPassword(walletPassword)) {
          // The password supplied is incorrect.
          checkPrivateKeysPanel.setMessageText1(controller.getLocaliser().getString(
                  "createNewReceivingAddressSubmitAction.passwordIsIncorrect"));
          checkPrivateKeysPanel.setMessageText2(" ");
          return;
        }
      }
    }

    // Check the private keys match the bitcoin addresses
    try {
      checkPrivateKeysMatchAddresses(bitcoinController.getModel().getActivePerWalletModelData(), walletPassword);
    } catch (PrivateKeysException pke) {
      // Error messaging is handled in the method itself
      pke.printStackTrace();
    }
  }

  @Override
  public void walletBusyChange(boolean newWalletIsBusy) {
    // Update the enable status of the action to match the wallet busy
    // status.
    if (super.bitcoinController.getModel().getActivePerWalletModelData().isBusy()) {
      // Wallet is busy with another operation that may change the private
      // keys - Action is disabled.
      putValue(
              SHORT_DESCRIPTION,
              controller.getLocaliser().getString(
                      "multiBitSubmitAction.walletIsBusy",
                      new Object[]{controller.getLocaliser().getString(
                              this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey())}
              )
      );
      setEnabled(false);
    } else {
      // Enable
      putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showCheckPrivateKeysAction.tooltip"));
      setEnabled(true);
    }
  }

  /**
   * Check that the private key in the wallet file correctly creates the bitcoin address
   *
   * @param perWalletModelData the wallet data to check the private keys for
   * @param password the wallet password
   * @return badAddresses A list of the bad addresses i.e. the addresses for which the private key does not match the bitcoin address.
   *                      If this is empty then all private keys are present and match the address.
   */
  private List<String> checkPrivateKeysMatchAddresses(WalletData perWalletModelData, CharSequence password) throws PrivateKeysException {
    if (perWalletModelData == null || perWalletModelData.getWallet() == null) {
      throw new PrivateKeysException("No wallet specified");
    }

    Message separatorMessage = new Message(MESSAGE_WINDOW_SEPARATOR);
    separatorMessage.setShowInStatusBar(false);

    boolean allKeysAreOk = true;
    List<String> badAddresses = Lists.newArrayList();

    try {
      Wallet walletToCheck = perWalletModelData.getWallet();
      List<ECKey> keysToCheck = walletToCheck.getKeys();

      // Derive keyParameter if wallet is encrypted
      KeyParameter keyParameter = null;
      if (password != null && !password.equals("") && walletToCheck.isEncrypted()) {
        keyParameter = walletToCheck.getKeyCrypter().deriveKey(password);
      }

      for (ECKey loopECKey : keysToCheck) {
        Address originalAddress = loopECKey.toAddress(NetworkParameters.fromID(NetworkParameters.ID_MAINNET));
        try {

          // Decrypt the ECKey if it is encrypted
          if (loopECKey.isEncrypted()) {
            loopECKey = loopECKey.decrypt(walletToCheck.getKeyCrypter(), keyParameter);
          }

          byte[] privateKeyBytes = loopECKey.getPrivKeyBytes();
          if (privateKeyBytes == null) {
            // The private key in the ecKey is missing
            allKeysAreOk = false;

            badAddresses.add(originalAddress.toString());
          } else {
            // Create an ECKey with just the private key bytes, it creates the public key - the address should be the same
            ECKey rebornKey = new ECKey(new BigInteger(1, privateKeyBytes), null, loopECKey.isCompressed());
            Address rebornAddress = rebornKey.toAddress(NetworkParameters.fromID(NetworkParameters.ID_MAINNET));
            if (!rebornAddress.toString().equals(originalAddress.toString())) {
              // The private key in the ecKey does not match the address - private key could be damaged or missing
              allKeysAreOk = false;
              badAddresses.add(originalAddress.toString());
            }
          }
        } catch (Exception e) {
          e.printStackTrace();
          allKeysAreOk = false;
          badAddresses.add(originalAddress.toString());
        }
      }

      MessageManager.INSTANCE.addMessage(separatorMessage);

      if (allKeysAreOk) {
        // No problems
        String messageText = super.bitcoinController.getLocaliser().getString("checkPrivateKeysSubmitAction.ok", new String[]{perWalletModelData.getWalletDescription()});
        checkPrivateKeysPanel.setMessageText1(messageText);
        checkPrivateKeysPanel.setMessageText2("");

        Message message = new Message(messageText);
        message.setShowInStatusBar(false);
        MessageManager.INSTANCE.addMessage(message);
      } else {
        // Some private keys are missing or damaged
        String messageText = super.bitcoinController.getLocaliser().getString("checkPrivateKeysSubmitAction.fail", new String[]{perWalletModelData.getWalletDescription(), "" + badAddresses.size()});
        checkPrivateKeysPanel.setMessageText1(messageText);
        checkPrivateKeysPanel.setMessageText2(super.bitcoinController.getLocaliser().getString("checkPrivateKeysSubmitAction.details"));

        Message message = new Message(messageText);
        message.setShowInStatusBar(false);
        MessageManager.INSTANCE.addMessage(message);

        message = new Message(super.bitcoinController.getLocaliser().getString("checkPrivateKeysSubmitAction.badAddresses", new String[]{Joiner.on(", ").join(badAddresses)}));
        message.setShowInStatusBar(false);
        MessageManager.INSTANCE.addMessage(message);

        message = new Message(super.bitcoinController.getLocaliser().getString("checkPrivateKeysSubmitAction.doNotSend"));
        message.setShowInStatusBar(false);
        MessageManager.INSTANCE.addMessage(message);
      }
    } catch (Exception e) {
      String messageText1 = super.bitcoinController.getLocaliser().getString("checkPrivateKeysSubmitAction.didNotComplete");
      String messageText2 = super.bitcoinController.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new String[]{e.getClass().getCanonicalName() + " " + e.getMessage()});
      checkPrivateKeysPanel.setMessageText1(messageText1);
      checkPrivateKeysPanel.setMessageText2(messageText2);
      Message message = new Message(messageText1);
      message.setShowInStatusBar(false);
      MessageManager.INSTANCE.addMessage(message);

      message = new Message(messageText2);
      message.setShowInStatusBar(false);
      MessageManager.INSTANCE.addMessage(message);

      throw new PrivateKeysException("The check of the private keys failed", e);
    } finally {
      MessageManager.INSTANCE.addMessage(separatorMessage);
    }

    return badAddresses;
  }
}