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

import java.awt.event.ActionEvent;
import java.nio.CharBuffer;
import java.security.SignatureException;
import java.util.Iterator;

import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.SignMessagePanel;
import org.multibit.viewsystem.swing.view.panels.VerifyMessagePanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.params.KeyParameter;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WrongNetworkException;

/**
 * This {@link Action} signs a message
 */
public class SignMessageSubmitAction extends MultiBitSubmitAction implements WalletBusyListener {

    private static final Logger log = LoggerFactory.getLogger(ImportPrivateKeysSubmitAction.class);

    private static final long serialVersionUID = 1923333087598757765L;

    private MultiBitFrame mainFrame;
    private SignMessagePanel signMessagePanel;
    
    /**
     * Creates a new {@link SignMessageSubmitAction}.
     */
    public SignMessageSubmitAction(BitcoinController bitcoinController, MultiBitFrame mainFrame,
            SignMessagePanel signMessagePanel, ImageIcon icon) {
        super(bitcoinController, "signMessageAction.text", "signMessageAction.tooltip", "signMessageAction.mnemonicKey", icon);
        this.mainFrame = mainFrame;
        this.signMessagePanel = signMessagePanel;

        // This action is a WalletBusyListener.
        super.bitcoinController.registerWalletBusyListener(this);
        walletBusyChange(super.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Verify the message.
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        if (abort()) {
            return;
        }
        
        if (signMessagePanel == null) {
            return;
        }
        
        String addressText = null;
        if (signMessagePanel.getAddressTextArea() != null) {
            addressText = signMessagePanel.getAddressTextArea().getText();
        }
        
        String messageText = null;
        if (signMessagePanel.getMessageTextArea() != null) {
            messageText = signMessagePanel.getMessageTextArea().getText();
        }
        

        
        CharSequence walletPassword = null;
        if (signMessagePanel.getWalletPasswordField() != null) {
            walletPassword = CharBuffer.wrap(signMessagePanel.getWalletPasswordField().getPassword());
        }        
        
        log.debug("addressText = '" + addressText + "'");
        log.debug("messageText = '" + messageText + "'");
        
        try {
            Address signingAddress = new Address(bitcoinController.getModel().getNetworkParameters(), addressText);
            
            // Find the ECKey corresponding to the signing address.
            Wallet activeWallet = bitcoinController.getModel().getActiveWallet();
            Iterable<ECKey> keychain = activeWallet.getKeys();
            Iterator<ECKey> iterator = keychain.iterator();
            ECKey signingKey = null;
            while (iterator.hasNext()) {
                ECKey ecKey = iterator.next();
                if (ecKey.toAddress(bitcoinController.getModel().getNetworkParameters()).equals(signingAddress)) {
                    signingKey = ecKey;
                    break;
                }
            }
            if (signingKey == null) {
                // No signing key found.
                // TODO notify user - FAIL.
            } else {
                KeyParameter aesKey = null;
                if (signingKey.isEncrypted()) {
                    aesKey = signingKey.getKeyCrypter().deriveKey(walletPassword);
                    signingKey = signingKey.decrypt(signingKey.getKeyCrypter(), aesKey);
                }

                String signatureBase64 = signingKey.signMessage(messageText, aesKey);
                if (signMessagePanel.getSignatureTextArea() != null) {
                    signMessagePanel.getSignatureTextArea().setText(signatureBase64);
                    // TODO Notify user - SUCCESS.
                }        
            }

                //signMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.success"));
                //signMessagePanel.setMessageText2(" "); 

        } catch (WrongNetworkException e) {
            logError(e);
        } catch (AddressFormatException e) {
            logError(e);
        } 
    }
    
    private void logError(Exception e) {
        e.printStackTrace();
        signMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.error"));
        signMessagePanel.setMessageText2(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", 
                new String[] {e.getClass().getCanonicalName() + " " + e.getMessage()}));
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
                            new Object[] { controller.getLocaliser().getString(
                                    this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey()) }));
            setEnabled(false);
        } else {
            // Enable unless wallet has been modified by another process.
            if (!super.bitcoinController.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("signMessageAction.tooltip"));
                setEnabled(true);
            }
        }
    }
}