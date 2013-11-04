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
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.WrongNetworkException;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.VerifyMessagePanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.security.SignatureException;

/**
 * This {@link Action} verifies a signed message.
 */
public class VerifyMessageSubmitAction extends MultiBitSubmitAction implements WalletBusyListener {

    private static final Logger log = LoggerFactory.getLogger(ImportPrivateKeysSubmitAction.class);

    private static final long serialVersionUID = 1923333087598757765L;

    private MultiBitFrame mainFrame;
    private VerifyMessagePanel verifyMessagePanel;
    
    /**
     * Creates a new {@link VerifyMessageSubmitAction}.
     */
    public VerifyMessageSubmitAction(BitcoinController bitcoinController, MultiBitFrame mainFrame,
            VerifyMessagePanel verifyMessagePanel, ImageIcon icon) {
        super(bitcoinController, "verifyMessageAction.text", "verifyMessageAction.tooltip", "verifyMessageAction.mnemonicKey", icon);
        this.mainFrame = mainFrame;
        this.verifyMessagePanel = verifyMessagePanel;

        // This action is a WalletBusyListener.
        super.bitcoinController.registerWalletBusyListener(this);
        walletBusyChange(super.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Verify the message.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        if (verifyMessagePanel == null) {
            return;
        }
        
        String addressText = null;
        if (verifyMessagePanel.getAddressTextArea() != null) {
            addressText = verifyMessagePanel.getAddressTextArea().getText();
            if (addressText != null) {
              addressText = WhitespaceTrimmer.trim(addressText);
            }
        }
        
        String messageText = null;
        if (verifyMessagePanel.getMessageTextArea() != null) {
            messageText = verifyMessagePanel.getMessageTextArea().getText();
        }
        
        String signatureText = null;
        if (verifyMessagePanel.getSignatureTextArea() != null) {
            signatureText = verifyMessagePanel.getSignatureTextArea().getText();
        }
        
        log.debug("addressText = '" + addressText + "'");
        log.debug("messageText = '" + messageText + "'");
        log.debug("signatureText = '" + signatureText + "'");
        
        if (addressText == null || "".equals(addressText)) {
            verifyMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.noAddress"));
            verifyMessagePanel.setMessageText2(" ");  
            return;
        }
        
        if (messageText == null || "".equals(messageText.trim())) {
            verifyMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.noMessage"));
            verifyMessagePanel.setMessageText2(" ");  
            return;
        }
        
        if (signatureText == null || "".equals(signatureText.trim())) {
            verifyMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.noSignature"));
            verifyMessagePanel.setMessageText2(" ");  
            return;
        }
        
        try {
            Address expectedAddress = new Address(bitcoinController.getModel().getNetworkParameters(), addressText);
            ECKey key = ECKey.signedMessageToKey(messageText, signatureText);
            Address gotAddress = key.toAddress(bitcoinController.getModel().getNetworkParameters());
            if (expectedAddress != null && expectedAddress.equals(gotAddress)) {
                log.debug("The message was signed by the specified address");
                verifyMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.success"));
                verifyMessagePanel.setMessageText2(" "); 
            } else {
                log.debug("The message was NOT signed by the specified address"); 
                verifyMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.failure"));
                verifyMessagePanel.setMessageText2(" "); 
            }
        } catch (WrongNetworkException e) {
            logError(e);
        } catch (AddressFormatException e) {
            logError(e);
        } catch (SignatureException e) {
            logError(e);
        }
    }
    
    private void logError(Exception e) {
        e.printStackTrace();
        verifyMessagePanel.setMessageText1(controller.getLocaliser().getString("verifyMessageAction.error"));
        verifyMessagePanel.setMessageText2(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", 
                new String[] {e.getClass().getCanonicalName() + " " + e.getMessage()}));
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
    }
}