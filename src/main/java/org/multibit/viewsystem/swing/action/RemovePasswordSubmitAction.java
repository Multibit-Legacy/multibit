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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncryptableWallet;
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.RemovePasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;

/**
 * This {@link Action} action removes the encryption of private keys in a wallet.
 */
public class RemovePasswordSubmitAction extends AbstractAction {
    private static final Logger log = LoggerFactory.getLogger(RemovePasswordSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private MultiBitController controller;

    private RemovePasswordPanel removePasswordPanel;
    private MultiBitFrame mainFrame;

    private JPasswordField password1;

    /**
     * Creates a new {@link RemovePasswordSubmitAction}.
     */
    public RemovePasswordSubmitAction(MultiBitController controller, RemovePasswordPanel removePasswordPanel,
            ImageIcon icon, JPasswordField password1, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("removePasswordSubmitAction.text"), icon);
        this.controller = controller;
        this.removePasswordPanel = removePasswordPanel;
        this.password1 = password1;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("removePasswordSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("removePasswordSubmitAction.mnemonicKey"));
    }

    /**
     * Remove the password protection on a wallet.
     */
    public void actionPerformed(ActionEvent e) {
        removePasswordPanel.clearMessages();


        char[] passwordToUse = password1.getPassword();

        // Get the passwords on the password fields.
        if (password1.getPassword() == null || password1.getPassword().length == 0) {
            // Notify that the user must enter a password.
            removePasswordPanel.setMessage1(controller.getLocaliser()
                    .getString("removePasswordPanel.enterPassword"));
            return;
        }
       
        if (controller.getModel().getActiveWallet() != null) {
            Wallet wallet = controller.getModel().getActiveWallet();
            if (wallet != null) {
                if (wallet instanceof EncryptableWallet) {
                    try {
                        ((EncryptableWallet)wallet).decrypt(passwordToUse);
                        wallet.setWalletType(WalletType.UNENCRYPTED);
                    } catch (EncrypterDecrypterException ede) {
                        removePasswordPanel.setMessage1(controller.getLocaliser()
                                .getString("removePasswordPanel.removePasswordFailed", new String[]{ede.getMessage()}));
                        return;
                    }
                }
            }
        }
        controller.fireDataChanged();

        // Success.
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                removePasswordPanel.clearMessages();
                removePasswordPanel.clearPasswords();
                removePasswordPanel.updatePasswordAction();
                removePasswordPanel.setMessage1(controller.getLocaliser()
                        .getString("removePasswordPanel.removePasswordSuccess")); 
            }});
    }
}