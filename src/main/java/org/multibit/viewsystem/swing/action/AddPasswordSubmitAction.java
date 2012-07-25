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
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.file.FileHandler;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.AddPasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.Wallet;

/**
 * This {@link Action} action encrypts the private keys with the password.
 */
public class AddPasswordSubmitAction extends AbstractAction {
    private static final Logger log = LoggerFactory.getLogger(AddPasswordSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private MultiBitController controller;

    private AddPasswordPanel addPasswordPanel;
    private MultiBitFrame mainFrame;

    private JPasswordField password1;

    private JPasswordField password2;

    /**
     * Creates a new {@link AddPasswordSubmitAction}.
     */
    public AddPasswordSubmitAction(MultiBitController controller, AddPasswordPanel addPasswordPanel,
            ImageIcon icon, JPasswordField password1, JPasswordField password2, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("addPasswordSubmitAction.text"), icon);
        this.controller = controller;
        this.addPasswordPanel = addPasswordPanel;
        this.password1 = password1;
        this.password2 = password2;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("addPasswordSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("addPasswordSubmitAction.mnemonicKey"));
    }

    /**
     * Add a password to a wallet.
     */
    public void actionPerformed(ActionEvent e) {
        addPasswordPanel.clearMessages();


        char[] passwordToUse = null;

        // Get the passwords on the password fields.
        if (password1.getPassword() == null || password1.getPassword().length == 0) {
            // Notify the user must enter a password.
            addPasswordPanel.setMessage1(controller.getLocaliser()
                    .getString("addPasswordPanel.enterPasswords"));
            return;
        } else {
            if (!Arrays.areEqual(password1.getPassword(), password2.getPassword())) {
                // Notify user passwords are different.
                addPasswordPanel.setMessage1(controller.getLocaliser().getString(
                        "showExportPrivateKeysAction.passwordsAreDifferent"));
                return;
            } else {
                passwordToUse = password1.getPassword();
            }
        }
       
        Wallet wallet = controller.getModel().getActiveWallet();
        if (wallet != null) {
            try {
                wallet.encrypt(passwordToUse);
                FileHandler fileHandler = new FileHandler(controller);
                fileHandler.savePerWalletModelData( controller.getModel().getActivePerWalletModelData(), true);
            } catch (EncrypterDecrypterException ede) {
                ede.printStackTrace();
                addPasswordPanel.setMessage1(controller.getLocaliser().getString(
                "addPasswordPanel.addPasswordFailed", new String[]{ede.getMessage()}));
                return;
            }
        }
        controller.fireDataChanged();
        
        // Success.
        
        // Success.
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                addPasswordPanel.clearMessages();
                addPasswordPanel.clearPasswords();
                addPasswordPanel.updatePasswordAction();
                addPasswordPanel.setMessage1(controller.getLocaliser()
                        .getString("addPasswordPanel.addPasswordSuccess")); 
            }});
    }
}