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

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.AddPasswordPanel;
import org.multibit.viewsystem.swing.view.ChangePasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.WalletType;

/**
 * This {@link Action} action decrypts private keys with the old password and then encrypts the private keys with the new password.
 */
public class ChangePasswordSubmitAction extends AbstractAction {
    private static final Logger log = LoggerFactory.getLogger(ChangePasswordSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private MultiBitController controller;

    private ChangePasswordPanel changePasswordPanel;
    private MultiBitFrame mainFrame;

    private JPasswordField currentPassword;
    
    private JPasswordField newPassword;

    private JPasswordField repeatNewPassword;

    /**
     * Creates a new {@link ChangePasswordSubmitAction}.
     */
    public ChangePasswordSubmitAction(MultiBitController controller, ChangePasswordPanel changePasswordPanel,
            ImageIcon icon, JPasswordField currentPassword, JPasswordField newPassword, JPasswordField repeatNewPassword, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("changePasswordSubmitAction.text"), icon);
        this.controller = controller;
        this.changePasswordPanel = changePasswordPanel;
        this.currentPassword = currentPassword;
        this.newPassword = newPassword;
        this.repeatNewPassword = repeatNewPassword;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("changePasswordSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("changePasswordSubmitAction.mnemonicKey"));
    }

    /**
     * Change the wallet password
     */
    public void actionPerformed(ActionEvent e) {
        changePasswordPanel.clearMessages();

        char[] newPasswordToUse = null;
        char[] currentPasswordToUse = null;

        if (currentPassword.getPassword() == null || currentPassword.getPassword().length == 0) {
            // Notify must enter the current password.
            changePasswordPanel.setMessage1(controller.getLocaliser()
                    .getString("changePasswordPanel.enterCurrentPassword"));
            return;
        } 
        currentPasswordToUse = currentPassword.getPassword();

        // Get the new passwords on the password fields.
        if (newPassword.getPassword() == null || newPassword.getPassword().length == 0) {
            // Notify the user must enter a new password.
            changePasswordPanel.setMessage1(controller.getLocaliser()
                    .getString("changePasswordPanel.enterPasswords"));
            return;
        } else {
            if (!Arrays.areEqual(newPassword.getPassword(), repeatNewPassword.getPassword())) {
                // Notify user passwords are different.
                changePasswordPanel.setMessage1(controller.getLocaliser().getString(
                            "showExportPrivateKeysAction.passwordsAreDifferent"));
                return;
            } else {
                newPasswordToUse = newPassword.getPassword();
            }
        }

        log.debug("Current password is : " + new String(currentPasswordToUse));
        log.debug("New password is : " + new String(newPasswordToUse));

        changePasswordPanel.clearMessages();
        changePasswordPanel.clearPasswords();
        
        controller.fireDataChanged();
    }
}