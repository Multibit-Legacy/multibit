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
import java.io.File;
import java.io.IOException;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletBusyListener;
import org.multibit.viewsystem.swing.view.panels.ChangePasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.EncrypterDecrypterException;

/**
 * This {@link Action} action decrypts private keys with the old password and then encrypts the private keys with the new password.
 */
public class ChangePasswordSubmitAction extends MultiBitSubmitAction implements WalletBusyListener {
    private static final Logger log = LoggerFactory.getLogger(ChangePasswordSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private ChangePasswordPanel changePasswordPanel;

    private JPasswordField currentPassword;
    
    private JPasswordField newPassword;

    private JPasswordField repeatNewPassword;
    
    private File privateKeysBackupFile;

    /**
     * Creates a new {@link ChangePasswordSubmitAction}.
     */
    public ChangePasswordSubmitAction(MultiBitController controller, ChangePasswordPanel changePasswordPanel,
            ImageIcon icon, JPasswordField currentPassword, JPasswordField newPassword, JPasswordField repeatNewPassword) {
        super(controller, "changePasswordSubmitAction.text", "changePasswordSubmitAction.tooltip", "changePasswordSubmitAction.mnemonicKey", icon);
        this.changePasswordPanel = changePasswordPanel;
        this.currentPassword = currentPassword;
        this.newPassword = newPassword;
        this.repeatNewPassword = repeatNewPassword;
        
        // This action is a WalletBusyListener.
        controller.registerWalletBusyListener(this);
        walletBusyChange(controller.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Change the wallet password.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        changePasswordPanel.clearMessages();
        privateKeysBackupFile = null;

        char[] newPasswordToUse = null;
        char[] currentPasswordToUse = null;

        if (currentPassword.getPassword() == null || currentPassword.getPassword().length == 0) {
            // Notify must enter the current password.
            changePasswordPanel.setMessage1(controller.getLocaliser().getString("changePasswordPanel.enterCurrentPassword"));
            return;
        }
        currentPasswordToUse = currentPassword.getPassword();

        // Get the new passwords on the password fields.
        if (newPassword.getPassword() == null || newPassword.getPassword().length == 0) {
            // Notify the user must enter a new password.
            changePasswordPanel.setMessage1(controller.getLocaliser().getString("changePasswordPanel.enterPasswords"));
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

        Wallet wallet = controller.getModel().getActiveWallet();
        if (wallet != null) {
            // Double check wallet is not busy then declare that the active
            // wallet is busy with the task.
            PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();

            if (!perWalletModelData.isBusy()) {
                perWalletModelData.setBusy(true);
                perWalletModelData.setBusyTask(controller.getLocaliser().getString("changePasswordSubmitAction.text"));

                controller.fireWalletBusyChange(true);

                boolean decryptSuccess = false;
                try {
                    wallet.decrypt(wallet.getEncrypterDecrypter().deriveKey(currentPasswordToUse));
                    decryptSuccess = true;
                } catch (EncrypterDecrypterException ede) {
                    // Notify the user that the decrypt failed.
                    changePasswordPanel.setMessage1(controller.getLocaliser().getString("changePasswordPanel.changePasswordFailed",
                            new String[] { ede.getMessage() }));

                    // Declare that wallet is no longer busy with the task.
                    perWalletModelData.setBusyTask(null);
                    perWalletModelData.setBusy(false);
                    controller.fireWalletBusyChange(false);
                    
                    return;
                }

                if (decryptSuccess) {
                    try {
                        wallet.encrypt(wallet.getEncrypterDecrypter().deriveKey(newPasswordToUse));
                        FileHandler fileHandler = new FileHandler(controller);
                        fileHandler.savePerWalletModelData(controller.getModel().getActivePerWalletModelData(), true);
                        
                        privateKeysBackupFile = fileHandler.backupPrivateKeys(newPasswordToUse);
                    } catch (EncrypterDecrypterException ede) {
                        // Notify the user that the encrypt failed.
                        changePasswordPanel.setMessage1(controller.getLocaliser().getString(
                                "changePasswordPanel.changePasswordFailed", new String[] { ede.getMessage() }));
                        return;
                    } catch (IOException ede) {
                        // Notify the user that the private key backup failed.
                        changePasswordPanel.setMessage2(controller.getLocaliser().getString(
                                "changePasswordPanel.keysBackupFailed", new String[] { ede.getMessage() }));
                        return;
                    } finally {
                        // Declare that wallet is no longer busy with the task.
                        perWalletModelData.setBusyTask(null);
                        perWalletModelData.setBusy(false);
                        controller.fireWalletBusyChange(false);
                    }
                } else {
                    // Declare that wallet is no longer busy with the task.
                    perWalletModelData.setBusyTask(null);
                    perWalletModelData.setBusy(false);
                    controller.fireWalletBusyChange(false);
                }
            }
        }

        // Success.
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                changePasswordPanel.clearMessages();
                changePasswordPanel.clearPasswords();
                changePasswordPanel.setMessage1(controller.getLocaliser().getString("changePasswordPanel.changePasswordSuccess"));
                if (privateKeysBackupFile != null) {
                    try {
                        changePasswordPanel.setMessage2(controller.getLocaliser().getString(
                                "changePasswordPanel.keysBackupSuccess", new Object[] { privateKeysBackupFile.getCanonicalPath() }));
                    } catch (IOException e1) {
                        log.debug(e1.getClass().getCanonicalName() + " " + e1.getMessage());
                    }
                }
            }
        });
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
        // Update the enable status of the action to match the wallet busy status.
        if (controller.getModel().getActivePerWalletModelData().isBusy()) {
            // Wallet is busy with another operation that may change the private keys - Action is disabled.
            putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("multiBitSubmitAction.walletIsBusy", new Object[]{controller.getModel().getActivePerWalletModelData().getBusyOperation()}));         
        } else {
            // Enable unless wallet has been modified by another process.
            if (!controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("changePasswordSubmitAction.text"));
            }
        }
    }
}