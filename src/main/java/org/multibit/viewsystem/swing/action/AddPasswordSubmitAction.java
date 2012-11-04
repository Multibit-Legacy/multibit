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
import com.google.bitcoin.crypto.EncrypterDecrypter;
import com.google.bitcoin.crypto.EncrypterDecrypterException;
import com.google.bitcoin.crypto.EncrypterDecrypterScrypt;
import com.google.bitcoin.crypto.ScryptParameters;
import org.multibit.file.FileHandler;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletBusyListener;
import com.google.bitcoin.core.WalletMajorVersion;
import org.multibit.viewsystem.swing.view.AddPasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.Wallet;

/**
 * This {@link Action} action encrypts the private keys with the password.
 */
public class AddPasswordSubmitAction extends MultiBitSubmitAction implements WalletBusyListener {
    private static final Logger log = LoggerFactory.getLogger(AddPasswordSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private AddPasswordPanel addPasswordPanel;

    private JPasswordField password1;

    private JPasswordField password2;
    
    private File privateKeysBackupFile;

    /**
     * Creates a new {@link AddPasswordSubmitAction}.
     */
    public AddPasswordSubmitAction(MultiBitController controller, AddPasswordPanel addPasswordPanel,
            ImageIcon icon, JPasswordField password1, JPasswordField password2) {
        super(controller, "addPasswordSubmitAction.text", "addPasswordSubmitAction.tooltip", "addPasswordSubmitAction.mnemonicKey", icon);
        this.addPasswordPanel = addPasswordPanel;
        this.password1 = password1;
        this.password2 = password2;
        
        // This action is a WalletBusyListener.
        controller.registerWalletBusyListener(this);
        walletBusyChange(controller.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Add a password to a wallet.
     */
    public void actionPerformed(ActionEvent e) {
        addPasswordPanel.clearMessages();
        privateKeysBackupFile = null;

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
            if (controller.getModel().getActiveWalletWalletInfo() != null) {
                // Only an unencrypted protobuf wallet can have a password added to it.
                if (controller.getModel().getActiveWalletWalletInfo().getWalletMajorVersion() != WalletMajorVersion.PROTOBUF) {
                    addPasswordPanel.setMessage1(controller.getLocaliser().getString(
                            "addPasswordPanel.addPasswordFailed", new String[]{"Wallet is not protobuf.2"}));
                    return;
                }
            }

            PerWalletModelData perWalletModelData = null;
            try {
                // Double check wallet is not busy then declare that the active
                // wallet is busy with the task
                perWalletModelData = controller.getModel().getActivePerWalletModelData();

                if (!perWalletModelData.isBusy()) {
                    perWalletModelData.setBusy(true);
                    perWalletModelData.setBusyTask(controller.getLocaliser().getString("addPasswordSubmitAction.text"));

                    controller.fireWalletBusyChange(true);

                    if (wallet.getEncrypterDecrypter() == null) {
                        byte[] salt = new byte[ScryptParameters.SALT_LENGTH];
                        controller.getMultiBitService().getSecureRandom().nextBytes(salt);
                        ScryptParameters scryptParameters = new ScryptParameters(salt);
                        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);
                        wallet.setEncrypterDecrypter(encrypterDecrypter);
                    }

                    wallet.encrypt(passwordToUse);
                    controller.getModel().getActiveWalletWalletInfo().setWalletMajorVersion(WalletMajorVersion.PROTOBUF_ENCRYPTED);
                    controller.getModel().getActivePerWalletModelData().setDirty(true);
                    FileHandler fileHandler = new FileHandler(controller);
                    fileHandler.savePerWalletModelData(controller.getModel().getActivePerWalletModelData(), true);

                    privateKeysBackupFile = fileHandler.backupPrivateKeys(passwordToUse);

                }
            } catch (EncrypterDecrypterException ede) {
                ede.printStackTrace();
                addPasswordPanel.setMessage1(controller.getLocaliser().getString("addPasswordPanel.addPasswordFailed",
                        new String[] { ede.getMessage() }));
                return;
            } catch (IOException ede) {
                // Notify the user that the private key backup failed.
                addPasswordPanel.setMessage2(controller.getLocaliser().getString(
                        "changePasswordPanel.keysBackupFailed", new String[] { ede.getMessage() }));
                return;
            } finally {
                // Declare that wallet is no longer busy with the task.
                perWalletModelData.setBusyTask(null);
                perWalletModelData.setBusy(false);
                controller.fireWalletBusyChange(false);                   
            }
        }
        controller.fireDataChanged();

        // Success.
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                addPasswordPanel.clearMessages();
                addPasswordPanel.clearPasswords();
                addPasswordPanel.setMessage1(controller.getLocaliser().getString("addPasswordPanel.addPasswordSuccess")); 
                
                if (privateKeysBackupFile != null) {
                    try {
                        addPasswordPanel.setMessage2(controller.getLocaliser().getString("changePasswordPanel.keysBackupSuccess", new Object[]{privateKeysBackupFile.getCanonicalPath()}));
                    } catch (IOException e1) {
                        log.debug(e1.getClass().getCanonicalName() + " " + e1.getMessage());
                    }
                }
             }});
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
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("addPasswordSubmitAction.text"));
            }
        }
    }
}