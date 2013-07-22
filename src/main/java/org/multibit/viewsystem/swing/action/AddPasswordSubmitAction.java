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
import java.nio.CharBuffer;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.BackupManager;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.viewsystem.swing.view.panels.AddPasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterException;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.protobuf.ByteString;

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
    public AddPasswordSubmitAction(BitcoinController bitcoinController, AddPasswordPanel addPasswordPanel,
            ImageIcon icon, JPasswordField password1, JPasswordField password2) {
        super(bitcoinController, "addPasswordSubmitAction.text", "addPasswordSubmitAction.tooltip", "addPasswordSubmitAction.mnemonicKey", icon);
        
        this.addPasswordPanel = addPasswordPanel;
        this.password1 = password1;
        this.password2 = password2;
        
        // This action is a WalletBusyListener.
        this.bitcoinController.registerWalletBusyListener(this);
        walletBusyChange(this.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Add a password to a wallet.
     */
    @Override
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
       
        Wallet wallet = this.bitcoinController.getModel().getActiveWallet();
        if (wallet != null) {
            if (this.bitcoinController.getModel().getActiveWalletWalletInfo() != null) {
                // Only an unencrypted protobuf wallet can have a password added to it.
                if (this.bitcoinController.getModel().getActiveWalletWalletInfo().getWalletVersion() != MultiBitWalletVersion.PROTOBUF) {
                    addPasswordPanel.setMessage1(this.bitcoinController.getLocaliser().getString(
                            "addPasswordPanel.addPasswordFailed", new String[]{"Wallet is not protobuf.2"}));
                    return;
                }
            }

            WalletData perWalletModelData = null;
            WalletInfoData walletInfoData = null;
            try {
                // Double check wallet is not busy then declare that the active
                // wallet is busy with the task
                perWalletModelData = this.bitcoinController.getModel().getActivePerWalletModelData();
                walletInfoData = this.bitcoinController.getModel().getActiveWalletWalletInfo();

                if (!perWalletModelData.isBusy()) {
                    perWalletModelData.setBusy(true);
                    perWalletModelData.setBusyTaskKey("addPasswordSubmitAction.text");

                    super.bitcoinController.fireWalletBusyChange(true);

                    KeyCrypter keyCrypterToUse;
                    if (wallet.getKeyCrypter() == null) {
                        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
                        super.bitcoinController.getMultiBitService().getSecureRandom().nextBytes(salt);
                        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
                        ScryptParameters scryptParameters = scryptParametersBuilder.build();
                        keyCrypterToUse = new KeyCrypterScrypt(scryptParameters);
                    } else {
                        keyCrypterToUse = wallet.getKeyCrypter();
                    }

                    wallet.encrypt(keyCrypterToUse, keyCrypterToUse.deriveKey(CharBuffer.wrap(passwordToUse)));
                    walletInfoData.setWalletVersion(MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
                    perWalletModelData.setDirty(true);
                    FileHandler fileHandler = new FileHandler(super.bitcoinController);
                    fileHandler.savePerWalletModelData(perWalletModelData, true);

                    // Backup the private keys.
                    privateKeysBackupFile = fileHandler.backupPrivateKeys(CharBuffer.wrap(passwordToUse));

                    // Backup the wallet and wallet info.
                    BackupManager.INSTANCE.backupPerWalletModelData(fileHandler,perWalletModelData);
                    
                    // Ensure that any unencrypted wallet backups are file encrypted with the wallet password.
                    BackupManager.INSTANCE.fileLevelEncryptUnencryptedWalletBackups(perWalletModelData, CharBuffer.wrap(passwordToUse));
                }
            } catch (KeyCrypterException ede) {
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
                if (perWalletModelData != null) {
                    perWalletModelData.setBusyTaskKey(null);
                    perWalletModelData.setBusy(false);
                }
                super.bitcoinController.fireWalletBusyChange(false);                   
            }
        }
        controller.fireDataChangedUpdateNow();

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
        if (this.bitcoinController.getModel().getActivePerWalletModelData().isBusy()) {
            // Wallet is busy with another operation that may change the private keys - Action is disabled.
            putValue(SHORT_DESCRIPTION, this.bitcoinController.getLocaliser().getString("multiBitSubmitAction.walletIsBusy", 
                    new Object[]{this.bitcoinController.getLocaliser().getString(this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey())}));         
        } else {
            // Enable unless wallet has been modified by another process.
            if (!this.bitcoinController.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                putValue(SHORT_DESCRIPTION, this.bitcoinController.getLocaliser().getString("addPasswordSubmitAction.text"));
            }
        }
    }
}