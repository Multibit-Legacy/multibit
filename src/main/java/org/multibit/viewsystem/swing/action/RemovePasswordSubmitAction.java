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
import java.nio.CharBuffer;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.BackupManager;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.RemovePasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypterException;

/**
 * This {@link Action} action removes the encryption of private keys in a wallet.
 */
public class RemovePasswordSubmitAction extends MultiBitSubmitAction implements WalletBusyListener {
    private static final Logger log = LoggerFactory.getLogger(RemovePasswordSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private RemovePasswordPanel removePasswordPanel;
    private JPasswordField password1;

    /**
     * Creates a new {@link RemovePasswordSubmitAction}.
     */
    public RemovePasswordSubmitAction(BitcoinController bitcoinController, RemovePasswordPanel removePasswordPanel,
            ImageIcon icon, JPasswordField password1, MultiBitFrame mainFrame) {
        super(bitcoinController, "removePasswordSubmitAction.text", "removePasswordSubmitAction.tooltip", "removePasswordSubmitAction.mnemonicKey", icon);
        this.removePasswordPanel = removePasswordPanel;
        this.password1 = password1;
        
        // This action is a WalletBusyListener.
        super.bitcoinController.registerWalletBusyListener(this);
        walletBusyChange(super.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Remove the password protection on a wallet.
     */
    @Override
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
       
        if (super.bitcoinController.getModel().getActiveWallet() != null) {
            Wallet wallet = super.bitcoinController.getModel().getActiveWallet();
            if (wallet != null) {
                    WalletData perWalletModelData = null;
                    WalletInfoData walletInfoData = null;
                    
                    try {
                        // Double check wallet is not busy then declare that the active
                        // wallet is busy with the task
                        perWalletModelData = super.bitcoinController.getModel().getActivePerWalletModelData();
                        walletInfoData = super.bitcoinController.getModel().getActiveWalletWalletInfo();

                        if (!perWalletModelData.isBusy()) {
                            perWalletModelData.setBusy(true);
                            perWalletModelData.setBusyTaskKey("removePasswordSubmitAction.text");

                            super.bitcoinController.fireWalletBusyChange(true);

                            wallet.decrypt(wallet.getKeyCrypter().deriveKey(CharBuffer.wrap(passwordToUse)));
                            walletInfoData.setWalletVersion(MultiBitWalletVersion.PROTOBUF);
                            perWalletModelData.setDirty(true);
                            FileHandler fileHandler = new FileHandler(super.bitcoinController);
                            fileHandler.savePerWalletModelData(perWalletModelData, true);
                            
                            // Backup the wallet and wallet info.
                            BackupManager.INSTANCE.backupPerWalletModelData(fileHandler, perWalletModelData);
                        }
                    } catch (KeyCrypterException kce) {
                        removePasswordPanel.setMessage1(controller.getLocaliser()
                                .getString("removePasswordPanel.removePasswordFailed", new String[]{kce.getMessage()}));
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
        }
        controller.fireDataChangedUpdateNow();

        // Success.
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                removePasswordPanel.clearMessages();
                removePasswordPanel.clearPasswords();
                removePasswordPanel.setMessage1(controller.getLocaliser()
                        .getString("removePasswordPanel.removePasswordSuccess")); 
            }});
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
        // Update the enable status of the action to match the wallet busy status.
        if (super.bitcoinController.getModel().getActivePerWalletModelData().isBusy()) {
            // Wallet is busy with another operation that may change the private keys - Action is disabled.
            putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("multiBitSubmitAction.walletIsBusy", 
                    new Object[]{controller.getLocaliser().getString(this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey())}));         
        } else {
            // Enable unless wallet has been modified by another process.
            if (!super.bitcoinController.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("removePasswordSubmitAction.text"));
            }
        }
    }
}