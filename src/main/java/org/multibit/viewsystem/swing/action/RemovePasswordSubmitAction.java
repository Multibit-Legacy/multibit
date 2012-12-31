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

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import com.google.bitcoin.crypto.EncrypterDecrypterException;
import org.multibit.file.FileHandler;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletBusyListener;
import com.google.bitcoin.core.WalletMajorVersion;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.RemovePasswordPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Wallet;

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
    public RemovePasswordSubmitAction(MultiBitController controller, RemovePasswordPanel removePasswordPanel,
            ImageIcon icon, JPasswordField password1, MultiBitFrame mainFrame) {
        super(controller, "removePasswordSubmitAction.text", "removePasswordSubmitAction.tooltip", "removePasswordSubmitAction.mnemonicKey", icon);
        this.removePasswordPanel = removePasswordPanel;
        this.password1 = password1;
        
        // This action is a WalletBusyListener.
        controller.registerWalletBusyListener(this);
        walletBusyChange(controller.getModel().getActivePerWalletModelData().isBusy());
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
       
        if (controller.getModel().getActiveWallet() != null) {
            Wallet wallet = controller.getModel().getActiveWallet();
            if (wallet != null) {

                    PerWalletModelData perWalletModelData = null;
                    try {
                        // Double check wallet is not busy then declare that the active
                        // wallet is busy with the task
                        perWalletModelData = controller.getModel().getActivePerWalletModelData();

                        if (!perWalletModelData.isBusy()) {
                            perWalletModelData.setBusy(true);
                            perWalletModelData.setBusyTask(controller.getLocaliser().getString("removePasswordSubmitAction.text"));

                            controller.fireWalletBusyChange(true);

                            wallet.removeEncryption(wallet.getEncrypterDecrypter().deriveKey(passwordToUse));
                            controller.getModel().getActiveWalletWalletInfo().setWalletMajorVersion(WalletMajorVersion.PROTOBUF);
                            controller.getModel().getActivePerWalletModelData().setDirty(true);
                            FileHandler fileHandler = new FileHandler(controller);
                            fileHandler.savePerWalletModelData( controller.getModel().getActivePerWalletModelData(), true);
                        }
                    } catch (EncrypterDecrypterException ede) {
                        removePasswordPanel.setMessage1(controller.getLocaliser()
                                .getString("removePasswordPanel.removePasswordFailed", new String[]{ede.getMessage()}));
                        return;
                    } finally {
                        // Declare that wallet is no longer busy with the task.
                        perWalletModelData.setBusyTask(null);
                        perWalletModelData.setBusy(false);
                        controller.fireWalletBusyChange(false);                   
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
                removePasswordPanel.setMessage1(controller.getLocaliser()
                        .getString("removePasswordPanel.removePasswordSuccess")); 
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
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("removePasswordSubmitAction.text"));
            }
        }
    }
}