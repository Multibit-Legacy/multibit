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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JPasswordField;

import org.spongycastle.util.Arrays;
import org.multibit.controller.MultiBitController;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.file.Verification;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.ExportPrivateKeysPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.MultiBitBlockChain;
import com.google.bitcoin.core.WalletType;

/**
 * This {@link Action} exports the active wallets private keys.
 */
public class ExportPrivateKeysSubmitAction extends MultiBitSubmitAction {
    private static final Logger log = LoggerFactory.getLogger(ExportPrivateKeysSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private ExportPrivateKeysPanel exportPrivateKeysPanel;
    private MultiBitFrame mainFrame;

    private PrivateKeysHandler privateKeysHandler;

    private JPasswordField walletPassword;

    private JPasswordField exportFilePassword;

    private JPasswordField exportFileRepeatPassword;

    /**
     * Creates a new {@link ExportPrivateKeysSubmitAction}.
     */ 
    public ExportPrivateKeysSubmitAction(MultiBitController controller, ExportPrivateKeysPanel exportPrivateKeysPanel,
            ImageIcon icon, JPasswordField walletPassword, JPasswordField exportFilePassword, JPasswordField exportFileRepeatPassword, MultiBitFrame mainFrame) {
        super(controller, "showExportPrivateKeysAction.text.camel", "showExportPrivateKeysAction.tooltip", "showExportPrivateKeysAction.mnemonicKey", icon);
                this.exportPrivateKeysPanel = exportPrivateKeysPanel;
        this.walletPassword = walletPassword;
        this.exportFilePassword = exportFilePassword;
        this.exportFileRepeatPassword = exportFileRepeatPassword;
        this.mainFrame = mainFrame;
    }

    /**
     * Export the private keys to a file.
     */
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }
        
        exportPrivateKeysPanel.clearMessages();

        // See if a wallet password is required and present.
        if (controller.getModel().getActiveWallet() != null && controller.getModel().getActiveWallet().getWalletType() == WalletType.ENCRYPTED) {
            if (walletPassword.getPassword() == null || walletPassword.getPassword().length == 0) {
                exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                "showExportPrivateKeysAction.youMustEnterTheWalletPassword"));
                return;
            }
            
            // See if the password is the correct wallet password. 
            if (!controller.getModel().getActiveWallet().checkPasswordCanDecryptFirstPrivateKey(walletPassword.getPassword())) {
                // The password supplied is incorrect.
                exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.passwordIsIncorrect"));
                exportPrivateKeysPanel.setMessage2(" ");
                return;
            }
        }
        
        // Get the required output file.
        String exportPrivateKeysFilename = exportPrivateKeysPanel.getOutputFilename();

        // Check an output file was selected.
        if (exportPrivateKeysFilename == null || "".equals(exportPrivateKeysFilename)) {
            exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
            "showExportPrivateKeysAction.youMustSelectAnOutputFile"));
            return;
        }

        File exportPrivateKeysFile = new File(exportPrivateKeysFilename);

        privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());

        boolean performEncryption = false;
        boolean performVerification = false;

        char[] exportPasswordToUse = null;

        if (exportPrivateKeysPanel.requiresEncryption()) {
            // Get the passwords on the export file password fields.
            if (exportFilePassword.getPassword() == null || exportFilePassword.getPassword().length == 0) {
                // Notify must enter a password.
                exportPrivateKeysPanel.setMessage1(controller.getLocaliser()
                        .getString("showExportPrivateKeysAction.enterPasswords"));
                return;
            } else {
                if (!Arrays.areEqual(exportFilePassword.getPassword(), exportFileRepeatPassword.getPassword())) {
                    // Notify user passwords are different.
                    exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                            "showExportPrivateKeysAction.passwordsAreDifferent"));
                    return;
                } else {
                    // Perform encryption.
                    performEncryption = true;
                    exportPasswordToUse = exportFilePassword.getPassword();
                }
            }
        }

        try {
            // Check on file overwrite.
            if (exportPrivateKeysFile.exists()) {
                String yesText = controller.getLocaliser().getString("showOpenUriView.yesText");
                String noText = controller.getLocaliser().getString("showOpenUriView.noText");
                String questionText = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwrite", new Object[] {exportPrivateKeysFile.getName()});
                String questionTitle = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwriteTitle");
                int selection = JOptionPane.showOptionDialog(mainFrame, questionText, questionTitle,
                        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,
                        ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE), new String[] { yesText, noText }, noText);
                if (selection != JOptionPane.YES_OPTION) {
                    return;
                }
            }

            MultiBitBlockChain blockChain = null;
            if (controller.getMultiBitService() != null) {
                blockChain = controller.getMultiBitService().getChain();
            }
            privateKeysHandler.exportPrivateKeys(exportPrivateKeysFile, controller.getModel().getActivePerWalletModelData()
                    .getWallet(), blockChain, performEncryption, exportPasswordToUse, walletPassword.getPassword());

            // Success.
            exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                    "showExportPrivateKeysAction.privateKeysExportSuccess"));
            performVerification = true;
            exportPrivateKeysPanel.clearPasswords();
        } catch (IOException ioe) {
            log.error(ioe.getClass().getName() + " " + ioe.getMessage());

            // IO failure of some sort.
            exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                    "showExportPrivateKeysAction.privateKeysExportFailure",
                    new Object[] { ioe.getClass().getName() + " " + ioe.getMessage() }));
        }

        if (performVerification) {
            // Perform a verification on the exported file to see if it is correct.
            MultiBitBlockChain blockChain = null;
            if (controller.getMultiBitService() != null) {
                blockChain = controller.getMultiBitService().getChain();
            }
            
            Verification verification = privateKeysHandler.verifyExportFile(exportPrivateKeysFile, controller.getModel()
                    .getActivePerWalletModelData().getWallet(), blockChain, performEncryption,
                    exportPasswordToUse,  walletPassword.getPassword());
            String verifyMessage = controller.getLocaliser().getString(verification.getMessageKey(), verification.getMessageData());
            exportPrivateKeysPanel.setMessage2(verifyMessage);
        }
    }
}