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

import org.bouncycastle.util.Arrays;
import org.multibit.controller.MultiBitController;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.view.ExportPrivateKeysPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} exports the active wallets private keys
 */
public class ExportPrivateKeysSubmitAction extends AbstractAction {
    private static final Logger log = LoggerFactory.getLogger(ExportPrivateKeysSubmitAction.class);

    private static final long serialVersionUID = 1923492460598757765L;

    private MultiBitController controller;

    private ExportPrivateKeysPanel exportPrivateKeysPanel;

    private PrivateKeysHandler privateKeysHandler;

    private JPasswordField password1;

    private JPasswordField password2;

    /**
     * Creates a new {@link ExportPrivateKeysSubmitAction}.
     */
    public ExportPrivateKeysSubmitAction(MultiBitController controller, ExportPrivateKeysPanel exportPrivateKeysPanel,
            ImageIcon icon, JPasswordField password1, JPasswordField password2) {
        super(controller.getLocaliser().getString("showExportPrivateKeysAction.text"), icon);
        this.controller = controller;
        this.exportPrivateKeysPanel = exportPrivateKeysPanel;
        this.password1 = password1;
        this.password2 = password2;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showExportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * Export the private keys to a file
     */
    public void actionPerformed(ActionEvent e) {
        exportPrivateKeysPanel.clearMessages();

        // get the required output file
        String exportPrivateKeysFilename = exportPrivateKeysPanel.getOutputFilename();

        // check an output file was selected
        if (exportPrivateKeysFilename == null || "".equals(exportPrivateKeysFilename)) {
            exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                    "showExportPrivateKeysAction.youMustSelectAnOutputFile"));
            return;
        }

        // check on file overwrite
        File exportPrivateKeysFile = new File(exportPrivateKeysFilename);

        if (exportPrivateKeysFile.exists()) {
            String yesText = controller.getLocaliser().getString("showOpenUriView.yesText");
            String noText = controller.getLocaliser().getString("showOpenUriView.noText");
            String questionText = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwrite");
            String questionTitle = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwriteTitle");
            int selection = JOptionPane.showOptionDialog(exportPrivateKeysPanel, questionText, questionTitle,
                    JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,
                    ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE), new String[] { yesText, noText }, noText);
            if (selection != JOptionPane.YES_OPTION) {
                return;
            }
        }

        privateKeysHandler = new PrivateKeysHandler(controller.getMultiBitService().getNetworkParameters());

        boolean performEncryption = false;
        boolean performVerification = false;

        char[] passwordToUse = null;

        if (exportPrivateKeysPanel.requiresEncryption()) {
            // get the passwords on the password fields
            if (password1.getPassword() == null || password1.getPassword().length == 0) {
                // notify must enter a password
                exportPrivateKeysPanel.setMessage1(controller.getLocaliser()
                        .getString("showExportPrivateKeysAction.enterPasswords"));
                return;
            } else {
                if (!Arrays.areEqual(password1.getPassword(), password2.getPassword())) {
                    // notify user passwords are different
                    exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                            "showExportPrivateKeysAction.passwordsAreDifferent"));
                    return;
                } else {
                    // perform encryption
                    performEncryption = true;
                    passwordToUse = password1.getPassword();
                }
            }
        }

        try {
            privateKeysHandler.exportPrivateKeys(exportPrivateKeysFile, controller.getModel().getActivePerWalletModelData()
                    .getWallet(), controller.getMultiBitService().getChain(), performEncryption, passwordToUse);

            // success
            exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                    "showExportPrivateKeysAction.privateKeysExportSuccess"));
            performVerification = true;
        } catch (IOException ioe) {
            log.error(ioe.getClass().getName() + " " + ioe.getMessage());

            // IO failure of some sort
            exportPrivateKeysPanel.setMessage1(controller.getLocaliser().getString(
                    "showExportPrivateKeysAction.privateKeysExportFailure",
                    new Object[] { ioe.getClass().getName() + " " + ioe.getMessage() }));
        }

        if (performVerification) {
            // perform a verification on the exported file to see if it is
            // correct
            exportPrivateKeysPanel.setMessage2(privateKeysHandler.verifyExportFile(exportPrivateKeysFile, controller.getModel()
                    .getActivePerWalletModelData().getWallet(), controller.getMultiBitService().getChain(), performEncryption,
                    passwordToUse));
        }
    }
}