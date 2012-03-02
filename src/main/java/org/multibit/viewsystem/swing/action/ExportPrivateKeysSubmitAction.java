/**
 * Copyright 2011 multibit.org
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
import javax.swing.JPasswordField;

import org.bouncycastle.util.Arrays;
import org.multibit.controller.MultiBitController;
import org.multibit.file.PrivateKeysHandler;
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
        privateKeysHandler = new PrivateKeysHandler(controller.getMultiBitService().getNetworkParameters());

        String message1 = controller.getLocaliser().getString("showExportPrivateKeysAction.noDataWasWritten");
        
        boolean performEncryption = false;
        char[] passwordToUse = null;
        
        if (exportPrivateKeysPanel.requiresEncryption()) {
            // get the passwords on the password fields
            if (password1.getPassword() == null) {
                // notify must enter a password
                return;
            } else {
                if (!Arrays.areEqual(password1.getPassword(), password2.getPassword())) {
                    // notify user passwords are different
                    return;
                } else {
                    // perform encryption
                    performEncryption = true;
                    passwordToUse = password1.getPassword();
                }
            }
        }
        
        // get the required output file
        String exportPrivateKeysFilename = exportPrivateKeysPanel.getOutputFilename();

        try {
            privateKeysHandler.exportPrivateKeys(new File(exportPrivateKeysFilename), controller.getModel()
                    .getActivePerWalletModelData().getWallet(), controller.getMultiBitService().getChain(), performEncryption, passwordToUse);

            // success
            message1 = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportSuccess");
        } catch (IOException ioe) {
            log.error(ioe.getClass().getName() + " " + ioe.getMessage());

            // failure
            message1 = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportFailure",
                    new Object[] { ioe.getClass().getName() + " " + ioe.getMessage() });
        }

        // perform a verification on the exported file to see if it is correct
        String message2 = privateKeysHandler.verifyExportFile(new File(exportPrivateKeysFilename), controller.getModel()
                .getActivePerWalletModelData().getWallet(), performEncryption, passwordToUse);
        
        exportPrivateKeysPanel.setMessage1(message1);
        exportPrivateKeysPanel.setMessage2(message2);
    }
}