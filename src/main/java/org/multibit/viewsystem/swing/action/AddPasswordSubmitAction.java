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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

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
                // Notify must enter a password.
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


            log.debug("Password is : " + new String(passwordToUse));
            
//        try {
//            // check on file overwrite
//
//            if (exportPrivateKeysFile.exists()) {
//                String yesText = controller.getLocaliser().getString("showOpenUriView.yesText");
//                String noText = controller.getLocaliser().getString("showOpenUriView.noText");
//                String questionText = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwrite", new Object[] {exportPrivateKeysFile.getName()});
//                String questionTitle = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwriteTitle");
//                int selection = JOptionPane.showOptionDialog(mainFrame, questionText, questionTitle,
//                        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,
//                        ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE), new String[] { yesText, noText }, noText);
//                if (selection != JOptionPane.YES_OPTION) {
//                    return;
//                }
//            }
//
//            privateKeysHandler.exportPrivateKeys(exportPrivateKeysFile, controller.getModel().getActivePerWalletModelData()
//                    .getWallet(), controller.getMultiBitService().getChain(), performEncryption, passwordToUse);
//
//            // success
//            addPasswordPanel.setMessage1(controller.getLocaliser().getString(
//                    "showExportPrivateKeysAction.privateKeysExportSuccess"));
//            performVerification = true;
//        } catch (IOException ioe) {
//            log.error(ioe.getClass().getName() + " " + ioe.getMessage());
//
//            // IO failure of some sort
//            addPasswordPanel.setMessage1(controller.getLocaliser().getString(
//                    "showExportPrivateKeysAction.privateKeysExportFailure",
//                    new Object[] { ioe.getClass().getName() + " " + ioe.getMessage() }));
//        }
//
//        if (performVerification) {
//            // perform a verification on the exported file to see if it is correct
//            Verification verification = privateKeysHandler.verifyExportFile(exportPrivateKeysFile, controller.getModel()
//                    .getActivePerWalletModelData().getWallet(), controller.getMultiBitService().getChain(), performEncryption,
//                    passwordToUse);
//            String verifyMessage = controller.getLocaliser().getString(verification.getMessageKey(), verification.getMessageData());
//            addPasswordPanel.setMessage2(verifyMessage);
//        }
    }
}