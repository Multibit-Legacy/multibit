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

    /**
     * Creates a new {@link ExportPrivateKeysSubmitAction}.
     */
    public ExportPrivateKeysSubmitAction(MultiBitController controller, ExportPrivateKeysPanel exportPrivateKeysPanel,
            ImageIcon icon) {
        super(controller.getLocaliser().getString("showExportPrivateKeysAction.text"), icon);
        this.controller = controller;
        this.exportPrivateKeysPanel = exportPrivateKeysPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showExportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * Export the private keys to a file
     */
    public void actionPerformed(ActionEvent e) {
        privateKeysHandler = new PrivateKeysHandler(controller.getMultiBitService().getNetworkParameters());

        String message = controller.getLocaliser().getString("showExportPrivateKeysAction.noDataWasWritten");

        // get the required output file
        String exportPrivateKeysFilename = exportPrivateKeysPanel.getOutputFilename();

        try {
            privateKeysHandler.exportPrivateKeys(new File(exportPrivateKeysFilename), controller.getModel()
                    .getActivePerWalletModelData().getWallet(), controller.getMultiBitService().getChain());

            // success
            message = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportSuccess");
        } catch (IOException ioe) {
            log.error(ioe.getClass().getName() + " " + ioe.getMessage());

            // failure
            message = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportFailure",
                    new Object[] { ioe.getClass().getName() + " " + ioe.getMessage() });
        }

        exportPrivateKeysPanel.setMessage(message);

        // controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
    }
}