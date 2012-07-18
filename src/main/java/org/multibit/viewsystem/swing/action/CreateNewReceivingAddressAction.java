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

import java.awt.Cursor;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.AbstractTradePanel;
import org.multibit.viewsystem.swing.view.CreateNewReceivingAddressDialog;
import org.multibit.viewsystem.swing.view.DeleteWalletConfirmDialog;
import org.multibit.viewsystem.swing.view.ReceiveBitcoinPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} represents the action to create the create new receiving address dialog.
 */
public class CreateNewReceivingAddressAction extends AbstractAction {
    private static Logger log = LoggerFactory.getLogger(CreateNewReceivingAddressAction.class);
    
    private static final long serialVersionUID = 200152235465875405L;

    private MultiBitController controller;

    private ReceiveBitcoinPanel receiveBitcoinPanel;
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link CreateNewReceivingAddressAction}.
     */
    public CreateNewReceivingAddressAction(MultiBitController controller, MultiBitFrame mainFrame, ReceiveBitcoinPanel receiveBitcoinPanel) {
        super(controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.text"));
        this.controller = controller;
        this.receiveBitcoinPanel = receiveBitcoinPanel;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.createReceiving.mnemonicKey"));
    }

    /**
     * Create new receiving address dialog.
     */
    public void actionPerformed(ActionEvent e) {
        // Check to see if the wallet files have changed.
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            setEnabled(false);
      
            try {
                CreateNewReceivingAddressDialog createNewReceivingAddressDialog = new CreateNewReceivingAddressDialog(controller, mainFrame, receiveBitcoinPanel);
                createNewReceivingAddressDialog.setVisible(true);
            } finally {
                setEnabled(true);
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
            
            if (receiveBitcoinPanel != null && receiveBitcoinPanel.getLabelTextArea() != null) {
                receiveBitcoinPanel.getLabelTextArea().requestFocusInWindow();
            }
        }
    }
}