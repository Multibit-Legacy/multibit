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

import javax.swing.Action;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.CreateNewReceivingAddressDialog;
import org.multibit.viewsystem.swing.view.panels.ReceiveBitcoinPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} represents the action to create the create new receiving
 * address dialog.
 */
public class CreateNewReceivingAddressAction extends MultiBitSubmitAction {
    private static Logger log = LoggerFactory.getLogger(CreateNewReceivingAddressAction.class);

    private static final long serialVersionUID = 200152235465875405L;

    private ReceiveBitcoinPanel receiveBitcoinPanel;
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link CreateNewReceivingAddressAction}.
     */
    public CreateNewReceivingAddressAction(BitcoinController bitcoinController, MultiBitFrame mainFrame,
            ReceiveBitcoinPanel receiveBitcoinPanel) {
        super(bitcoinController, "createOrEditAddressAction.createReceiving.text", "createOrEditAddressAction.createReceiving.tooltip",
                "createOrEditAddressAction.createReceiving.mnemonicKey", null);
        this.receiveBitcoinPanel = receiveBitcoinPanel;
        this.mainFrame = mainFrame;
    }

    /**
     * Create new receiving address dialog.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }

        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            CreateNewReceivingAddressDialog createNewReceivingAddressDialog = new CreateNewReceivingAddressDialog(super.bitcoinController,
                    mainFrame, receiveBitcoinPanel);
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