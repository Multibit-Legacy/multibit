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

import java.awt.Cursor;
import java.awt.event.ActionEvent;

import javax.swing.Action;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.DeleteSendingAddressConfirmDialog;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;

/**
 * This {@link Action} show the delete sending address confirmation dialog.
 */
public class DeleteSendingAddressAction extends MultiBitSubmitAction {
    private static final long serialVersionUID = 1333933460523457765L;

    private MultiBitFrame mainFrame;
    private SendBitcoinPanel sendBitcoinPanel;

    /**
     * Creates a new {@link DeleteSendingAddressAction}.
     */
    public DeleteSendingAddressAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, SendBitcoinPanel sendBitcoinPanel) {
        super(bitcoinController, "deleteSendingAddressSubmitAction.text", "deleteSendingAddressSubmitAction.tooltip", "deleteSendingAddressSubmitAction.mnemonic", null);
        this.mainFrame = mainFrame;
        this.sendBitcoinPanel = sendBitcoinPanel;
    }

    /**
     * Prompt for deletion of a sending address.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);
  
        try {
            DeleteSendingAddressConfirmDialog deleteSendingAddressConfirmDialog = new DeleteSendingAddressConfirmDialog(super.bitcoinController, mainFrame, sendBitcoinPanel);
            deleteSendingAddressConfirmDialog.setVisible(true);
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }
}
