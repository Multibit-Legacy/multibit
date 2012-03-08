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

import org.multibit.controller.MultiBitController;
import org.multibit.model.WalletTableData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.TransactionDetailsDialog;

/**
 * This {@link Action} shows the transaction details dialog
 */
public class ShowTransactionDetailsAction extends AbstractAction {

    private static final long serialVersionUID = 1913592498732457765L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    private WalletTableData rowTableData;

    /**
     * Creates a new {@link ShowTransactionDetailsAction}.
     */
    public ShowTransactionDetailsAction(MultiBitController controller, MultiBitFrame mainFrame, WalletTableData rowTableData) {
        super(controller.getLocaliser().getString("showTransactionsDetailAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.rowTableData = rowTableData;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showTransactionsDetailAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showTransactionsDetailAction.mnemonicKey"));
    }

    /**
     * show the send bitcoin confirm dialog
     */
    public void actionPerformed(ActionEvent e) {
            TransactionDetailsDialog transactionDetailsDialog = new TransactionDetailsDialog(controller, mainFrame, rowTableData);
            transactionDetailsDialog.setVisible(true);
    }
}