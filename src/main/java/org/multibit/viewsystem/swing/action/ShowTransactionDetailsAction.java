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

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletTableData;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.TransactionDetailsDialog;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;

/**
 * This {@link Action} shows the transaction details dialog
 */
public class ShowTransactionDetailsAction extends AbstractAction {

    private static final long serialVersionUID = 1913592498732457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private MultiBitFrame mainFrame;
    private ShowTransactionsPanel showTransactionsPanel;

    /**
     * Creates a new {@link ShowTransactionDetailsAction}.
     */
    public ShowTransactionDetailsAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, ShowTransactionsPanel showTransactionsPanel) {
        super(bitcoinController.getLocaliser().getString("showTransactionsDetailAction.text"), ImageLoader.createImageIcon(ImageLoader.TRANSACTIONS_ICON_FILE));
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.showTransactionsPanel = showTransactionsPanel;
        
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showTransactionsDetailAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showTransactionsDetailAction.mnemonicKey"));
    }

    /**
     * show the show transaction details dialog
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        WalletTableData rowTableData = showTransactionsPanel.getSelectedRowData();

        final TransactionDetailsDialog transactionDetailsDialog = new TransactionDetailsDialog(this.bitcoinController, mainFrame, rowTableData);
        transactionDetailsDialog.setVisible(true);
        
        // Put the focus back on the table so that the up and down arrows work.
        showTransactionsPanel.getTable().requestFocusInWindow();
    }
}