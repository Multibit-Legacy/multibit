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

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * This {@link Action} represents an action to create a sending address.
 */
public class CreateNewSendingAddressAction extends MultiBitSubmitAction {

    private static final long serialVersionUID = 200111935465875405L;

    private SendBitcoinPanel sendBitcoinPanel;

    /**
     * Creates a new {@link CreateNewSendingAddressAction}.
     */
    public CreateNewSendingAddressAction(BitcoinController bitcoinController, SendBitcoinPanel sendBitcoinPanel) {
        super(bitcoinController, "createOrEditAddressAction.createReceiving.text", "createOrEditAddressAction.createSending.tooltip",
                "createOrEditAddressAction.createSending.mnemonicKey", null);
        this.sendBitcoinPanel = sendBitcoinPanel;
    }

    /**
     * Create new send address.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }

        // Check to see if the wallet files have changed.
        WalletData perWalletModelData = super.bitcoinController.getModel().getActivePerWalletModelData();

        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();
        if (walletInfo == null) {
            walletInfo = new WalletInfoData(perWalletModelData.getWalletFilename(), perWalletModelData.getWallet(), MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
            perWalletModelData.setWalletInfo(walletInfo);
        }

        if (walletInfo.getSendingAddresses().isEmpty()) {
            String address = super.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.SEND_ADDRESS);
            String label = super.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.SEND_LABEL);

            perWalletModelData.getWalletInfo().addSendingAddress(new WalletAddressBookData(label, address));
            sendBitcoinPanel.getAddressesTableModel().fireTableDataChanged();
            super.bitcoinController.getModel().getActivePerWalletModelData().setDirty(true);
        } else {
            perWalletModelData.getWalletInfo().addSendingAddress(new WalletAddressBookData("", ""));
            sendBitcoinPanel.getAddressesTableModel().fireTableDataChanged();
            sendBitcoinPanel.selectRows();

            super.bitcoinController.getModel().setActiveWalletPreference(BitcoinModel.SEND_ADDRESS, "");
            super.bitcoinController.getModel().setActiveWalletPreference(BitcoinModel.SEND_LABEL, "");
        }
        
        sendBitcoinPanel.checkDeleteSendingEnabled();
        
        controller.displayView(controller.getCurrentView());

        if (sendBitcoinPanel.getLabelTextArea() != null) {
            sendBitcoinPanel.getLabelTextArea().requestFocusInWindow();
        }
    }
}