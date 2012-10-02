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

import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.file.WalletSaveException;
import org.multibit.file.WalletVersionException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;
import org.multibit.viewsystem.swing.view.AbstractTradePanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;

/**
 * This {@link Action} represents an action to create a receiving address.
 */
public class CreateNewReceivingAddressAction extends MultiBitSubmitAction {
    private static Logger log = LoggerFactory.getLogger(CreateNewReceivingAddressAction.class);

    private static final long serialVersionUID = 200152235465875405L;

    private AbstractTradePanel receiveBitcoinPanel;

    /**
     * Creates a new {@link CreateNewReceivingAddressAction}.
     */
    public CreateNewReceivingAddressAction(MultiBitController controller, AbstractTradePanel receiveBitcoinPanel) {
        super(controller, "createOrEditAddressAction.createReceiving.text", "createOrEditAddressAction.createReceiving.tooltip",
                "createOrEditAddressAction.createReceiving.mnemonicKey", null);
        this.receiveBitcoinPanel = receiveBitcoinPanel;
    }

    /**
     * Create new receiving address.
     */
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }

        // Check to see if the wallet files have changed.
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();

        ECKey newKey = new ECKey();
        perWalletModelData.getWallet().keychain.add(newKey);

        String addressString = newKey.toAddress(controller.getModel().getNetworkParameters()).toString();
        WalletInfo walletInfo = perWalletModelData.getWalletInfo();
        if (walletInfo == null) {
            walletInfo = new WalletInfo(perWalletModelData.getWalletFilename(), WalletVersion.PROTOBUF);
            perWalletModelData.setWalletInfo(walletInfo);
        }
        walletInfo.addReceivingAddress(new AddressBookData("", addressString), false);
        receiveBitcoinPanel.getAddressesTableModel().fireTableDataChanged();
        receiveBitcoinPanel.selectRows();

        controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS, addressString);
        controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL, "");

        try {
            controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
        } catch (WalletSaveException wse) {
            log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
            MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
        } catch (WalletVersionException wve) {
            log.error(wve.getClass().getCanonicalName() + " " + wve.getMessage());
            MessageManager.INSTANCE.addMessage(new Message(wve.getClass().getCanonicalName() + " " + wve.getMessage()));
        }
        controller.displayView(controller.getCurrentView());

        if (receiveBitcoinPanel != null && receiveBitcoinPanel.getLabelTextArea() != null) {
            receiveBitcoinPanel.getLabelTextArea().requestFocusInWindow();
        }
    }
}