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

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.viewsystem.swing.view.AbstractTradePanel;

import com.google.bitcoin.core.ECKey;

/**
 * This {@link Action} represents an action to create a receiving address
 */
public class CreateNewReceivingAddressAction extends AbstractAction {

    private static final long serialVersionUID = 200152235465875405L;

    private MultiBitController controller;

    private AbstractTradePanel receiveBitcoinPanel;

    /**
     * Creates a new {@link CreateNewReceivingAddressAction}.
     */
    public CreateNewReceivingAddressAction(MultiBitController controller, AbstractTradePanel receiveBitcoinPanel) {
        super(controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.text"));
        this.controller = controller;
        this.receiveBitcoinPanel = receiveBitcoinPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.createReceiving.mnemonicKey"));
    }

    /**
     * create new receiving address
     */
    public void actionPerformed(ActionEvent e) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            ECKey newKey = new ECKey();
            perWalletModelData.getWallet().keychain.add(newKey);

            String addressString = newKey.toAddress(controller.getMultiBitService().getNetworkParameters()).toString();
            WalletInfo walletInfo = perWalletModelData.getWalletInfo();
            if (walletInfo == null) {
                walletInfo = new WalletInfo(perWalletModelData.getWalletFilename());
                perWalletModelData.setWalletInfo(walletInfo);
            }
            walletInfo.addReceivingAddress(new AddressBookData("", addressString), false);
            controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS, addressString);
            controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL, "");
            
            controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);

            controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);

            receiveBitcoinPanel.getFormPanel().requestFocusInWindow();
            receiveBitcoinPanel.getLabelTextArea().requestFocusInWindow();
        }
    }
}