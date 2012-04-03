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

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;

/**
 * This {@link Action} represents the swing copy address action
 */
public class CopyQRCodeTextAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;
    
    private DataProvider dataProvider;
    private MultiBitController controller;

    /**
     * Creates a new {@link CopyQRCodeTextAction}.
     */
    public CopyQRCodeTextAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("copyQRCodeTextAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyQRCodeTextAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyQRCodeTextAction.mnemonicKey"));
    }

    /**
     * process QR code
     */
    public void actionPerformed(ActionEvent event) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                boolean isReceive = false;

                Item isReceiveItem = data.getItem(MultiBitModel.IS_RECEIVE_BITCOIN);
                if (isReceiveItem != null && Boolean.TRUE.toString().equals(isReceiveItem.getNewValue())) {
                    isReceive = true;
                }
                Item amountItem = null;
                Item addressItem = null;
                Item labelItem = null;
                if (isReceive) {
                    amountItem = data.getItem(MultiBitModel.RECEIVE_AMOUNT);
                    addressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
                    labelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                } else {
                    amountItem = data.getItem(MultiBitModel.SEND_AMOUNT);
                    addressItem = data.getItem(MultiBitModel.SEND_ADDRESS);
                    labelItem = data.getItem(MultiBitModel.SEND_LABEL);
                }
                String amount = "";
                String address = "";
                String label = "";

                if (amountItem != null && amountItem.getNewValue() != null) {
                    amount = (String) amountItem.getNewValue();
                }

                if (addressItem != null && addressItem.getNewValue() != null) {
                    address = (String) addressItem.getNewValue();
                }

                if (labelItem != null && labelItem.getNewValue() != null) {
                    label = (String) labelItem.getNewValue();
                }

                String bitcoinURI;
                try {
                    bitcoinURI = com.google.bitcoin.uri.BitcoinURI.convertToBitcoinURI(new Address(controller.getMultiBitService().getNetworkParameters(), address), Utils.toNanoCoins(amount), label, null);
                } catch (AddressFormatException e) {
                    throw new RuntimeException(e);
                }

                // copy to clipboard
                TextTransfer textTransfer = new TextTransfer();
                textTransfer.setClipboardContents(bitcoinURI);
            }
        }
    }
}