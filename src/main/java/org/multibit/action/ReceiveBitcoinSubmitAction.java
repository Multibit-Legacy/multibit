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
package org.multibit.action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

/**
 * an action to process the submit of the Receive Bitcoin view
 * 
 * @author jim
 * 
 */
public class ReceiveBitcoinSubmitAction implements Action {

    private MultiBitController controller;

    public ReceiveBitcoinSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the receive address and label and put it in the user preferences and address book
        if (dataProvider != null) {
            String receiveAddress = null;
            String receiveLabel = null;

            Data data = dataProvider.getData();

            if (data != null) {
                Item receiveAddressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
                if (receiveAddressItem != null && receiveAddressItem.getNewValue() != null) {
                    receiveAddress = (String) receiveAddressItem.getNewValue();
                    controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS,
                            receiveAddress);
                }
                Item receiveLabelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                if (receiveLabelItem != null && receiveLabelItem.getNewValue() != null) {
                    receiveLabel = (String) receiveLabelItem.getNewValue();
                    controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL,
                            receiveLabel);
                }
            }
            
            if (receiveAddress != null) {
                if (receiveLabel == null) {
                    receiveLabel = "";
                }
                controller.getModel().getActiveWalletWalletInfo().addReceivingAddress(new AddressBookData(receiveLabel, receiveAddress), false);
            }
        }
        controller.setActionForwardToParent();
    }
}
