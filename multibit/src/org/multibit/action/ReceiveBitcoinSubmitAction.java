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
                    controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_ADDRESS,
                            receiveAddress);
                }
                Item receiveLabelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                if (receiveLabelItem != null && receiveLabelItem.getNewValue() != null) {
                    receiveLabel = (String) receiveLabelItem.getNewValue();
                    controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_LABEL,
                            receiveLabel);
                }
            }
            
            if (receiveAddress != null) {
                if (receiveLabel == null) {
                    receiveLabel = "";
                }
                controller.getModel().getWalletInfo().addReceivingAddress(new AddressBookData(receiveLabel, receiveAddress), false);
            }
        }
        controller.setActionForwardToParent();
    }

    public String getDisplayText() {
        // would not normally be seen
        return "receiveBitcoinSubmit";
    }
}
