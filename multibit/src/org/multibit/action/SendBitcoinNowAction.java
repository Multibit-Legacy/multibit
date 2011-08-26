package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBook;
import org.multibit.model.AddressBookData;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;

/**
 * an action that actually sends bitcoin
 * 
 * @author jim
 * 
 */
public class SendBitcoinNowAction implements Action {

    private MultiBitController controller;

    public SendBitcoinNowAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the data out of the user preferences
        String sendAddress = controller.getModel().getUserPreference(MultiBitModel.SEND_ADDRESS);
        String sendLabel = controller.getModel().getUserPreference(MultiBitModel.SEND_LABEL);
        String sendAmount = controller.getModel().getUserPreference(MultiBitModel.SEND_AMOUNT);

        if (sendLabel != null && sendLabel != "") {
            AddressBook addressBook = controller.getModel().getAddressBook();
            addressBook.addSendingAddress(new AddressBookData(sendLabel, sendAddress));
        }
        controller.sendCoins(sendAddress, sendLabel, sendAmount);

        controller.fireWalletDataChanged();
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_TRANSACTIONS);
    }

    public String getDisplayText() {
        // TODO localise
        return "sendNow";
    }
}
