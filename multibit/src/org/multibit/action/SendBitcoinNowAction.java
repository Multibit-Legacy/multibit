package org.multibit.action;

import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;

import com.google.bitcoin.core.Utils;

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
        // get the data out of the wallet preferences
        String sendAddress = controller.getModel().getWalletPreference(MultiBitModel.SEND_ADDRESS);
        String sendLabel = controller.getModel().getWalletPreference(MultiBitModel.SEND_LABEL);
        String sendAmount = controller.getModel().getWalletPreference(MultiBitModel.SEND_AMOUNT);
        String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
        BigInteger fee;
        if (sendFeeString == null || sendFeeString == "") {
            fee = MultiBitModel.SEND_FEE_DEFAULT;
        } else {
            fee = Utils.toNanoCoins(sendFeeString);
        }

        if (sendLabel != null && sendLabel != "") {
            WalletInfo addressBook = controller.getModel().getWalletInfo();
            addressBook.addSendingAddress(new AddressBookData(sendLabel, sendAddress));
        }
        controller.sendCoins(sendAddress, sendLabel, sendAmount, fee);

        controller.fireWalletDataChanged();
        controller.setActionForwardToParent();
    }

    public String getDisplayText() {
        // TODO localise
        return "sendNow";
    }
}
