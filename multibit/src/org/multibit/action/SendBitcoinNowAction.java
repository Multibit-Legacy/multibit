package org.multibit.action;

import java.io.IOException;
import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;

import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;

/**
 * an action that actually sends bitcoin
 * 
 * @author jim
 * 
 */
public class SendBitcoinNowAction implements Action {

    private MultiBitController controller;
    
    private final int MAX_LENGTH_OF_ERROR_MESSAGE = 70;

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
            fee = MultiBitModel.SEND_MINIMUM_FEE;
        } else {
            fee = Utils.toNanoCoins(sendFeeString);
        }

        if (sendLabel != null && sendLabel != "") {
            WalletInfo addressBook = controller.getModel().getWalletInfo();
            addressBook.addSendingAddress(new AddressBookData(sendLabel, sendAddress));
        }
        
        Boolean sendWasSuccessful = Boolean.FALSE;
        String errorMessage = " ";
        try {
            controller.sendCoins(sendAddress, sendLabel, sendAmount, fee);
            sendWasSuccessful = Boolean.TRUE;
        } catch (IOException e) {
            e.printStackTrace();
            errorMessage = e.getMessage();
        } catch (AddressFormatException e) {
            e.printStackTrace();
            errorMessage = e.getMessage();
        }

//        sendWasSuccessful = Boolean.FALSE;
//        errorMessage = "snuibbnfjhsbfjlsfbjslfbnsjkfb";
        
        if (errorMessage != null && errorMessage.length() > MAX_LENGTH_OF_ERROR_MESSAGE) {
            errorMessage = errorMessage.substring(0, MAX_LENGTH_OF_ERROR_MESSAGE) + "...";
        }
        controller.getModel().setUserPreference(MultiBitModel.SEND_WAS_SUCCESSFUL, sendWasSuccessful.toString());
        controller.getModel().setUserPreference(MultiBitModel.SEND_ERROR_MESSAGE, errorMessage);
    }

    public String getDisplayText() {
        // TODO localise
        return "sendNow";
    }
}
