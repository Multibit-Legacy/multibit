package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;

/**
 * an action to the send bitcoin confirm view
 * @author jim
 *
 */
public class SendBitcoinConfirmAction implements Action {

    private MultiBitController controller;
    
    public SendBitcoinConfirmAction(MultiBitController controller) {
        this.controller = controller;    
    }
    
    public void execute(DataProvider dataProvider) {
        // copy the data into the user preferences (side effect of getData call)
        dataProvider.getData();
  
        String sendAddress = controller.getModel().getWalletPreference(MultiBitModel.SEND_ADDRESS);
        String sendAmount = controller.getModel().getWalletPreference(MultiBitModel.SEND_AMOUNT);

        Validator validator = new Validator(controller);
        if (validator.validate(sendAddress, sendAmount)) {
            controller.setActionForwardToChild(ActionForward.FORWARD_TO_SEND_BITCOIN_CONFIRM);
        } else {
            controller.setActionForwardToChild(ActionForward.FORWARD_TO_VALIDATION_ERROR);           
        }
    }
}
