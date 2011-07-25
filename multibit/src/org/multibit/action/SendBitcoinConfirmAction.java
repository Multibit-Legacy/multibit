package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

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
        // no changes required to model
        
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_SEND_BITCOIN_CONFIRM);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "send";
    }
}
