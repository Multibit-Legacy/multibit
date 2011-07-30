package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
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
        // copy the data into the user preferences (side effect of getData call)
        Data data = dataProvider.getData();
        
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_SEND_BITCOIN_CONFIRM);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "send";
    }
}
