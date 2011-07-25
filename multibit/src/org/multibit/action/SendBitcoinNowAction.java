package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action that actually sends bitcoin
 * @author jim
 *
 */
public class SendBitcoinNowAction implements Action {

    private MultiBitController controller;
    private Localiser localiser;
    
    public SendBitcoinNowAction(MultiBitController controller, Localiser localiser) {
        this.controller = controller;
        this.localiser = localiser;     
    }
    
    public void execute(DataProvider dataProvider) {
        // TODO - Actually send the bitcoin
        controller.displayMessage("sendBitcoinNowAction.message", null, "");
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_HOME_PAGE);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "sendNow";
    }
}
