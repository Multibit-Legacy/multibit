package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to open wallet view
 * 
 * @author jim
 *
 */
public class OpenWalletAction implements Action {

    private MultiBitController controller;
    
    public OpenWalletAction(MultiBitController controller) {
        this.controller = controller;    
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_OPEN_WALLET);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "openWallet";
    }
}
