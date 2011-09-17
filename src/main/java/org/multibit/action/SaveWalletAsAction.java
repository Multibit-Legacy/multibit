package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to save wallet as view
 * 
 * @author jim
 *
 */
public class SaveWalletAsAction implements Action {

    private MultiBitController controller;
    
    public SaveWalletAsAction(MultiBitController controller) {
        this.controller = controller;   
    }
    
    public void execute(DataProvider dataProvider) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_SAVE_WALLET_AS);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "saveWallet";
    }
}
