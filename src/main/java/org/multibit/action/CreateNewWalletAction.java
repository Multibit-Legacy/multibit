package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to create a new wallet view
 * 
 * @author jim
 *
 */
public class CreateNewWalletAction implements Action {

    private MultiBitController controller;
    
    public CreateNewWalletAction(MultiBitController controller) {
        this.controller = controller;   
    }
    
    public void execute(DataProvider dataProvider) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_NEW_WALLET);       
    }
}
