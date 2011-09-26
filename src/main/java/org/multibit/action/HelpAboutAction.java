package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to the help about view
 * @author jim
 *
 */
public class HelpAboutAction implements Action {

    private MultiBitController controller;
    
    public HelpAboutAction(MultiBitController controller) {
        this.controller = controller;     
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_HELP_ABOUT);       
    }
}
