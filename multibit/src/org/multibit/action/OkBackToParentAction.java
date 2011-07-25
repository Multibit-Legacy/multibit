package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to go back to the home page
 * (typically used in an OK button on a messagebox)
 * 
 * @author jim
 *
 */
public class OkBackToParentAction implements Action {

    private MultiBitController controller;
    private Localiser localiser;
    
    public OkBackToParentAction(MultiBitController controller, Localiser localiser) {
        this.controller = controller;
        this.localiser = localiser;     
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        controller.setActionForwardToParent();       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "ok";
    }
}
