package org.multibit.action;

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
    
    public OkBackToParentAction(MultiBitController controller) {
        this.controller = controller;   
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        controller.setActionForwardToParent();       
    }
}
