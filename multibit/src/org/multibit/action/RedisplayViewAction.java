package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to redisplay the same view again
 * @author jim
 *
 */
public class RedisplayViewAction implements Action {

    private MultiBitController controller;
    private Localiser localiser;
    
    public RedisplayViewAction(MultiBitController controller, Localiser localiser) {
        this.controller = controller;
        this.localiser = localiser;     
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "redo";
    }

}
