package org.multibit.action;

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
    
    public RedisplayViewAction(MultiBitController controller) {
        this.controller = controller;    
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
