package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to show the preferences view
 * 
 * @author jim
 *
 */
public class ShowPreferencesAction implements Action {

    private MultiBitController controller;
    private Localiser localiser;
    
    public ShowPreferencesAction(MultiBitController controller, Localiser localiser) {
        this.controller = controller;
        this.localiser = localiser;     
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_PREFERENCES);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "preferences";
    }
}
