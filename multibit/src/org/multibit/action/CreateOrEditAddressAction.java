package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to create or edit an address
 * @author jim
 *
 */
public class CreateOrEditAddressAction implements Action {

    private MultiBitController controller;
    private Localiser localiser;
 
    private boolean isCreate;
    private boolean isReceiving;

    public CreateOrEditAddressAction(MultiBitController controller, Localiser localiser, boolean isCreate, boolean isReceiving) {
        this.controller = controller;
        this.localiser = localiser; 
        this.isCreate = isCreate;
        this.isReceiving = isReceiving;
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        
        if (isCreate) {
            if (isReceiving) {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_NEW_RECEIVING_ADDRESS); 
            } else {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_NEW_SENDING_ADDRESS); 
            }
        } else {
            if (isReceiving) {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_EDIT_RECEIVING_ADDRESS); 
            } else {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_EDIT_SENDING_ADDRESS); 
            }
        }
    }
    
    public String getDisplayText() {
        // TODO localise
        if (isCreate) {
            return "createNew";

        } else {
            return "edit";
        }
    }

}
