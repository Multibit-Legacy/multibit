package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to copy the address in the supplied formbean to the system clipboard
 * @author jim
 *
 */
public class CopyAddressAction implements Action {
    private MultiBitController controller;
    private Localiser localiser;
    
    public CopyAddressAction(MultiBitController controller, Localiser localiser) {
        this.controller = controller;
        this.localiser = localiser;     
    }
    
    public void execute(DataProvider dataProvider) {
        // TO DO - copy address from supplied formbean to system clipboard
        String addressToCopy = "TODO - get address from form";
        
        TextTransfer textTransfer = new TextTransfer();
        textTransfer.setClipboardContents(addressToCopy);
        
        // forward back to the same view
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "copyAddress";
    }
}


