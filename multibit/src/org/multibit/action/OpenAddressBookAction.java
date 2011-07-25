package org.multibit.action;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to open the address book
 * @author jim
 *
 */
public class OpenAddressBookAction implements Action {

    private MultiBitController controller;
    private Localiser localiser;
    private boolean isChild;   // true = child, false = sibling
    private boolean isReceiving;
    
    public OpenAddressBookAction(MultiBitController controller, Localiser localiser, boolean isChild, 
            boolean isReceiving) {
        this.controller = controller;
        this.localiser = localiser; 
        this.isChild = isChild;
        this.isReceiving = isReceiving;
    }
    
    public void execute(DataProvider dataProvider) {
        // no changes required to model
        if (isChild) {
            if (isReceiving) {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_ADDRESS_BOOK_RECEIVING);       
            } else {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_ADDRESS_BOOK_SENDING);       
            }
        } else {
            if (isReceiving) {
                controller.setActionForwardToSibling(ActionForward.FORWARD_TO_ADDRESS_BOOK_RECEIVING);       
            } else {
                controller.setActionForwardToSibling(ActionForward.FORWARD_TO_ADDRESS_BOOK_SENDING);       
            }
        }
    }
    
    public String getDisplayText() {
        // TODO localise
        if (isChild) {
            return "addressBook";
        } else {
            if (isReceiving) {
                return "receivingAddressBook";   
            } else {
                return "sendingAddressBook";
            }
        }
    }
}
