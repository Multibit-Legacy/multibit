package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * an action to process the submit of the Reset Transactions view
 * 
 * @author jim
 * 
 */
public class ResetTransactionsSubmitAction implements Action {
    private MultiBitController controller;

    public ResetTransactionsSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // check to see if another process has changed the active wallet
        
        // work out the earliest transaction date and save it to the wallet
        
        // remove the transactions from the wallet
        
        // navigate backwards in the blockchain to work out how far back in time to go
        
        // switch off the PeerGroup ?
        
        // save the wallet (with transactions removed) and the truncated blockchain
        
        // navigate back tot he reset transactions page, telling user to restart MultiBit
 
        // return to parent view
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
    }
}
