package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.view.SendBitcoinPanel;

/**
 * an action to paste the contents of the system clipboard into the address field in the supplied formbean
 * @author jim
 *
 */
public class PasteAddressAction implements Action {
    private MultiBitController controller;
    private SendBitcoinPanel sendBitcoinPanel;
    
    public PasteAddressAction(MultiBitController controller, SendBitcoinPanel sendBitcoinPanel) {
        this.controller = controller;    
        this.sendBitcoinPanel = sendBitcoinPanel;
    }
    
    public void execute(DataProvider dataProvider) {
        TextTransfer textTransfer = new TextTransfer();
        String stringToPaste = textTransfer.getClipboardContents();
      
        // TODO parse string - if bitcoin URI then fill out other fields
        
        String label = sendBitcoinPanel.getLabelTextField().getText();
        AddressBookData addressBookData = new AddressBookData(label, stringToPaste);
        sendBitcoinPanel.setAddressBookDataByRow(addressBookData);
       
        // put it in the user preferences - will then get loaded when view form loads
        controller.getModel().setUserPreference(MultiBitModel.SEND_ADDRESS, stringToPaste);
        
        // forward back to the view currently being displayed
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);       
    }
    
    public String getDisplayText() {
        // TODO localise
        return "pasteAddress";
    }
}


