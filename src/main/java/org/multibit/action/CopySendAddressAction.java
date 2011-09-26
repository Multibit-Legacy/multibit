package org.multibit.action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.action.TextTransfer;

/**
 * an action to copy the send address in the supplied formbean to the system
 * clipboard
 * 
 * @author jim
 * 
 */
public class CopySendAddressAction implements Action {
    private MultiBitController controller;

    public CopySendAddressAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item addressItem = data.getItem(MultiBitModel.SEND_ADDRESS);
                String addressText = "";
                
                if (addressItem != null && addressItem.getNewValue() != null) {
                    addressText = (String) addressItem.getNewValue();

                    // copy to clipboard
                    TextTransfer textTransfer = new TextTransfer();
                    textTransfer.setClipboardContents(addressText);
                }
            }
        }
    }
}
