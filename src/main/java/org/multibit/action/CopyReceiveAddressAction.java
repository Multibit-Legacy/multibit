package org.multibit.action;

import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.action.TextTransfer;

/**
 * an action to copy the receive address in the supplied formbean to the system
 * clipboard
 * 
 * @author jim
 * 
 */
public class CopyReceiveAddressAction implements Action {

    public CopyReceiveAddressAction() {
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item addressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
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
