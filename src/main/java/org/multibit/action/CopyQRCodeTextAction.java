package org.multibit.action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.action.TextTransfer;

/**
 * an action to copy the bitcoin URI in the supplied formbean to the system
 * clipboard
 * 
 * @author jim
 * 
 */
public class CopyQRCodeTextAction implements Action {
    private MultiBitController controller;

    public CopyQRCodeTextAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item qrCodeTextItem = data.getItem(MultiBitModel.RECEIVE_URI_TEXT);
                String qrCodeText = "";
                
                if (qrCodeTextItem != null && qrCodeTextItem.getNewValue() != null) {
                    qrCodeText = (String) qrCodeTextItem.getNewValue();

                    // copy to clipboard
                    TextTransfer textTransfer = new TextTransfer();
                    textTransfer.setClipboardContents(qrCodeText);
                }
            }
        }
    }

    public String getDisplayText() {
        return "copyQRCodeText";
    }
}
