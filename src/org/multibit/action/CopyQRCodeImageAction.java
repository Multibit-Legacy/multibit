package org.multibit.action;

import java.awt.datatransfer.Clipboard;

import javax.swing.JLabel;
import javax.swing.TransferHandler;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

/**
 * an action to copy the QRCode in the supplied formbean to the system
 * clipboard
 * 
 * @author jim
 * 
 */
public class CopyQRCodeImageAction implements Action {
    private MultiBitController controller;

    public CopyQRCodeImageAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item qrCodeImageItem = data.getItem(MultiBitModel.RECEIVE_URI_IMAGE);
               
                if (qrCodeImageItem != null && qrCodeImageItem.getNewValue() != null) {

                    JLabel qrCodeLabel = (JLabel) qrCodeImageItem.getNewValue();
                    final Clipboard clipboard = qrCodeLabel.getTopLevelAncestor().getToolkit().getSystemClipboard();

                    // copy to clipboard
                    TransferHandler handler = qrCodeLabel.getTransferHandler();
                    handler.exportToClipboard(qrCodeLabel, clipboard, TransferHandler.COPY);
                }
            }
        }
    }

    public String getDisplayText() {
        return "copyQRCodeImage";
    }
}
