package org.multibit.viewsystem.swing.action;

import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JLabel;
import javax.swing.TransferHandler;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

/**
 * This {@link Action} represents the swing copy QRCode action
 */
public class CopyQRCodeImageAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopyQRCodeImageAction}.
     */
    public CopyQRCodeImageAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("copyQRCodeImageAction.text"));
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyQRCodeImageAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyQRCodeImageAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy QRCode image action
     */
    public void actionPerformed(ActionEvent e) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                boolean isReceive = false;

                Item isReceiveItem = data.getItem(MultiBitModel.IS_RECEIVE_BITCOIN);
                if (isReceiveItem != null && Boolean.TRUE.toString().equals(isReceiveItem.getNewValue())) {
                    isReceive = true;
                }

                Item qrCodeImageItem = null;
                if (isReceive) {
                    qrCodeImageItem = data.getItem(MultiBitModel.RECEIVE_URI_IMAGE);
                } else {
                    qrCodeImageItem = data.getItem(MultiBitModel.SEND_URI_IMAGE);
                }
                
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
}