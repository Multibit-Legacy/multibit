package org.multibit.viewsystem.swing.action;

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.PerWalletModelData;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.swing.view.AbstractTradePanel;

/**
 * This {@link Action} enables you to paste a swatch into the Send Bitcoin Panel
 */
public class PasteSwatchAction extends AbstractAction {

    private static final long serialVersionUID = 193452235465057705L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link PasteSwatchAction}.
     */
    public PasteSwatchAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("pasteSwatchAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("pasteSwatchAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("pasteSwatchAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy QRCode image action
     */
    public void actionPerformed(ActionEvent e) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            // see if text was pasted
            TextTransfer textTransfer = new TextTransfer();
            String stringToPaste = textTransfer.getClipboardContents();
            if (stringToPaste != null) {
                // some text was pasted
                stringToPaste = WhitespaceTrimmer.trim(stringToPaste);

                if (dataProvider != null) {
                    AbstractTradePanel abstractTradePanel = (AbstractTradePanel) dataProvider;
                    abstractTradePanel.processDecodedString(stringToPaste, null);
                }
            }

            // see if an image was pasted
            Image image = getImageFromClipboard();
            if (image != null) {
                if (dataProvider != null) {
                    AbstractTradePanel abstractTradePanel = (AbstractTradePanel) dataProvider;
                    abstractTradePanel.processDroppedImage(image);
                }
            }

            // forward back to the view currently being displayed
            controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
        }
    }

    private Image getImageFromClipboard() {
        Transferable transferable = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
        if (transferable != null && transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
            try {
                return (Image) transferable.getTransferData(DataFlavor.imageFlavor);
            } catch (UnsupportedFlavorException e) {
                // handle this as desired
                e.printStackTrace();
            } catch (IOException e) {
                // handle this as desired
                e.printStackTrace();
            }
        }
        return null;
    }
}