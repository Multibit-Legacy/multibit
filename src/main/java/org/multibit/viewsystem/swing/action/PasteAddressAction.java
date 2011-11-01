package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.FileHandler;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.swing.view.SendBitcoinPanel;

/**
 * This {@link Action} represents the swing paste address action
 */
public class PasteAddressAction extends AbstractAction {

    private static final long serialVersionUID = 114352235465057705L;

    private MultiBitController controller;
    private SendBitcoinPanel sendBitcoinPanel;

    /**
     * Creates a new {@link PasteAddressAction}.
     */
    public PasteAddressAction(MultiBitController controller, SendBitcoinPanel sendBitcoinPanel, ImageIcon icon) {
        super("", icon);
        // super(controller.getLocaliser().getString("pasteAddressAction.text"));
        this.controller = controller;
        this.sendBitcoinPanel = sendBitcoinPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("pasteAddressAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("pasteAddressAction.mnemonicKey"));
    }

    /**
     * delegate to generic paste address action
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
            TextTransfer textTransfer = new TextTransfer();
            String stringToPaste = textTransfer.getClipboardContents();
            stringToPaste = WhitespaceTrimmer.trim(stringToPaste);

            // TODO parse string - if bitcoin URI then fill out other fields

            String label = sendBitcoinPanel.getLabelTextArea().getText();
            AddressBookData addressBookData = new AddressBookData(label, stringToPaste);
            sendBitcoinPanel.setAddressBookDataByRow(addressBookData);

            // put it in the user preferences - will then get loaded when view
            // form loads
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, stringToPaste);

            // forward back to the view currently being displayed
            controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
        }
    }

}