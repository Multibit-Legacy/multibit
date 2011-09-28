package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
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
        //super(controller.getLocaliser().getString("pasteAddressAction.text"));
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
        TextTransfer textTransfer = new TextTransfer();
        String stringToPaste = textTransfer.getClipboardContents();
      
        // TODO parse string - if bitcoin URI then fill out other fields
        
        String label = sendBitcoinPanel.getLabelTextField().getText();
        AddressBookData addressBookData = new AddressBookData(label, stringToPaste);
        sendBitcoinPanel.setAddressBookDataByRow(addressBookData);
       
        // put it in the user preferences - will then get loaded when view form loads
        controller.getModel().setWalletPreference(MultiBitModel.SEND_ADDRESS, stringToPaste);
        
        // forward back to the view currently being displayed
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);       
    }

}