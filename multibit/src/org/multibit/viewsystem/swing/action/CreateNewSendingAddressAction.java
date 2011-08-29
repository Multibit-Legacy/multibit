package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.view.SendBitcoinPanel;

/**
 * This {@link Action} represents an action to create a sending address
 */
public class CreateNewSendingAddressAction extends AbstractAction {

    private static final long serialVersionUID = 200111935465875405L;

    private MultiBitController controller;
    
    private SendBitcoinPanel sendBitcoinPanel;

    /**
     * Creates a new {@link CreateNewSendingAddressAction}.
     */
    public CreateNewSendingAddressAction(MultiBitController controller, SendBitcoinPanel sendBitcoinPanel) {
        super(controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.text"));
        this.controller = controller;
        this.sendBitcoinPanel = sendBitcoinPanel;

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.createSending.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("createOrEditAddressAction.createSending.mnemonicKey"));
    }

    /**
     * create new send address
     */
    public void actionPerformed(ActionEvent e) {
        
        //String addressString = "";
        controller.getModel().getAddressBook().addSendingAddress(new AddressBookData("", ""));
        
        controller.getModel().setUserPreference(MultiBitModel.SEND_ADDRESS, "");
        controller.getModel().setUserPreference(MultiBitModel.SEND_LABEL, "");

        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME); 
        
        sendBitcoinPanel.getFormPanel().requestFocusInWindow();
        sendBitcoinPanel.getLabelTextField().requestFocusInWindow();
    }
}