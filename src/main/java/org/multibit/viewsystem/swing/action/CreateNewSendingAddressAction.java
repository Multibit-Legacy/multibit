package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;
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

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.createSending.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.createSending.mnemonicKey"));
    }

    /**
     * create new send address
     */
    public void actionPerformed(ActionEvent e) {
        WalletInfo walletInfo = controller.getModel().getActiveWalletWalletInfo();
        if (walletInfo == null) {
            walletInfo = new WalletInfo(controller.getModel().getActiveWalletFilename());
            controller.getModel().setActiveWalletInfo(walletInfo);
        }

        if (walletInfo.getSendingAddresses().size() == 0) {
            String address = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
            String label = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);

            controller.getModel().getActiveWalletWalletInfo().addSendingAddress(new AddressBookData(label, address));
        } else {
            controller.getModel().getActiveWalletWalletInfo().addSendingAddress(new AddressBookData("", ""));
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, "");
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, "");
        }
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);

        sendBitcoinPanel.getFormPanel().requestFocusInWindow();
        sendBitcoinPanel.getLabelTextField().requestFocusInWindow();
    }
}