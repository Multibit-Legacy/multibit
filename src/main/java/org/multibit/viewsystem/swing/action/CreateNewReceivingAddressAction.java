package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;
import org.multibit.viewsystem.swing.view.ReceiveBitcoinPanel;

import com.google.bitcoin.core.ECKey;

/**
 * This {@link Action} represents an action to create a receiving address
 */
public class CreateNewReceivingAddressAction extends AbstractAction {

    private static final long serialVersionUID = 200152235465875405L;

    private MultiBitController controller;
    
    private ReceiveBitcoinPanel receiveBitcoinPanel;

    /**
     * Creates a new {@link CreateNewReceivingAddressAction}.
     */
    public CreateNewReceivingAddressAction(MultiBitController controller, ReceiveBitcoinPanel receiveBitcoinPanel) {
        super(controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.text"));
        this.controller = controller;
        this.receiveBitcoinPanel = receiveBitcoinPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.createReceiving.mnemonicKey"));
    }

    /**
     * create new receiving address
     */
    public void actionPerformed(ActionEvent e) {
        ECKey newKey = new ECKey();
        controller.getModel().getActiveWallet().keychain.add(newKey);
        
        String addressString = newKey.toAddress(controller.getMultiBitService().getNetworkParameters()).toString();
        WalletInfo walletInfo = controller.getModel().getActiveWalletWalletInfo();
        if (walletInfo == null) {
            walletInfo = new WalletInfo(controller.getModel().getActiveWalletFilename());
            controller.getModel().setActiveWalletInfo(walletInfo);
        }
        controller.getModel().getActiveWalletWalletInfo().addReceivingAddress(new AddressBookData("", addressString), false);      
        controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS, addressString);
        controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL, "");

        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME); 
        
        receiveBitcoinPanel.getFormPanel().requestFocusInWindow();
        receiveBitcoinPanel.getLabelTextField().requestFocusInWindow();
    }
}