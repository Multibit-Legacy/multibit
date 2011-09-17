package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} forwards to the send bitcoin confirm view
 */
public class SendBitcoinConfirmAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    private DataProvider dataProvider;
    
    /**
     * Creates a new {@link SendBitcoinConfirmAction}.
     */
    public SendBitcoinConfirmAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("sendBitcoinConfirmAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
    }

    /**
     * delegate to generic sendBitcoinConfirmAction
     */
    public void actionPerformed(ActionEvent e) { 
        org.multibit.action.SendBitcoinConfirmAction sendBitcoinConfirmAction = new org.multibit.action.SendBitcoinConfirmAction(controller);
        sendBitcoinConfirmAction.execute(dataProvider);    
    }
}