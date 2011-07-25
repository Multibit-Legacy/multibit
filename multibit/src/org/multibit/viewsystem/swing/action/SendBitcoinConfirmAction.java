package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} forwards to the send bitcoin confirm view
 */
public class SendBitcoinConfirmAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    
    /**
     * Creates a new {@link SendBitcoinConfirmAction}.
     */
    public SendBitcoinConfirmAction(MultiBitController controller) {
        super(controller.getLocaliser().getString("sendBitcoinConfirmAction.text"));
        this.controller = controller;
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
    }

    /**
     * delegate to generic sendBitcoinConfirmAction
     */
    public void actionPerformed(ActionEvent e) { 
        org.multibit.action.SendBitcoinConfirmAction sendBitcoinConfirmAction = new org.multibit.action.SendBitcoinConfirmAction(controller);
        sendBitcoinConfirmAction.execute(null);    
    }
}