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
    private Localiser localiser;
    
    /**
     * Creates a new {@link SendBitcoinConfirmAction}.
     */
    public SendBitcoinConfirmAction(MultiBitController controller, Localiser localiser) {
        super(localiser.getString("sendBitcoinConfirmAction.text"));
        this.controller = controller;
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
    }

    /**
     * delegate to generic sendBitcoinConfirmAction
     */
    public void actionPerformed(ActionEvent e) { 
        org.multibit.action.SendBitcoinConfirmAction sendBitcoinConfirmAction = new org.multibit.action.SendBitcoinConfirmAction(controller, localiser);
        sendBitcoinConfirmAction.execute(null);    
    }
}