package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} forwards to the send bitcoin view
 */
public class SendBitcoinAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    
    /**
     * Creates a new {@link SendBitcoinAction}.
     */
    public SendBitcoinAction(MultiBitController controller, ImageIcon icon, JFrame mainFrame) {
        super(controller.getLocaliser().getString("sendBitcoinAction.text"), icon);
        this.controller = controller;
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("sendBitcoinAction.mnemonicKey"));
    }

    /**
     * delegate to generic sendBitcoinAction
     */
    public void actionPerformed(ActionEvent e) { 
        org.multibit.action.SendBitcoinAction sendBitcoinAction = new org.multibit.action.SendBitcoinAction(controller);
        sendBitcoinAction.execute(null);     
    }
}