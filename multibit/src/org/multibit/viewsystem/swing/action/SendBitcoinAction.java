package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} forwards to the send bitcoin view
 */
public class SendBitcoinAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private JFrame mainFrame;
    private MultiBitController controller;
    private Localiser localiser;
    
    /**
     * Creates a new {@link SendBitcoinAction}.
     */
    public SendBitcoinAction(MultiBitController controller, Localiser localiser, ImageIcon icon, JFrame mainFrame) {
        super(localiser.getString("sendBitcoinAction.text"), icon);
        this.controller = controller;
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("sendBitcoinAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("sendBitcoinAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * delegate to generic sendBitcoinAction
     */
    public void actionPerformed(ActionEvent e) { 
        org.multibit.action.SendBitcoinAction sendBitcoinAction = new org.multibit.action.SendBitcoinAction(controller, localiser);
        sendBitcoinAction.execute(null);     
    }
}