package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} receives bitcoin
 */
public class ReceiveBitcoinAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitFrame mainFrame;
    private MultiBitController controller;
    private Localiser localiser;
    
    /**
     * Creates a new {@link ReceiveBitcoinAction}.
     */
    public ReceiveBitcoinAction(MultiBitController controller, 
            Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("receiveBitcoinAction.text"), icon);
        this.controller = controller;
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("receiveBitcoinAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("receiveBitcoinAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * receive bitcoins
     */
    public void actionPerformed(ActionEvent e) {  
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_RECEIVE_BITCOIN);   
    }
}