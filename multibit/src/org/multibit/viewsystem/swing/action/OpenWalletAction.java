package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} opens a wallet from a file
 */
public class OpenWalletAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457705L;

    private MultiBitController controller;
    
    /**
     * Creates a new {@link OpenWalletAction}.
     */
    public OpenWalletAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("openWalletAction.text"), icon);
        this.controller = controller;
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("openWalletAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("openWalletAction.mnemonicKey"));
    }

    /**
     * froward to open wallet view
     */
    public void actionPerformed(ActionEvent e) { 
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_OPEN_WALLET);      
    }
}