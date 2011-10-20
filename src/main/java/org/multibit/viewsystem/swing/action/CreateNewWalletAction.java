package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} saves a wallet to a new file, backing up any existing
 * file with the same name
 */
public class CreateNewWalletAction extends AbstractAction {

    private static final long serialVersionUID = 191352210565057705L;

    private MultiBitController controller;
    
    /**
     * Creates a new {@link CreateNewWalletAction}.
     */
    public CreateNewWalletAction(MultiBitController controller, ImageIcon icon, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("createNewWalletAction.text"), icon);
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createNewWalletAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createNewWalletAction.mnemonicKey"));
   }

    /**
     * Create new wallet. Note - old files are always backed up to avoid
     * bitcoin loss
     */
    public void actionPerformed(ActionEvent e) {
        // forward to the create new wallet view
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_NEW_WALLET); 
    }
}