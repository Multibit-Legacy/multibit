package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views the Your Wallets screen
 */
public class YourWalletsAction extends AbstractAction {

    private static final long serialVersionUID = 1911192460523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link YourWalletsAction}.
     */
    public YourWalletsAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showYourWalletsAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showYourWalletsAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showYourWalletsAction.mnemonicKey"));
    }

    /**
     * forward to your wallets view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_YOUR_WALLETS);
    }
}