package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views the My Wallets screen
 */
public class MyWalletsAction extends AbstractAction {

    private static final long serialVersionUID = 1911192460523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link MyWalletsAction}.
     */
    public MyWalletsAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showMyWalletsAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showMyWalletsAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showMyWalletsAction.mnemonicKey"));
    }

    /**
     * forward to preferences view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_MY_WALLETS);
    }
}