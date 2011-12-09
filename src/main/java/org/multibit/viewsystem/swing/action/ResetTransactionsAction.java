package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views the reset transactions panel
 */
public class ResetTransactionsAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link ResetTransactionsAction}.
     */
    public ResetTransactionsAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("resetTransactionsAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("resetTransactionsAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("resetTransactionsAction.mnemonicKey"));
    }

    /**
     * forward to reset transactions view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_RESET_TRANSACTIONS_VIEW);
    }
}