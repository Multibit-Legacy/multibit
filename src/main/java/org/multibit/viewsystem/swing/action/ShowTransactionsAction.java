package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views the MultiBit transactions
 */
public class ShowTransactionsAction extends AbstractAction {

    private static final long serialVersionUID = 1923444460523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link ShowTransactionsAction}.
     */
    public ShowTransactionsAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showTransactionsAction.text"), icon);
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showTransactionsAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showTransactionsAction.mnemonicKey"));
    }

    /**
     * forward to transactions view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_TRANSACTIONS);
    }
}