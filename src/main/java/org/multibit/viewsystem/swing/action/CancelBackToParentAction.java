package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} represents a cancel action to go back to the parent view
 */
public class CancelBackToParentAction extends AbstractAction {

    private static final long serialVersionUID = 191354565461234705L;

    private MultiBitController controller;

    /**
     * Creates a new {@link CancelBackToParentAction}.
     */
    public CancelBackToParentAction(MultiBitController controller) {
        this(controller, null);
    }

    /**
     * Creates a new {@link CancelBackToParentAction}.
     */
    public CancelBackToParentAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("cancelBackToParentAction.text"), icon);
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("cancelBackToParentAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("canceBackToParentAction.mnemonicKey"));
    }

    /**
     * delegate to the generic CancelBackToParentAction
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CancelBackToParentAction cancelBackToParentAction = 
            new org.multibit.action.CancelBackToParentAction(controller);
        cancelBackToParentAction.execute(null);
    }
}