package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} represents an ok action to go back to the parent view
 */
public class OkBackToParentAction extends AbstractAction {

    private static final long serialVersionUID = 191352235461234705L;

    private MultiBitController controller;

    /**
     * Creates a new {@link OkBackToParentAction}.
     */
    public OkBackToParentAction(MultiBitController controller) {
        super(controller.getLocaliser().getString("okBackToParentAction.text"));
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("okBackToParentAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("okBackToParentAction.mnemonicKey"));
    }

    /**
     * delegate to the generic OkBackToParentAction
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.OkBackToParentAction okBackToParentAction = 
            new org.multibit.action.OkBackToParentAction(controller);
        // TODO OK will get split into separate working actions
        okBackToParentAction.execute(null);
    }
}