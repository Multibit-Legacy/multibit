package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} represents a cancel action to go back to the parent view
 */
public class CancelBackToParentAction extends AbstractAction {

    private static final long serialVersionUID = 191354565461234705L;

    private MultiBitController controller;
    private Localiser localiser;

    /**
     * Creates a new {@link CancelBackToParentAction}.
     */
    public CancelBackToParentAction(MultiBitController controller, Localiser localiser) {
        super(localiser.getString("cancelBackToParentAction.text"));
        this.controller = controller;
        this.localiser = localiser;

        putValue(SHORT_DESCRIPTION, localiser.getString("cancelBackToParentAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("canceBackToParentAction.mnemonicKey"));
    }

    /**
     * delegate to the generic CancelBackToParentAction
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CancelBackToParentAction cancelBackToParentAction = 
            new org.multibit.action.CancelBackToParentAction(controller, localiser);
        cancelBackToParentAction.execute(null);
    }
}