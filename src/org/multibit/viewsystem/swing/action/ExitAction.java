package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} exits the application.
 */
public class ExitAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460565057705L;

    private MultiBitController controller;
    
    /**
     * Creates a new {@link ExitAction}.
     */
    public ExitAction(MultiBitController controller) {
        super(controller.getLocaliser().getString("exitAction.text"));
        this.controller = controller;
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("exitAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("exitAction.mnemonicKey"));
    }

    /**
     * delegate to the generic ExitAction
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.ExitAction exitAction = new org.multibit.action.ExitAction(controller);
        exitAction.execute(null);
    }
}