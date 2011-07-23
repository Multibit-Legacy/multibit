package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.Localiser;

/**
 * This {@link Action} exits the application.
 */
public class ExitAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460565057705L;

    /**
     * Creates a new {@link ExitAction}.
     */
    public ExitAction(Localiser localiser) {
        super(localiser.getString("exitAction.text"));
        putValue(SHORT_DESCRIPTION, localiser.getString("exitAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("exitAction.mnemonicKey"));
    }

    /**
     * Exits the application.
     */
    public void actionPerformed(ActionEvent e) {
        System.exit(0);
    }
}