package org.multibit.viewsystem.swing.action;

import java.awt.Cursor;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} exits the application.
 */
public class ExitAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460565057705L;

    private MultiBitController controller;
    private JFrame mainFrame;
    
    /**
     * Creates a new {@link ExitAction}.
     */
    public ExitAction(MultiBitController controller, JFrame mainFrame) {
        super(controller.getLocaliser().getString("exitAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("exitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("exitAction.mnemonicKey"));
    }

    /**
     * delegate to the generic ExitAction
     */
    @SuppressWarnings("deprecation")
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.WAIT_CURSOR);
        org.multibit.action.ExitAction exitAction = new org.multibit.action.ExitAction(controller);
        exitAction.execute(null);
    }
}