package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} undoes the last changes made to the preferences panel
 */
public class UndoPreferencesChangesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492412423457765L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link UndoPreferencesChangesSubmitAction}.
     */
    public UndoPreferencesChangesSubmitAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("undoPreferencesChangesSubmitAction.mnemonicKey"));
    }

    /**
     * delegate to generic submit action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.UndoPreferencesChangesSubmitAction submitAction = new org.multibit.action.UndoPreferencesChangesSubmitAction(controller);
        submitAction.execute(dataProvider);
    }
}