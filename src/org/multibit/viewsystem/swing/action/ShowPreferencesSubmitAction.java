package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} views the BitCoinJ preferences
 */
public class ShowPreferencesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public ShowPreferencesSubmitAction(MultiBitController controller, MultiBitFrame mainFrame, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("showPreferencesSubmitAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.dataProvider = dataProvider;

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
    }

    /**
     * delegate to generic submit action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.ShowPreferencesSubmitAction submitAction = new org.multibit.action.ShowPreferencesSubmitAction(controller, mainFrame);
        submitAction.execute(dataProvider);
    }
}