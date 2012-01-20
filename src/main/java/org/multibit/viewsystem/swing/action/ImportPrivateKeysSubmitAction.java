package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} imports the private keys to the active wallet
 */
public class ImportPrivateKeysSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492087598757765L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link ImportPrivateKeysSubmitAction}.
     */
    public ImportPrivateKeysSubmitAction(MultiBitController controller, DataProvider dataProvider, ImageIcon icon) {
        super(controller.getLocaliser().getString("showImportPrivateKeysAction.text"), icon);
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showImportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showImportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * delegate to generic submit action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.ImportPrivateKeysSubmitAction submitAction = new org.multibit.action.ImportPrivateKeysSubmitAction(controller);
        submitAction.execute(dataProvider);
    }
}