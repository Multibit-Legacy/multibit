package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} exports the active wallets private keys
 */
public class ExportPrivateKeysSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460598757765L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link ExportPrivateKeysSubmitAction}.
     */
    public ExportPrivateKeysSubmitAction(MultiBitController controller, DataProvider dataProvider, ImageIcon icon) {
        super(controller.getLocaliser().getString("showExportPrivateKeysAction.text"), icon);
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showExportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * delegate to generic submit action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.ExportPrivateKeysSubmitAction submitAction = new org.multibit.action.ExportPrivateKeysSubmitAction(controller);
        submitAction.execute(dataProvider);
    }
}