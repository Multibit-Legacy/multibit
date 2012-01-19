package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views Export Private Keys view
 */
public class ShowExportPrivateKeysAction extends AbstractAction {

    private static final long serialVersionUID = 1923499990523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link ShowExportPrivateKeysAction}.
     */
    public ShowExportPrivateKeysAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showExportPrivateKeysAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showExportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * forward to show import private keys view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_SHOW_EXPORT_PRIVATE_KEYS_VIEW);
    }
}