package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views Import Private Keys view
 */
public class ShowImportPrivateKeysAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460528887765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link ShowImportPrivateKeysAction}.
     */
    public ShowImportPrivateKeysAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showImportPrivateKeysAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showImportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showImportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * forward to show import private keys view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_SHOW_IMPORT_PRIVATE_KEYS_VIEW);
    }
}