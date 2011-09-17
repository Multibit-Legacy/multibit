package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} views the MultiBit preferences
 */
public class ShowPreferencesAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link ShowPreferencesAction}.
     */
    public ShowPreferencesAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showPreferencesAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showPreferencesAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesAction.mnemonicKey"));
    }

    /**
     * forward to preferences view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_PREFERENCES);
    }
}