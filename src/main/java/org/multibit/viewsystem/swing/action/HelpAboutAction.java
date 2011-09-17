package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} represents the Help About action
 */
public class HelpAboutAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private MultiBitController controller;

    /**
     * Creates a new {@link HelpAboutAction}.
     */
    public HelpAboutAction(MultiBitController controller, ImageIcon icon, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("helpAboutAction.text"), icon);
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("helpAboutAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("helpAboutAction.mnemonicKey"));
    }

    /**
     * forward to help about view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_HELP_ABOUT);
    }
}