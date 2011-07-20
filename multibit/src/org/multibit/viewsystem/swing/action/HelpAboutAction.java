package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.ActionForward;
import org.multibit.MultiBitController;
import org.multibit.viewsystem.Localiser;
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
    public HelpAboutAction(MultiBitController controller, Localiser localiser, MultiBitFrame mainFrame) {
        super(localiser.getString("helpAboutAction.text"));
        this.controller = controller;

        putValue(SHORT_DESCRIPTION, localiser.getString("helpAboutAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("helpAboutAction.mnemonicKey"));
    }

    /**
     * forward to help about view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForward(ActionForward.FORWARD_TO_HELP_ABOUT);
    }
}