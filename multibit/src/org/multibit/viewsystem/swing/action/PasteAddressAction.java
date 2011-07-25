package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} represents the swing paste address action
 */
public class PasteAddressAction extends AbstractAction {

    private static final long serialVersionUID = 114352235465057705L;

    private MultiBitController controller;
    private Localiser localiser;

    /**
     * Creates a new {@link PasteAddressAction}.
     */
    public PasteAddressAction(MultiBitController controller, Localiser localiser) {
        super(localiser.getString("pasteAddressAction.text"));
        this.controller = controller;
        this.localiser = localiser;
        
        putValue(SHORT_DESCRIPTION, localiser.getString("pasteAddressAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("pasteAddressAction.mnemonicKey"));
    }

    /**
     * delegate to generic paste address action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.PasteAddressAction genericPasteAddressAction = new org.multibit.action.PasteAddressAction(controller, localiser);
        genericPasteAddressAction.execute(null);
    }
}