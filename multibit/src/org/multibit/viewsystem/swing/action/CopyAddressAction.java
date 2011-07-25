package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} represents the swing copy address action
 */
public class CopyAddressAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopyAddressAction}.
     */
    public CopyAddressAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("copyAddressAction.text"));
        this.controller = controller;
        
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyAddressAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("copyAddressAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy address action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CopyAddressAction genericCopyAddressAction = new org.multibit.action.CopyAddressAction(controller);
        genericCopyAddressAction.execute(dataProvider);
    }
}