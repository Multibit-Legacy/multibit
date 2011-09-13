package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} represents the swing copy address action
 */
public class CopySendAddressAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopySendAddressAction}.
     */
    public CopySendAddressAction(MultiBitController controller, DataProvider dataProvider, ImageIcon icon) {
        super("", icon);
        this.controller = controller;
        this.dataProvider = dataProvider;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyAddressAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyAddressAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy send address text action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CopySendAddressAction genericCopySendAddressAction = new org.multibit.action.CopySendAddressAction(controller);
        genericCopySendAddressAction.execute(dataProvider);
    }
}