package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} represents the swing copy receive address action
 */
public class CopyReceiveAddressAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopyReceiveAddressAction}.
     * @param copyIcon 
     */
    public CopyReceiveAddressAction(MultiBitController controller, DataProvider dataProvider, ImageIcon copyIcon) {
        super("", copyIcon);
        this.dataProvider = dataProvider;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyAddressAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyAddressAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy receive address text action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CopyReceiveAddressAction genericCopyReceiveAddressAction = new org.multibit.action.CopyReceiveAddressAction();
        genericCopyReceiveAddressAction.execute(dataProvider);
    }
}