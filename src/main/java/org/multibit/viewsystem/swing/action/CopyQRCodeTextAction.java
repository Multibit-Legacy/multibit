package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} represents the swing copy address action
 */
public class CopyQRCodeTextAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;
    
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopyQRCodeTextAction}.
     */
    public CopyQRCodeTextAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("copyQRCodeTextAction.text"));
        this.dataProvider = dataProvider;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyQRCodeTextAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyQRCodeTextAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy QRCode text action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CopyQRCodeTextAction genericQRCodeTextAction = new org.multibit.action.CopyQRCodeTextAction();
        genericQRCodeTextAction.execute(dataProvider);
    }
}