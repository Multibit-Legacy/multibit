package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} represents the swing copy QRCode action
 */
public class CopyQRCodeImageAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopyQRCodeImageAction}.
     */
    public CopyQRCodeImageAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("copyQRCodeImageAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;
        
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyQRCodeImageAction.tooltip"));
        putValue(MNEMONIC_KEY, controller.getLocaliser().getMnemonic("copyQRCodeImagAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy QRCode image action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CopyQRCodeImageAction genericQRCodeImageAction = new org.multibit.action.CopyQRCodeImageAction(controller);
        genericQRCodeImageAction.execute(dataProvider);
    }
}