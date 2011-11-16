package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} show the merchant 'create bulk addresses view'
 */
public class ShowCreateBulkAddressesAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link ShowCreateBulkAddressesAction}.
     */
    public ShowCreateBulkAddressesAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("showCreateBulkAddressesAction.text"), icon);
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showCreateBulkAddressesAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showCreateBulkAddressesAction.mnemonicKey"));
    }

    /**
     * forward to create bulk addresses view
     */
    public void actionPerformed(ActionEvent e) {
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_BULK_ADDRESSES_VIEW);
    }
}