package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} represents the open address book action
 */
public class OpenAddressBookAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465123405L;

    private MultiBitController controller;
    private Localiser localiser;
    private boolean isChild; // true = child, false = sibling
    private boolean isReceiving;

    /**
     * Creates a new {@link OpenAddressBookAction} with icon
     */
    public OpenAddressBookAction(MultiBitController controller, Localiser localiser,
            ImageIcon icon, boolean isChild, boolean isReceiving) {
        super(localiser.getString("openAddressBookAction.receiving.text"), icon);
        this.controller = controller;
        this.localiser = localiser;
        this.isChild = isChild;
        this.isReceiving = isReceiving;

        if (isReceiving) {
            putValue(SHORT_DESCRIPTION,
                    localiser.getString("openAddressBookAction.receiving.tooltip"));
            putValue(MNEMONIC_KEY,
                    localiser.getMnemonic("openAddressBookAction.receiving.mnemonicKey"));
        } else {
            putValue(SHORT_DESCRIPTION, localiser.getString("openAddressBookAction.sending.tooltip"));
            putValue(MNEMONIC_KEY,
                    localiser.getMnemonic("openAddressBookAction.sending.mnemonicKey"));
        }

    }

    /**
     * Creates a new {@link OpenAddressBookAction}.
     */
    public OpenAddressBookAction(MultiBitController controller, Localiser localiser,
            boolean isChild, boolean isReceiving) {
        this(controller, localiser, null, isChild, isReceiving);
    }

    /**
     * delegate to generic open address action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.OpenAddressBookAction genericOpenAddressBookAction = new org.multibit.action.OpenAddressBookAction(
                controller, localiser, isChild, isReceiving);
        genericOpenAddressBookAction.execute(null);
    }
}