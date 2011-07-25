package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} represents an action to create or edit an address
 */
public class CreateOrEditAddressAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465875405L;

    private MultiBitController controller;
    private Localiser localiser;
    private boolean isCreate;
    private boolean isReceiving;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CreateOrEditAddressAction}.
     */
    public CreateOrEditAddressAction(MultiBitController controller, Localiser localiser, boolean isCreate, boolean isReceiving, DataProvider dataProvider) {
        super(isCreate ? localiser.getString("createOrEditAddressAction.createReceiving.text") :  localiser.getString("createOrEditAddressAction.editReceiving.text"));
        this.controller = controller;
        this.localiser = localiser;
        this.isCreate = isCreate;
        this.isReceiving = isReceiving;
        this.dataProvider = dataProvider;
        
        if (isCreate) {
            if (isReceiving) {
                putValue(SHORT_DESCRIPTION, localiser.getString("createOrEditAddressAction.createReceiving.tooltip"));
                putValue(MNEMONIC_KEY, localiser.getMnemonic("createOrEditAddressAction.createReceiving.mnemonicKey"));
            } else {
                putValue(SHORT_DESCRIPTION, localiser.getString("createOrEditAddressAction.createSending.tooltip"));
                putValue(MNEMONIC_KEY, localiser.getMnemonic("createOrEditAddressAction.createSending.mnemonicKey"));
            }
        } else {
            if (isReceiving) {
                putValue(SHORT_DESCRIPTION, localiser.getString("createOrEditAddressAction.editReceiving.tooltip"));
                putValue(MNEMONIC_KEY, localiser.getMnemonic("createOrEditAddressAction.editReceiving.mnemonicKey"));
            } else {
                putValue(SHORT_DESCRIPTION, localiser.getString("createOrEditAddressAction.editSending.tooltip"));
                putValue(MNEMONIC_KEY, localiser.getMnemonic("createOrEditAddressAction.editSending.mnemonicKey"));
            }
        }
    }

    /**
     * delegate to generic create or edit address action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CreateOrEditAddressAction genericCreateOrEditAddressAction = new org.multibit.action.CreateOrEditAddressAction(controller, localiser, isCreate, isReceiving);
        genericCreateOrEditAddressAction.execute(dataProvider);
    }
}