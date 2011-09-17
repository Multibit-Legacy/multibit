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
    private boolean isCreate;
    private boolean isReceiving;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CreateOrEditAddressAction}.
     */
    public CreateOrEditAddressAction(MultiBitController controller, boolean isCreate, boolean isReceiving, DataProvider dataProvider) {
        super(isCreate ? controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.text") :  controller.getLocaliser().getString("createOrEditAddressAction.editReceiving.text"));
        this.controller = controller;
        this.isCreate = isCreate;
        this.isReceiving = isReceiving;
        this.dataProvider = dataProvider;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        if (isCreate) {
            if (isReceiving) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.createReceiving.tooltip"));
                putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.createReceiving.mnemonicKey"));
            } else {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.createSending.tooltip"));
                putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.createSending.mnemonicKey"));
            }
        } else {
            if (isReceiving) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.editReceiving.tooltip"));
                putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.editReceiving.mnemonicKey"));
            } else {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressAction.editSending.tooltip"));
                putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressAction.editSending.mnemonicKey"));
            }
        }
    }

    /**
     * delegate to generic create or edit address action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CreateOrEditAddressAction genericCreateOrEditAddressAction = new org.multibit.action.CreateOrEditAddressAction(controller, isCreate, isReceiving);
        genericCreateOrEditAddressAction.execute(dataProvider);
    }
}