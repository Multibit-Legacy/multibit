package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} submits the CreateOrEditAddress swing view
 */
public class CreateOrEditAddressSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private DataProvider dataProvider;
    private boolean isCreate;
    private boolean isReceiver;

    /**
     * Creates a new {@link CreateOrEditAddressSubmitAction}.
     */
    public CreateOrEditAddressSubmitAction(MultiBitController controller, DataProvider dataProvider, boolean isCreate, boolean isReceiver) {
        super(controller.getLocaliser().getString("createOrEditAddressSubmitAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;
        this.isCreate = isCreate;
        this.isReceiver = isReceiver;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createOrEditAddressSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createOrEditAddressSubmitAction.mnemonicKey"));
    }

    /**
     * delegate to generic submit action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CreateOrEditAddressSubmitAction submitAction = new org.multibit.action.CreateOrEditAddressSubmitAction(controller, isCreate, isReceiver);
        submitAction.execute(dataProvider);
    }
}