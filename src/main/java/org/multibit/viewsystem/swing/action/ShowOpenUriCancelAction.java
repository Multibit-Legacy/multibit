package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

/**
 * This {@link Action} represents a cancel action to go back to the parent view for the open uri command
 */
public class ShowOpenUriCancelAction extends AbstractAction {

    private static final long serialVersionUID = 191354561231234705L;

    private MultiBitController controller;
    
    private DataProvider dataProvider;

    /**
     * Creates a new {@link ShowOpenUriCancelAction}.
     */
    public ShowOpenUriCancelAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("showOpenUriView.noText"));
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("cancelBackToParentAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("canceBackToParentAction.mnemonicKey"));
    }

    /**
     * delegate to the generic CancelBackToParentAction
     */
    public void actionPerformed(ActionEvent e) {
        
        Item showDialogItem = dataProvider.getData().getItem(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        
        if (showDialogItem != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG, (String)showDialogItem.getNewValue());                
        }

        // we do not want to use the uri as the user clicked yes
        controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI, "false");   

        org.multibit.action.CancelBackToParentAction cancelBackToParentAction = 
            new org.multibit.action.CancelBackToParentAction(controller);
        cancelBackToParentAction.execute(null);
    }
}