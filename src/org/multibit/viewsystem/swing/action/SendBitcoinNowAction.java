package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.SendBitcoinConfirmView;

/**
 * This {@link Action} forwards to the send bitcoin now action
 */
public class SendBitcoinNowAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitFrame mainFrame;
    private MultiBitController controller;
    private DataProvider dataProvider;
    
    /**
     * Creates a new {@link SendBitcoinNowAction}.
     */
    public SendBitcoinNowAction(MultiBitFrame mainFrame, MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("sendBitcoinConfirmAction.text"));
        this.mainFrame = mainFrame;
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
    }

    /**
     * delegate to generic sendBitcoinNowAction
     */
    public void actionPerformed(ActionEvent e) { 
        org.multibit.action.SendBitcoinNowAction sendBitcoinNowAction = new org.multibit.action.SendBitcoinNowAction(controller);
        sendBitcoinNowAction.execute(dataProvider);   
        
        mainFrame.fireDataChanged();
        
        // put confirmation on the view
        SendBitcoinConfirmView sendBitcoinConfirmView = (SendBitcoinConfirmView)dataProvider;
        

        String sendWasSuccessfulText = controller.getModel().getUserPreference(MultiBitModel.SEND_WAS_SUCCESSFUL);
        String errorMessage =controller.getModel().getUserPreference(MultiBitModel.SEND_ERROR_MESSAGE);
        
        if (Boolean.TRUE.toString().equals(sendWasSuccessfulText)) {
            sendBitcoinConfirmView.setSendConfirmText(controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSentOk"), "");
        } else {
            sendBitcoinConfirmView.setSendConfirmText(controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSendFailed"), errorMessage);
        }
    }
}