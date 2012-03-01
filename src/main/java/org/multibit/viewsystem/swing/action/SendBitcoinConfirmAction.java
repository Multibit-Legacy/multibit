/**
 * Copyright 2012 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.SendBitcoinConfirmDialog;
import org.multibit.viewsystem.swing.view.ValidationErrorDialog;

/**
 * This {@link Action} shows the send bitcoin confirm dialog or validation dialog on an attempted spend
 */
public class SendBitcoinConfirmAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link SendBitcoinConfirmAction}.
     */
    public SendBitcoinConfirmAction(MultiBitController controller, MultiBitFrame mainFrame, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("sendBitcoinConfirmAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
    }

    /**
     * show the send bitcoin confirm dialog
     */
    public void actionPerformed(ActionEvent e) {
        SendBitcoinConfirmDialog sendBitcoinConfirmDialog = null;
        ValidationErrorDialog validationErrorDialog = null;

        // copy the data into the user preferences (side effect of getData call)
        dataProvider.getData();

        String sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        String sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);

        Validator validator = new Validator(controller);
        if (validator.validate(sendAddress, sendAmount)) {
            sendBitcoinConfirmDialog = new SendBitcoinConfirmDialog(controller, mainFrame);
            sendBitcoinConfirmDialog.setVisible(true);
        } else {
            validationErrorDialog = new ValidationErrorDialog(controller, mainFrame);
            validationErrorDialog.setVisible(true);
        }
    }
}