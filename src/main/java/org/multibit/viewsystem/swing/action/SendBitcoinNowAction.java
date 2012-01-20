/**
 * Copyright 2011 multibit.org
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
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.SendBitcoinConfirmView;

/**
 * This {@link Action} forwards to the send bitcoin now action
 */
public class SendBitcoinNowAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link SendBitcoinNowAction}.
     */
    public SendBitcoinNowAction(MultiBitFrame mainFrame, MultiBitController controller, DataProvider dataProvider, ImageIcon icon) {
        super(controller.getLocaliser().getString("sendBitcoinConfirmAction.text"), icon);
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
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);

            SendBitcoinConfirmView sendBitcoinConfirmView = (SendBitcoinConfirmView) dataProvider;
            sendBitcoinConfirmView.setSendConfirmText(
                    controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSendFailed"), controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip"));
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {

            org.multibit.action.SendBitcoinNowAction sendBitcoinNowAction = new org.multibit.action.SendBitcoinNowAction(
                    controller);
            sendBitcoinNowAction.execute(dataProvider);

            controller.fireDataChanged();

            // put confirmation on the view
            SendBitcoinConfirmView sendBitcoinConfirmView = (SendBitcoinConfirmView) dataProvider;

            String sendWasSuccessfulText = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_WAS_SUCCESSFUL);
            String errorMessage = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ERROR_MESSAGE);

            if (Boolean.TRUE.toString().equals(sendWasSuccessfulText)) {
                sendBitcoinConfirmView.setSendConfirmText(
                        controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSentOk"), "");
            } else {
                sendBitcoinConfirmView.setSendConfirmText(
                        controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSendFailed"), errorMessage);
            }
        }
    }
}