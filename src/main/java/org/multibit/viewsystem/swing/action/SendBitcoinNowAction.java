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
import java.io.IOException;
import java.math.BigInteger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.SendBitcoinConfirmDialog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Utils;

/**
 * This {@link Action} forwards to the send bitcoin now action
 */
public class SendBitcoinNowAction extends AbstractAction {

    public Logger log = LoggerFactory.getLogger(SendBitcoinNowAction.class.getName());

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    private SendBitcoinConfirmDialog sendBitcoinConfirmView;

    private final static int MAX_LENGTH_OF_ERROR_MESSAGE = 70;

    /**
     * Creates a new {@link SendBitcoinNowAction}.
     */
    public SendBitcoinNowAction(MultiBitFrame mainFrame, MultiBitController controller,
            SendBitcoinConfirmDialog sendBitcoinConfirmView, ImageIcon icon) {
        super(controller.getLocaliser().getString("sendBitcoinConfirmAction.text"), icon);
        this.controller = controller;
        this.sendBitcoinConfirmView = sendBitcoinConfirmView;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
    }

    /**
     * actually send the bitcoin
     */
    public void actionPerformed(ActionEvent event) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            // get the data out of the wallet preferences
            String sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
            String sendLabel = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
            String sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);
            String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            BigInteger fee;
            if (sendFeeString == null || sendFeeString.equals("")) {
                fee = MultiBitModel.SEND_MINIMUM_FEE;
            } else {
                fee = Utils.toNanoCoins(sendFeeString);
            }

            if (sendLabel != null && !sendLabel.equals("")) {
                WalletInfo addressBook = perWalletModelData.getWalletInfo();
                addressBook.addSendingAddress(new AddressBookData(sendLabel, sendAddress));
            }

            boolean sendWasSuccessful = false;
            String errorMessage = " ";
            try {
                Transaction transaction = controller.sendCoins(perWalletModelData, sendAddress, sendAmount, fee);
                if (transaction == null) {
                    // a null transaction returned indicates there was not
                    // enough money (in spite of our validation)
                    errorMessage = controller.getLocaliser().getString("sendBitcoinNowAction.thereWereInsufficientFundsForTheSend");
                    log.error(errorMessage);
                } else {
                    sendWasSuccessful = true;
                    log.debug("Sent transaction was:\n" + transaction.toString());
                }

                // save the wallet
                controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
            } catch (IOException e) {
                log.error(e.getMessage(), e);
                errorMessage = e.getMessage();
            } catch (AddressFormatException e) {
                log.error(e.getMessage(), e);
                errorMessage = e.getMessage();
            } catch (Throwable t) {
                // really trying to catch anything that goes wrong with the send bitcoin
                log.error(t.getMessage(), t);
                errorMessage = t.getMessage();
            }

            // // for testing
            // sendWasSuccessful = false;
            // errorMessage = "snuibbnfjhsbfjlsfbjslfbnsjkfb";

            if (errorMessage != null && errorMessage.length() > MAX_LENGTH_OF_ERROR_MESSAGE) {
                errorMessage = errorMessage.substring(0, MAX_LENGTH_OF_ERROR_MESSAGE) + "...";
            }

            if (sendWasSuccessful) {
                sendBitcoinConfirmView.setSendConfirmText(
                        controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSentOk"), "");
            } else {
                sendBitcoinConfirmView.setSendConfirmText(
                        controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSendFailed"), errorMessage);
            }
        }
    }
}