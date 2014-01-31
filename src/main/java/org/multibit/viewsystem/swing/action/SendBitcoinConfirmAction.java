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

import com.google.dogecoin.core.Address;
import com.google.dogecoin.core.AddressFormatException;
import com.google.dogecoin.core.Utils;
import com.google.dogecoin.core.Wallet.SendRequest;
import com.google.dogecoin.core.WrongNetworkException;
import com.google.dogecoin.crypto.KeyCrypterException;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.dataproviders.BitcoinFormDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.SendBitcoinConfirmDialog;
import org.multibit.viewsystem.swing.view.dialogs.ValidationErrorDialog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.math.BigInteger;

/**
 * This {@link Action} shows the send dogecoin confirm dialog or validation dialog on an attempted spend.
 */
public class SendBitcoinConfirmAction extends MultiBitSubmitAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private static final Logger log = LoggerFactory.getLogger(SendBitcoinConfirmAction.class);

    private MultiBitFrame mainFrame;
    private BitcoinFormDataProvider dataProvider;
    private BitcoinController bitcoinController;

    /**
     * Creates a new {@link SendBitcoinConfirmAction}.
     */
    public SendBitcoinConfirmAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, BitcoinFormDataProvider dataProvider) {
        super(bitcoinController, "sendBitcoinConfirmAction.text", "sendBitcoinConfirmAction.tooltip", "sendBitcoinConfirmAction.mnemonicKey", ImageLoader.createImageIcon(ImageLoader.SEND_BITCOIN_ICON_FILE));
        this.mainFrame = mainFrame;
        this.dataProvider = dataProvider;
        this.bitcoinController = bitcoinController;
    }

    /**
     * Complete the transaction to work out the fee) and then show the send dogecoin confirm dialog.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }

        SendBitcoinConfirmDialog sendBitcoinConfirmDialog = null;
        ValidationErrorDialog validationErrorDialog = null;

        try {
            String sendAddress = dataProvider.getAddress();
            String sendAmount = dataProvider.getAmount();

            Validator validator = new Validator(super.bitcoinController);
            if (validator.validate(sendAddress, sendAmount)) {
                // The address and amount are valid.

                // Create a SendRequest.
                Address sendAddressObject;

                sendAddressObject = new Address(bitcoinController.getModel().getNetworkParameters(), sendAddress);
                SendRequest sendRequest = SendRequest.to(sendAddressObject, Utils.toNanoCoins(sendAmount));
                sendRequest.ensureMinRequiredFee = true;
                sendRequest.fee = BigInteger.ZERO;
                sendRequest.feePerKb = BitcoinModel.SEND_FEE_PER_KB_DEFAULT;

                // Note - Request is populated with the AES key in the SendBitcoinNowAction after the user has entered it on the SendBitcoinConfirm form.

                // Complete it (which works out the fee) but do not sign it yet.
                log.debug("Just about to complete the tx (and calculate the fee)...");
                boolean completedOk = bitcoinController.getModel().getActiveWallet().completeTx(sendRequest, false);
                log.debug("The fee after completing the transaction was " + sendRequest.fee);
                if (completedOk) {
                    // There is enough money.

                    sendBitcoinConfirmDialog = new SendBitcoinConfirmDialog(super.bitcoinController, mainFrame, sendRequest);
                    sendBitcoinConfirmDialog.setVisible(true);
                } else {
                    // There is not enough money.
                    // TODO setup validation parameters accordingly so that it displays ok.
                    validationErrorDialog = new ValidationErrorDialog(super.bitcoinController, mainFrame, sendRequest, true);
                    validationErrorDialog.setVisible(true);
                }

            } else {
                validationErrorDialog = new ValidationErrorDialog(super.bitcoinController, mainFrame, null, false);
                validationErrorDialog.setVisible(true);
            }
        } catch (WrongNetworkException e1) {
            logMessage(e1);
        } catch (AddressFormatException e1) {
            logMessage(e1);
        } catch (KeyCrypterException e1) {
            logMessage(e1);
        } catch (Exception e1) {
            logMessage(e1);
        }
    }

    private void logMessage(Exception e) {
        e.printStackTrace();
        String errorMessage = controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSendFailed");
        String detailMessage = controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new String[]{e.getClass().getCanonicalName() + " " + e.getMessage()});
        MessageManager.INSTANCE.addMessage(new Message(errorMessage + " " + detailMessage));
    }
}