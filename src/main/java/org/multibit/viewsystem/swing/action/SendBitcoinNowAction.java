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
import java.io.IOException;
import java.math.BigInteger;
import java.nio.CharBuffer;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;

import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.bitcoin.PerWalletModelData;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.bitcoin.WalletInfo;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.crypto.KeyCrypterException;

/**
 * This {@link Action} actually spends bitcoin.
 */
public class SendBitcoinNowAction extends AbstractAction implements WalletBusyListener {

    public Logger log = LoggerFactory.getLogger(SendBitcoinNowAction.class.getName());

    private static final long serialVersionUID = 1913592460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private SendBitcoinConfirmPanel sendBitcoinConfirmPanel;
    private JPasswordField walletPasswordField;

    private final static int MAX_LENGTH_OF_ERROR_MESSAGE = 70;
    
    /**
     * Boolean to indicate that the test parameters should be used for "sending".
     */
    private boolean useTestParameters = false;
    
    /**
     * Boolean to indicate that the "send was successful" or not (when useTestParameters = true). 
     */
    private boolean sayTestSendWasSuccessful = false;
      
    private Transaction transaction;
    

    /**
     * Creates a new {@link SendBitcoinNowAction}.
     */
    public SendBitcoinNowAction(MultiBitFrame mainFrame, BitcoinController bitcoinController,
            SendBitcoinConfirmPanel sendBitcoinConfirmPanel, JPasswordField walletPasswordField, ImageIcon icon) {
        super(bitcoinController.getLocaliser().getString("sendBitcoinConfirmAction.text"), icon);
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        this.sendBitcoinConfirmPanel = sendBitcoinConfirmPanel;
        this.walletPasswordField = walletPasswordField;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("sendBitcoinConfirmAction.mnemonicKey"));
        
        // This action is a WalletBusyListener.
        this.bitcoinController.registerWalletBusyListener(this);
        walletBusyChange(this.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
    }

    /**
     * Actually send the bitcoin.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        sendBitcoinConfirmPanel.setMessageText(" ", " ");

        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = this.bitcoinController.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = this.bitcoinController.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // Set on the perWalletModelData that files have changed and fire data changed.
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            this.bitcoinController.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            // Put sending message and remove the send button.
            sendBitcoinConfirmPanel.setMessageText(controller.getLocaliser().getString("sendBitcoinNowAction.sendingBitcoin"), " ");

            // Get the data out of the wallet preferences.
            String sendAddress = this.bitcoinController.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
            String sendLabel = this.bitcoinController.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
            String sendAmount = this.bitcoinController.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);
            String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            BigInteger fee;
            if (sendFeeString == null || sendFeeString.equals("")) {
                fee = MultiBitModel.SEND_FEE_DEFAULT;
            } else {
                fee = Utils.toNanoCoins(sendFeeString);
            }

            if (sendLabel != null && !sendLabel.equals("")) {
                WalletInfo addressBook = perWalletModelData.getWalletInfo();
                addressBook.addSendingAddress(new AddressBookData(sendLabel, sendAddress));
            }
            
            char[] walletPassword = walletPasswordField.getPassword();
 
            if (this.bitcoinController.getModel().getActiveWallet() != null
                    && this.bitcoinController.getModel().getActiveWallet().getEncryptionType() != EncryptionType.UNENCRYPTED) {
                // Encrypted wallet.
                if (walletPassword == null || walletPassword.length == 0) {
                    // User needs to enter password.
                    sendBitcoinConfirmPanel.setMessageText(
                            controller.getLocaliser().getString("showExportPrivateKeysAction.youMustEnterTheWalletPassword"), "");
                    return;
                }

                try {
                    if (!this.bitcoinController.getModel().getActiveWallet().checkPassword(CharBuffer.wrap(walletPassword))) {
                        // The password supplied is incorrect.
                        sendBitcoinConfirmPanel.setMessageText(
                                controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.passwordIsIncorrect"),
                                "");
                        return;
                    }
                } catch (KeyCrypterException kce) {
                    log.debug(kce.getClass().getCanonicalName() + " " + kce.getMessage());
                    // The password supplied is probably incorrect.
                    sendBitcoinConfirmPanel.setMessageText(
                            controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.passwordIsIncorrect"), "");
                    return;
                }
            }
            
            // Double check wallet is not busy then declare that the active wallet is busy with the task
            if (!perWalletModelData.isBusy()) {
                perWalletModelData.setBusy(true);
                perWalletModelData.setBusyTaskVerbKey("sendBitcoinNowAction.sendingBitcoin");

                sendBitcoinConfirmPanel.setMessageText(controller.getLocaliser().getString("sendBitcoinNowAction.sendingBitcoin"), " ");
                
                this.bitcoinController.fireWalletBusyChange(true);

                performSend(perWalletModelData, sendAddress, sendAmount, fee, CharBuffer.wrap(walletPassword));
            }
        }
    }

    /**
     * Send the transaction directly.
     */
    private void performSend(PerWalletModelData perWalletModelData, String sendAddress, String sendAmount, BigInteger fee, CharSequence walletPassword) {
        String message = null;
        
        boolean sendWasSuccessful = Boolean.FALSE;
        try {            
            log.debug("Sending from wallet " + perWalletModelData.getWalletFilename() + ", amount = " + sendAmount + ", fee = "
                    + fee + " to address = " + sendAddress);
            
            if (useTestParameters) {
                log.debug("Using test parameters - not really sending");
                if (sayTestSendWasSuccessful) {
                    sendWasSuccessful = Boolean.TRUE;
                    log.debug("Using test parameters - saying send was successful");  
                } else {
                    message = "test - send failed";
                    log.debug("Using test parameters - saying send failed");  
                }
            } else {
                transaction = this.bitcoinController.getMultiBitService().sendCoins(perWalletModelData, sendAddress, sendAmount, fee, walletPassword);
                if (transaction == null) {
                    // a null transaction returned indicates there was not
                    // enough money (in spite of our validation)
                    message = controller.getLocaliser().getString("sendBitcoinNowAction.thereWereInsufficientFundsForTheSend");
                    log.error(message);
                } else {
                    sendWasSuccessful = Boolean.TRUE;
                    log.debug("Sent transaction was:\n" + transaction.toString());
               }
            }
        } catch (KeyCrypterException e) {
            log.error(e.getMessage(), e);
            message = e.getMessage();
        } catch (WalletSaveException e) {
            log.error(e.getMessage(), e);
            message = e.getMessage();
        } catch (IOException e) {
            log.error(e.getMessage(), e);
            message = e.getMessage();
        } catch (AddressFormatException e) {
            log.error(e.getMessage(), e);
            message = e.getMessage();
        } catch (Exception e) {
            // Really trying to catch anything that goes wrong with the send bitcoin.
            log.error(e.getMessage(), e);
            message = e.getMessage();
        } finally {
            // Save the wallet.

            try {
                this.bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelData, false);
            } catch (WalletSaveException e) {
                log.error(e.getMessage(), e);
                message = e.getMessage();
            }
            
            if (sendWasSuccessful) {
                String successMessage = controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSentOk");
                if (sendBitcoinConfirmPanel != null && (sendBitcoinConfirmPanel.isVisible() || useTestParameters)) {
                    sendBitcoinConfirmPanel.setMessageText(
                            controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSentOk"));
                    sendBitcoinConfirmPanel.showOkButton();
                    sendBitcoinConfirmPanel.clearPassword();
                } else {
                    MessageManager.INSTANCE.addMessage(new Message(successMessage));
                }
            } else {
                log.error(message);

                if (message != null && message.length() > MAX_LENGTH_OF_ERROR_MESSAGE) {
                    message = message.substring(0, MAX_LENGTH_OF_ERROR_MESSAGE) + "...";
                }

                String errorMessage = controller.getLocaliser().getString("sendBitcoinNowAction.bitcoinSendFailed");
                if (sendBitcoinConfirmPanel != null  && (sendBitcoinConfirmPanel.isVisible() || useTestParameters)) {
                    sendBitcoinConfirmPanel.setMessageText(errorMessage, message);
                } else {
                    MessageManager.INSTANCE.addMessage(new Message(errorMessage + " " + message));
                }
            }
            
            // Declare that wallet is no longer busy with the task.
            perWalletModelData.setBusyTaskKey(null);
            perWalletModelData.setBusy(false);
        this.bitcoinController.fireWalletBusyChange(false);                   

            log.debug("firing fireRecreateAllViews...");
            controller.fireRecreateAllViews(false);
            log.debug("firing fireRecreateAllViews...done");
        }
    }
    
    public Transaction getTransaction() {
        return transaction;
    }
    
    void setTestParameters(boolean useTestParameters, boolean sayTestSendWasSuccessful) {
        this.useTestParameters = useTestParameters;
        this.sayTestSendWasSuccessful = sayTestSendWasSuccessful;
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
        // Update the enable status of the action to match the wallet busy status.
        if (this.bitcoinController.getModel().getActivePerWalletModelData().isBusy()) {
            // Wallet is busy with another operation that may change the private keys - Action is disabled.
            putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("multiBitSubmitAction.walletIsBusy", 
                    new Object[]{controller.getLocaliser().getString(this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey())}));
            setEnabled(false);           
        } else {
            // Enable unless wallet has been modified by another process.
            if (!this.bitcoinController.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip"));
                setEnabled(true);
            }
        }
    }
}