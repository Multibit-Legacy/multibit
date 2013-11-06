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
package org.multibit.viewsystem.swing.view.dialogs;



import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet.BalanceType;
import com.google.bitcoin.core.Wallet.SendRequest;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.view.components.*;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * The validation error dialog - used to tell the user their input is invalid.
 */
public class ValidationErrorDialog extends MultiBitDialog {
    private static final long serialVersionUID = 191499812345057705L;

    private static final int HEIGHT_DELTA = 150;
    private static final int WIDTH_DELTA = 160;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    private final SendRequest sendRequest;
    private boolean insufficientFee;
    

    /**
     * Creates a new {@link ValidationErrorDialog}.
     */
    public ValidationErrorDialog(BitcoinController bitcoinController, MultiBitFrame mainFrame, SendRequest sendRequest, boolean insufficientFee) {
        super(mainFrame, bitcoinController.getLocaliser().getString("validationErrorView.title"));
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.sendRequest = sendRequest;
        this.insufficientFee = insufficientFee;

        initUI();
    }

    /**
     * Initialise the validation error dialog.
     */
    private void initUI() {
        // Get the data out of the user preferences.
        String addressValue = this.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.VALIDATION_ADDRESS_VALUE);
        String amountValue = this.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.VALIDATION_AMOUNT_VALUE);
  
        String amountPlusConversionToFiat = CurrencyConverter.INSTANCE.prettyPrint(amountValue);
        
        // Invalid address.
        String addressIsInvalid = this.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.VALIDATION_ADDRESS_IS_INVALID);
        boolean addressIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(addressIsInvalid)) {
            addressIsInvalidBoolean = true;
        }

        // Amount is missing.
        String amountIsMissing = this.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.VALIDATION_AMOUNT_IS_MISSING);
        boolean amountIsMissingBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsMissing)) {
            amountIsMissingBoolean = true;
        }

        // Invalid amount i.e. not a number or could not parse.
        String amountIsInvalid = this.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.VALIDATION_AMOUNT_IS_INVALID);
        boolean amountIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsInvalid)) {
            amountIsInvalidBoolean = true;
        }

        // Amount is negative or zero.
        String amountIsNegativeOrZero = this.bitcoinController.getModel().getActiveWalletPreference(
                BitcoinModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO);
        boolean amountIsNegativeOrZeroBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsNegativeOrZero)) {
            amountIsNegativeOrZeroBoolean = true;
        }

        // Amount is too small.
        String amountIsTooSmall = this.bitcoinController.getModel().getActiveWalletPreference(
                BitcoinModel.VALIDATION_AMOUNT_IS_TOO_SMALL);
        boolean amountIsTooSmallBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsTooSmall)) {
            amountIsTooSmallBoolean = true;
        }

        // Amount is more than available funds.
        String notEnoughFunds = this.bitcoinController.getModel().getActiveWalletPreference(BitcoinModel.VALIDATION_NOT_ENOUGH_FUNDS);
        boolean notEnoughFundsBoolean = false;
        if (Boolean.TRUE.toString().equals(notEnoughFunds) || insufficientFee) {
            notEnoughFundsBoolean = true;
        }

        // Get localised validation messages.
        StringBuilder completeMessage = new StringBuilder();

        int rows = 0;
        String longestRow = "";

        if (addressIsInvalidBoolean) {
            completeMessage.append(controller.getLocaliser().getString("validationErrorView.addressInvalidMessage",
                    new String[] { addressValue }));
            longestRow = completeMessage.toString();
            rows++;
        }
        if (amountIsMissingBoolean) {
            if (completeMessage.length()>0) {
                completeMessage.append("\n");
            }
            String textToAdd = controller.getLocaliser().getString("validationErrorView.amountIsMissingMessage");
            if (textToAdd.length() > longestRow.length()) {
                longestRow = textToAdd;
            }
            completeMessage.append(textToAdd);
            rows++;
        }
        if (amountIsInvalidBoolean) {
            if (completeMessage.length() > 0) {
                completeMessage.append("\n");
            }
            String textToAdd = controller.getLocaliser().getString("validationErrorView.amountInvalidMessage",
                    new String[] { amountValue });
            if (textToAdd.length() > longestRow.length()) {
                longestRow = textToAdd;
            }
            completeMessage.append(textToAdd);

            rows++;
        }
      if (amountIsNegativeOrZeroBoolean) {
          if (completeMessage.length()>0) {
              completeMessage.append("\n");
          }

          String textToAdd = controller.getLocaliser().getString("validationErrorView.amountIsNegativeOrZeroMessage");
          if (textToAdd.length() > longestRow.length()) {
              longestRow = textToAdd;
          }
          completeMessage.append(textToAdd);

          rows++;
      }
      if (amountIsTooSmallBoolean) {
          if (completeMessage.length()>0) {
              completeMessage.append("\n");
          }

          String textToAdd = controller.getLocaliser().getString("validationErrorView.amountIsTooSmallMessage", new String[]{Transaction.MIN_NONDUST_OUTPUT.toString()});
          if (textToAdd.length() > longestRow.length()) {
              longestRow = textToAdd;
          }
          completeMessage.append(textToAdd);

          rows++;
      }
        if (notEnoughFundsBoolean) {
            if (completeMessage.length()>0) {
                completeMessage.append("\n");
            }

            String textToAdd = controller.getLocaliser().getString("validationErrorView.notEnoughFundsMessage",
                    new String[] { amountPlusConversionToFiat});
            if (this.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.AVAILABLE).compareTo(this.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED)) != 0) {
                textToAdd = controller.getLocaliser().getString("validationErrorView.notEnoughFundsMessage2",
                        new String[] { amountPlusConversionToFiat});
            }
            // There is an extra "BTC." in the translations - remove and add a return.
            textToAdd = textToAdd.replaceAll("BTC\\.", "\\.");
             
            String[] lines = textToAdd.split("\\n");
            for (int i = 0; i < lines.length; i++) {
                if (lines[i] != null && lines[i].length() > longestRow.length()) {
                    longestRow = lines[i];
                }
                if (lines[i] != null && lines[i].length() > 0) {
                    if (i == 0) {
                        completeMessage.append(lines[i]);
                    } else {
                        completeMessage.append("\n").append(lines[i]);
                    }
                    rows++;
                }
            }
        }

        // Spacer row at top and bottom.
        rows = rows + 2;
        
        // Tell user validation messages.
        Action availableToSpendHelpAction = new HelpContextAction(controller, null, "validationErrorView.moreHelp",
                "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText", HelpContentsPanel.HELP_AVAILABLE_TO_SPEND_URL);
        HelpButton availableToSpendHelpButton = new HelpButton(availableToSpendHelpAction, controller, true);
        final ValidationErrorDialog finalValidationErrorDialog = this;
        availableToSpendHelpButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                finalValidationErrorDialog.setVisible(false);
                
            }});

        OkBackToParentAction okAction = new OkBackToParentAction(controller, this);
        MultiBitButton okButton = new MultiBitButton(okAction, controller);
        okButton.setOpaque(true);
        okButton.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        Object[] options = {okButton};
        if (this.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.AVAILABLE).compareTo(this.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED)) != 0) {
            options = new Object[] { okButton, availableToSpendHelpButton};
        }
        MultiBitTextArea completeMessageTextArea = new MultiBitTextArea("\n" + completeMessage.toString() + "\n", rows, 20, controller);
        completeMessageTextArea.setOpaque(false);
        completeMessageTextArea.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        completeMessageTextArea.setEditable(false);
        completeMessageTextArea.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        completeMessageTextArea.setBorder(BorderFactory.createEmptyBorder(0, 8, 0, 8));

        JOptionPane optionPane = new JOptionPane(completeMessageTextArea, JOptionPane.ERROR_MESSAGE, JOptionPane.DEFAULT_OPTION,
                ImageLoader.createImageIcon(ImageLoader.EXCLAMATION_MARK_ICON_FILE), options, options[0]);

        add(optionPane);
        optionPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        optionPane.setOpaque(true);
        FontMetrics fontMetrics = optionPane.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());

        int minimumHeight = fontMetrics.getHeight() * rows + HEIGHT_DELTA;
        //log.debug("longest row = '" + longestRow + "'");
        int minimumWidth = fontMetrics.stringWidth(longestRow) + WIDTH_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);
    }
}