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
package org.multibit.viewsystem.swing.view;

import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JOptionPane;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;

import com.google.bitcoin.core.Wallet.BalanceType;

/**
 * The validation error dialog - used to tell the user their input is invalid.
 */
public class ValidationErrorDialog extends MultiBitDialog {
    private static final long serialVersionUID = 191499812345057705L;

    private static final int HEIGHT_DELTA = 150;
    private static final int WIDTH_DELTA = 160;

    private MultiBitController controller;

    /**
     * Creates a new {@link ValidationErrorDialog}.
     * 
     * @param controller
     *            The controller
     */
    public ValidationErrorDialog(MultiBitController controller, MultiBitFrame mainFrame) {
        super(mainFrame, controller.getLocaliser().getString("validationErrorView.title"));
        this.controller = controller;

        initUI();
    }

    /**
     * Initialise the validation error dialog.
     */
    private void initUI() {
        // Get the data out of the user preferences.
        String addressValue = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE);
        String amountValue = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE);

        // Invalid address.
        String addressIsInvalid = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID);
        boolean addressIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(addressIsInvalid)) {
            addressIsInvalidBoolean = true;
        }

        // Amount is missing.
        String amountIsMissing = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING);
        boolean amountIsMissingBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsMissing)) {
            amountIsMissingBoolean = true;
        }

        // Invalid amount i.e. not a number or could not parse.
        String amountIsInvalid = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID);
        boolean amountIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsInvalid)) {
            amountIsInvalidBoolean = true;
        }

        // Amount is negative or zero.
        String amountIsNegativeOrZero = controller.getModel().getActiveWalletPreference(
                MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO);
        boolean amountIsNegativeOrZeroBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsNegativeOrZero)) {
            amountIsNegativeOrZeroBoolean = true;
        }

        // Amount is more than available funds.
        String notEnoughFunds = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS);
        boolean notEnoughFundsBoolean = false;
        if (Boolean.TRUE.toString().equals(notEnoughFunds)) {
            notEnoughFundsBoolean = true;
        }

        // Get localised validation messages.
        String completeMessage = "";

        int rows = 0;
        String longestRow = "";

        if (addressIsInvalidBoolean) {
            completeMessage = controller.getLocaliser().getString("validationErrorView.addressInvalidMessage",
                    new String[] { addressValue });
            longestRow = completeMessage;
            rows++;
        }
        if (amountIsMissingBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            String textToAdd = controller.getLocaliser().getString("validationErrorView.amountIsMissingMessage");
            if (textToAdd.length() > longestRow.length()) {
                longestRow = textToAdd;
            }
            completeMessage = completeMessage + textToAdd;
            rows++;
        }
        if (amountIsInvalidBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            String textToAdd = controller.getLocaliser().getString("validationErrorView.amountInvalidMessage",
                    new String[] { amountValue });
            if (textToAdd.length() > longestRow.length()) {
                longestRow = textToAdd;
            }
            completeMessage = completeMessage + textToAdd;

            rows++;
        }
        if (amountIsNegativeOrZeroBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }

            String textToAdd = controller.getLocaliser().getString("validationErrorView.amountIsNegativeOrZeroMessage");
            if (textToAdd.length() > longestRow.length()) {
                longestRow = textToAdd;
            }
            completeMessage = completeMessage + textToAdd;

            rows++;
        }
        if (notEnoughFundsBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            if (fee == null || fee.equals("")) {
                fee = controller.getLocaliser().bitcoinValueToString(MultiBitModel.SEND_FEE_DEFAULT, false, false);
            }
            String textToAdd = controller.getLocaliser().getString("validationErrorView.notEnoughFundsMessage",
                    new String[] { amountValue, fee });
            if (controller.getModel().getActiveWallet().getBalance(BalanceType.AVAILABLE).compareTo(controller.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED)) != 0) {
                textToAdd = controller.getLocaliser().getString("validationErrorView.notEnoughFundsMessage2",
                        new String[] { amountValue, fee });
            }
            String[] lines = textToAdd.split("\n");
            for (int i = 0; i < lines.length; i++) {
                if (lines[i] != null && lines[i].length() > longestRow.length()) {
                    longestRow = lines[i];
                }
            }
            completeMessage = completeMessage + textToAdd;
            rows++;
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
        if (controller.getModel().getActiveWallet().getBalance(BalanceType.AVAILABLE).compareTo(controller.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED)) != 0) {
            options = new Object[] { okButton, availableToSpendHelpButton};
        }
        MultiBitTextArea completeMessageTextArea = new MultiBitTextArea("\n" + completeMessage + "\n", rows, 20, controller);
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