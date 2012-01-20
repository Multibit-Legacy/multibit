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

import javax.swing.Action;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The validation error view - used to tell the user their input is invalid
 */
public class ValidationErrorView implements View {

    private static final Logger log = LoggerFactory.getLogger(ValidationErrorView.class);

    private static final long serialVersionUID = 191499812345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private JDialog messageDialog;

    /**
     * Creates a new {@link ValidationErrorView}.
     * @param controller The controller
     * @param mainFrame The frame
     */
    public ValidationErrorView(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }

    /**
     * show validation error view
     */
    public void displayView() {
        // get the data out of the user preferences
        String addressValue = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE);
        String amountValue = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE);

        // invalid address
        String addressIsInvalid = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID);
        boolean addressIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(addressIsInvalid)) {
            addressIsInvalidBoolean = true;
        }

        // amount is missing
        String amountIsMissing = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING);
        boolean amountIsMissingBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsMissing)) {
            amountIsMissingBoolean = true;
        }

        // invalid amount i.e. not a number or could not parse
        String amountIsInvalid = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID);
        boolean amountIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsInvalid)) {
            amountIsInvalidBoolean = true;
        }

        // amount is negative or zero
        String amountIsNegativeOrZero = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO);
        boolean amountIsNegativeOrZeroBoolean = false;
        if (Boolean.TRUE.toString().equals(amountIsNegativeOrZero)) {
            amountIsNegativeOrZeroBoolean = true;
        }

        // amount is more than available funds
        String notEnoughFunds = controller.getModel().getActiveWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS);
        boolean notEnoughFundsBoolean = false;
        if (Boolean.TRUE.toString().equals(notEnoughFunds)) {
            notEnoughFundsBoolean = true;
        }

        // get localised validation messages;
        String completeMessage = "";

        int rows = 0;
        if (addressIsInvalidBoolean) {
            completeMessage = controller.getLocaliser().getString("validationErrorView.addressInvalidMessage",
                    new String[] { addressValue });
            rows++;
        }
        if (amountIsMissingBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            completeMessage = completeMessage
                    + controller.getLocaliser().getString("validationErrorView.amountIsMissingMessage");
            rows++;
        }
        if (amountIsInvalidBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            completeMessage = completeMessage
                    + controller.getLocaliser().getString("validationErrorView.amountInvalidMessage",
                            new String[] { amountValue });
            rows++;
        }
        if (amountIsNegativeOrZeroBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            completeMessage = completeMessage
                    + controller.getLocaliser().getString("validationErrorView.amountIsNegativeOrZeroMessage");
            rows++;
        }
        if (notEnoughFundsBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            if (fee == null || fee.equals("")) {
                fee = controller.getLocaliser().bitcoinValueToString4(MultiBitModel.SEND_MINIMUM_FEE, false, false);
            }
            completeMessage = completeMessage
                    + controller.getLocaliser().getString("validationErrorView.notEnoughFundsMessage",
                            new String[] { amountValue, fee });
            rows++;
        }
        
        // tell user validation messages
        OkBackToParentAction okAction = new OkBackToParentAction(controller);
        MultiBitButton okButton = new MultiBitButton(okAction, controller);


        Object[] options = { okButton };

        MultiBitTextArea completeMessageTextArea = new MultiBitTextArea(completeMessage, rows, 20, controller);
        completeMessageTextArea.setOpaque(false);
        completeMessageTextArea.setEditable(false);
        
        JOptionPane optionPane = new JOptionPane(completeMessageTextArea, JOptionPane.ERROR_MESSAGE, JOptionPane.DEFAULT_OPTION, MultiBitFrame.createImageIcon(MultiBitFrame.EXCLAMATION_MARK_ICON_FILE),
                options, options[0]);

        messageDialog = optionPane.createDialog(mainFrame, controller.getLocaliser().getString("validationErrorView.title"));
        messageDialog.setVisible(true);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        if (messageDialog != null) {
            messageDialog.setVisible(false);
            messageDialog.dispose();
            messageDialog = null;
        }
    }

    @Override
    public void updateView() {
        // TODO Auto-generated method stub
        
    }
}