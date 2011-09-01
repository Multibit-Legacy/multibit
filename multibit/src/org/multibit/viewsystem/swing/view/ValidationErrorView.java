package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.action.Action;
import org.multibit.action.OkBackToParentAction;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

import com.google.bitcoin.core.Utils;

/**
 * The validation error view - used to tell the user their input is invalid
 */
public class ValidationErrorView implements View {

    private static final long serialVersionUID = 191499812345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private JDialog messageDialog;

    /**
     * Creates a new {@link ValidationErrorView}.
     */
    public ValidationErrorView(MultiBitController controller, 
            MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }

    public String getDescription() {
        return controller.getLocaliser().getString("validationErrorView.title");
    }

    /**
     * show validation error view
     */
    public void displayView() {
        // get the data out of the user preferences
        String addressValue = controller.getModel().getUserPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE);
        String amountValue = controller.getModel().getUserPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE);
       
        // invalid address
        String addressIsInvalid = controller.getModel().getUserPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID);
        boolean addressIsInvalidBoolean = false;
        if (Boolean.TRUE.toString().equals(addressIsInvalid)) {
            addressIsInvalidBoolean = true;           
        }
        
        // invalid amount i.e. not a number or could not parse
        String amountIsInvalid = controller.getModel().getUserPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID);
        boolean amountIsInvalidBoolean = false; 
        if (Boolean.TRUE.toString().equals(amountIsInvalid)) {
            amountIsInvalidBoolean = true;
        }
        
        // amount is more than available funds
        String notEnoughFunds = controller.getModel().getUserPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS);
        boolean notEnoughFundsBoolean = false; 
        if (Boolean.TRUE.toString().equals(notEnoughFunds)) {
            notEnoughFundsBoolean = true;
        }

        // get localised validation messages;
        String completeMessage = "";
        
        if (addressIsInvalidBoolean) {
            completeMessage = controller.getLocaliser().getString("validationErrorView.addressInvalidMessage", new String[] {addressValue});
        }
        if (amountIsInvalidBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            completeMessage = completeMessage + controller.getLocaliser().getString("validationErrorView.amountInvalidMessage", new String[] {amountValue});
        }
        if (notEnoughFundsBoolean) {
            if (!"".equals(completeMessage)) {
                completeMessage = completeMessage + "\n";
            }
            String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            if (fee == null || fee == "") {
                fee = Utils.bitcoinValueToFriendlyString3(MultiBitModel.SEND_FEE_DEFAULT);
            }
            completeMessage = completeMessage + controller.getLocaliser().getString("validationErrorView.notEnoughFundsMessage", new String[] {amountValue, fee});
        }
 
        
        // tell user validation messages
        Object[] options = { controller.getLocaliser().getString("validationErrorView.okOption") };
 
        JOptionPane optionPane = new JOptionPane(completeMessage, JOptionPane.ERROR_MESSAGE,
                JOptionPane.DEFAULT_OPTION, null, options, options[0]);

        messageDialog = optionPane.createDialog(mainFrame,
                controller.getLocaliser().getString("validationErrorView.title"));
        messageDialog.setVisible(true);

        // if ok was pressed (i.e. not disposed by navigateAwayFromView) fire
        // action forward else cancel
        Object returnValue = optionPane.getValue();
        //JOptionPane.showMessageDialog(mainFrame, optionPane.getValue());
        if (returnValue instanceof String && options[0].equals((String) returnValue)) {
                org.multibit.action.OkBackToParentAction okBackToParentAction = new OkBackToParentAction(
                        controller);
                okBackToParentAction.execute(null);
        }
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

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}