package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.action.SendBitcoinNowAction;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The send bitcoin confirm view
 */
public class SendBitcoinConfirmView implements View {

    private static final long serialVersionUID = 191435612345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private JDialog messageDialog;

    /**
     * Creates a new {@link SendBitcoinConfirmView}.
     */
    public SendBitcoinConfirmView(MultiBitController controller, Localiser localiser,
            MultiBitFrame mainFrame) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;
    }

    public String getDescription() {
        return localiser.getString("sendBitcoinConfirmView.title");
    }

    /**
     * show send bitcoin confirm view
     */
    public void displayView() {

        // ask for confirmation of send bitcoin
        Object[] options = { localiser.getString("sendBitcoinConfirmView.sendOption"),
                localiser.getString("sendBitcoinConfirmView.cancelOption") };
        String confirmMessage = localiser.getString("sendBitcoinConfirmView.message", new String[] {
                "AMOUNT", "ADDRESS", "LABEL" });
        JOptionPane optionPane = new JOptionPane(confirmMessage, JOptionPane.INFORMATION_MESSAGE,
                JOptionPane.DEFAULT_OPTION, null, options, options[1]);

        messageDialog = optionPane.createDialog(mainFrame,
                localiser.getString("sendBitcoinConfirmView.title"));
        messageDialog.show();

        // if send was pressed (i.e. not disposed by navigateAwayFromView) fire
        // action forward else cancel
        Object returnValue = optionPane.getValue();
        // JOptionPane.showMessageDialog(mainFrame, optionPane.getValue());
        if (returnValue instanceof String && options[0].equals((String) returnValue)) {
            // send
            // actually send bitcoin
            SendBitcoinNowAction sendBitcoinNowAction = new SendBitcoinNowAction(controller);
            // the proposed spend is on the model
            sendBitcoinNowAction.execute(null);
        } else {
            // cancel
            if (returnValue instanceof String && options[1].equals((String) returnValue)) {
                org.multibit.action.CancelBackToParentAction cancelBackToParentAction = new org.multibit.action.CancelBackToParentAction(
                        controller);
                cancelBackToParentAction.execute(null);
            }
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