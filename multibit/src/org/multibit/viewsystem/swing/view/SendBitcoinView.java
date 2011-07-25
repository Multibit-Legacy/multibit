package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The send bitcoin view
 */
public class SendBitcoinView implements View {

    private static final long serialVersionUID = 19143234343457705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private SendBitcoinDialog sendBitcoinDialog;

    /**
     * Creates a new {@link SendBitcoinView}.
     */
    public SendBitcoinView(MultiBitController controller, Localiser localiser, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;
   }

    public String getDescription() {
        return localiser.getString("sendBitcoinDialog.title");
    }

    /**
     * show send bitcoin dialog
     */
    public void displayView() {
        sendBitcoinDialog = new SendBitcoinDialog(mainFrame, controller);
        
        sendBitcoinDialog.setVisible(true);
        
        // the action listeners of the code in the dialog do all the action forwarding so nothing to do here
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId) {
        if (sendBitcoinDialog != null) {
            sendBitcoinDialog.setVisible(false);
            sendBitcoinDialog.dispose();
            sendBitcoinDialog = null;
        }
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}