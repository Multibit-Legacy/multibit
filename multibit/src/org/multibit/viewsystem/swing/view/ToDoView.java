package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The a placeholder view
 */
public class ToDoView implements View {

    private static final long serialVersionUID = 191435612345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private JDialog messageDialog;

    /**
     * Creates a new {@link ToDoView}.
     */
    public ToDoView(MultiBitController controller, Localiser localiser, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;
   }

    public String getDescription() {
        return "TODO";
    }

    /**
     * show message box with TODO
     */
    public void displayView() {
        JOptionPane optionPane = new JOptionPane("TODO", 
                JOptionPane.INFORMATION_MESSAGE,
                JOptionPane.DEFAULT_OPTION);

        messageDialog = optionPane.createDialog(mainFrame,"TODO");
        messageDialog.show();
        
        // if ok was pressed (i.e. not disposed by navigateAwayFromView) fire action forward
       Object returnValue = optionPane.getValue();
       //JOptionPane.showMessageDialog(mainFrame, optionPane.getValue());
       if (returnValue instanceof Integer && ((Integer)returnValue).intValue() == JOptionPane.OK_OPTION) {
           //JOptionPane.showMessageDialog(mainFrame, "FIRE");
            controller.setActionForwardToChild(ActionForward.FORWARD_TO_PREVIOUS);
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