package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The show preferences view
 */
public class ShowPreferencesView implements View {

    private static final long serialVersionUID = 191435612343457705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private ShowPreferencesDialog showPreferencesDialog;

    /**
     * Creates a new {@link ShowPreferencesView}.
     */
    public ShowPreferencesView(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
   }

    public String getDescription() {
        return controller.getLocaliser().getString("showPreferencesDialog.title");
    }

    /**
     * show show preferences dialog
     */
    public void displayView() {
        showPreferencesDialog = new ShowPreferencesDialog(mainFrame, controller);
        
        showPreferencesDialog.setVisible(true);
        
        // the action listeners of the code in the dialog do all the action forwarding so nothing to do here
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId) {
        if (showPreferencesDialog != null) {
            showPreferencesDialog.setVisible(false);
            showPreferencesDialog.dispose();
            showPreferencesDialog = null;
        }
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}