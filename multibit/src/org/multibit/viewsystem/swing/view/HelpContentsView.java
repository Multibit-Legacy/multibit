package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The help contents view
 */
public class HelpContentsView implements View {

    private static final long serialVersionUID = 191435612345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private Browser browser;

    /**
     * Creates a new {@link HelpContentsView}.
     */
    public HelpContentsView(MultiBitController controller, Localiser localiser, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;
    }

    public String getDescription() {
        return localiser.getString("helpContentsView.messageTitle");
    }

    /**
     * show help contents message box
     */
    public void displayView() {
        browser = new Browser(mainFrame);

        controller.setActionForwardToParent();
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        // just keep the help frame open
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}