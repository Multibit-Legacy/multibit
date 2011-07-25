package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The help about view
 */
public class HelpAboutView implements View {

    private static final long serialVersionUID = 191352212345057705L;

    private static final String SPLASH_ICON_FILE = "/images/splash.jpg";

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private ImageIcon imageIcon;

    private JDialog messageDialog;

    /**
     * Creates a new {@link HelpAboutView}.
     */
    public HelpAboutView(MultiBitController controller, Localiser localiser, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;

        // messageBox icon
        imageIcon = createImageIcon(SPLASH_ICON_FILE);
    }

    public String getDescription() {
        return localiser.getString("helpAboutAction.messageBoxTitle");
    }

    /**
     * show help about message box
     */
    @SuppressWarnings("deprecation")
    public void displayView() {
        // JOptionPane.showMessageDialog(mainFrame, "HelpAboutView#displayView called");
        String versionNumber = localiser.getVersionNumber();

        JOptionPane optionPane = new JOptionPane(localiser.getString("helpAboutAction.messageText",
                new Object[] { versionNumber }), JOptionPane.INFORMATION_MESSAGE,
                JOptionPane.DEFAULT_OPTION, imageIcon);

        messageDialog = optionPane.createDialog(mainFrame,
                localiser.getString("helpAboutAction.messageBoxTitle"));
        messageDialog.show();
        
        // if ok was pressed (i.e. not disposed by navigateAwayFromView) fire action forward
        Object returnValue = optionPane.getValue();
        if (returnValue instanceof Integer && ((Integer)returnValue).intValue() == JOptionPane.OK_OPTION) {
            controller.setActionForwardToChild(ActionForward.FORWARD_TO_PREVIOUS);
        }
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("org.multibit.ViewerFrame#createImageIcon: Could not find file: "
                    + path);
            return null;
        }
    }

    public void navigateAwayFromView(int nextViewId) {
        if (messageDialog != null) {
            messageDialog.setVisible(false);
            messageDialog.dispose();
        }
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}