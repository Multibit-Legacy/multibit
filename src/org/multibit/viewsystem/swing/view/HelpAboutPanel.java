package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Collection;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The help about view
 */
public class HelpAboutPanel extends JPanel implements View {

    private static final long serialVersionUID = 191352212345057705L;

    private static final String SPLASH_ICON_FILE = "/images/splash.jpg";

    private static final String MULTIBIT_URL = "http://multibit.org";
    
    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private ImageIcon imageIcon;

    private JDialog messageDialog;

    /**
     * Creates a new {@link HelpAboutPanel}.
     */
    public HelpAboutPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        
        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));
        
        String versionNumber = controller.getLocaliser().getVersionNumber();

        String versionText = controller.getLocaliser().getString("helpAboutAction.versionText",
                new Object[] { versionNumber });

        GridBagConstraints constraints = new GridBagConstraints();
        setLayout(new GridBagLayout());
        
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.12;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel1 = new JPanel();
        add(fillerPanel1, constraints);


        JLabel titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(getDescription());
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE, MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        titleLabel.setFont(font);
 
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        add(titleLabel, constraints);

        imageIcon = createImageIcon(SPLASH_ICON_FILE);
        JLabel splashLabel = new JLabel();
        splashLabel.setIcon(imageIcon);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.64;
        constraints.anchor = GridBagConstraints.CENTER;
        add(splashLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel urlLabel = new JLabel(MULTIBIT_URL);
        add(urlLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel versionLabel = new JLabel(versionText);
        add(versionLabel, constraints);
        
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.12;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel2 = new JPanel();
        add(fillerPanel2, constraints);

    }

    public String getDescription() {
        return controller.getLocaliser().getString("helpAboutAction.messageBoxTitle");
    }

    /**
     * show help about message box
     */
    public void displayView() {
 
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

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}