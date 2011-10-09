package org.multibit.viewsystem.swing.view;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;

/**
 * The help about view
 */
public class HelpAboutPanel extends JPanel implements View {

    private static final Logger log = LoggerFactory.getLogger(HelpAboutPanel.class);

    private static final long serialVersionUID = 191352212345057705L;

    private static final String SPLASH_ICON_FILE = "/images/splash.jpg";

    private static final String MULTIBIT_URL = "http://multibit.org";

    private MultiBitController controller;

  /**
     * Creates a new {@link HelpAboutPanel}.
     */
    public HelpAboutPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        
        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0), BorderFactory.createMatteBorder(1, 0, 1, 0,  MultiBitFrame.DARK_BACKGROUND_COLOR.darker())));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

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
        fillerPanel1.setOpaque(false);
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

        ImageIcon imageIcon = createImageIcon(SPLASH_ICON_FILE);
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
        fillerPanel2.setOpaque(false);
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

    /**
     * TODO Consider refactoring this method into a static utility
     * @param path The image path
     * @return an ImageIcon, or null if the path was invalid.
     */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            log.error("org.multibit.ViewerFrame#createImageIcon: Could not find file: "
              + path);
            return null;
        }
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }
}