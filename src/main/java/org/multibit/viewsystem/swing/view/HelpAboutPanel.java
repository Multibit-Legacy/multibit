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

import java.awt.ComponentOrientation;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ResetTransactionsAction;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The help about view
 */
public class HelpAboutPanel extends JPanel implements View {

    private static final Logger log = LoggerFactory.getLogger(HelpAboutPanel.class);

    private static final long serialVersionUID = 191352212345057705L;

    private static final String SPLASH_ICON_FILE = "/images/splash.jpg";

    private static final String MULTIBIT_URL = "http://multibit.org";

  /**
     * Creates a new {@link HelpAboutPanel}.
     */
    public HelpAboutPanel(MultiBitController controller, MultiBitFrame mainFrame) {        
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
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel1 = new JPanel();
        fillerPanel1.setOpaque(false);
        add(fillerPanel1, constraints);

        ImageIcon imageIcon = createImageIcon(SPLASH_ICON_FILE);
        JLabel splashLabel = new JLabel();
        splashLabel.setIcon(imageIcon);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.4;
        constraints.anchor = GridBagConstraints.CENTER;
        add(splashLabel, constraints);

        MultiBitLabel titleLabel = new MultiBitLabel("", controller);
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(controller.getLocaliser().getString("helpAboutAction.messageBoxTitle"));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        MultiBitLabel urlLabel = new MultiBitLabel(MULTIBIT_URL, controller);
        add(urlLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        MultiBitLabel versionLabel = new MultiBitLabel(versionText, controller);
        add(versionLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.2;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel2 = new JPanel();
        fillerPanel2.setOpaque(false);
        add(fillerPanel2, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.BASELINE_TRAILING;
        ResetTransactionsAction resetTransactionsAction = new ResetTransactionsAction(controller, null);
        MultiBitButton resetTransactionsButton = new MultiBitButton(resetTransactionsAction, controller);
        add(resetTransactionsButton, constraints);
        
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
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

    @Override
    public void updateView() {
        // TODO Auto-generated method stub
        
    }
}