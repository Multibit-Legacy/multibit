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
package org.multibit.viewsystem.swing.view.panels;

import java.awt.ComponentOrientation;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.controller.Controller;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;

/**
 * The help about view.
 */
public class HelpAboutPanel extends JPanel implements Viewable {
    private static final long serialVersionUID = 191352212345057705L;

    private static final String MULTIBIT_URL = "https://multidoge.org";
    
    private Controller controller;

  /**
     * Creates a new {@link HelpAboutPanel}.
     */
    public HelpAboutPanel(Controller controller, MultiBitFrame mainFrame) {        
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        this.controller = controller;

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

        MultiBitLabel titleLabel = new MultiBitLabel("");
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(controller.getLocaliser().getString("helpAboutAction.messageBoxTitle"));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        MultiBitLabel urlLabel = new MultiBitLabel(MULTIBIT_URL);
        add(urlLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        MultiBitLabel versionLabel = new MultiBitLabel(versionText);
        add(versionLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.2;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel2 = new JPanel();
        fillerPanel2.setOpaque(false);
        add(fillerPanel2, constraints);

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    @Override
    public void navigateAwayFromView() {
    }

    @Override
    public void displayView(DisplayHint displayHint) {        
    }
       
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.MULTIBIT_SMALL_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("helpAboutAction.tooltip");
    }
    
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("helpAboutAction.tooltip");
    }

    @Override
    public View getViewId() {
        return View.HELP_ABOUT_VIEW;
    }
}