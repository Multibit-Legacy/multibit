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

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

/**
 * The reset blockchain and transactions view
 */
public class WelcomePanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 199992298245057705L;

    private MultiBitController controller;

    private Data data;

    /**
     * Creates a new {@link WelcomePanel}.
     */
    public WelcomePanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;

        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        initUI();
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));

        GridBagConstraints constraints = new GridBagConstraints();
        setLayout(new GridBagLayout());

        String[] keys = new String[] {"resetTransactionsPanel.walletDescriptionLabel", "resetTransactionsPanel.walletFilenameLabel"};
        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createWelcomePanel(stentWidth), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        add(filler1, constraints);

        Action helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
                "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                HelpContentsPanel.HELP_CONTENTS_URL);
        HelpButton helpButton = new HelpButton(helpAction, controller);
        helpButton.setText("");

        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        helpButton.setToolTipText(tooltipText);
        helpButton.setHorizontalAlignment(SwingConstants.LEADING);
        helpButton.setBorder(BorderFactory.createEmptyBorder(0, AbstractTradePanel.HELP_BUTTON_INDENT, AbstractTradePanel.HELP_BUTTON_INDENT, 0));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BASELINE_LEADING;
        add(helpButton, constraints);

        JLabel filler2 = new JLabel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 100;
        constraints.anchor = GridBagConstraints.CENTER;
        add(filler2, constraints);
    }

    private JPanel createWelcomePanel(int stentWidth) {
        MultiBitTitledPanel welcomePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "welcomePanel.title"));
        
        welcomePanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(" ", 3, welcomePanel);

        MultiBitTextArea paragraph1TextArea = new MultiBitTextArea(controller.getLocaliser().getString("welcomePanel.paragraph1"), 4, 40, controller);
        paragraph1TextArea.setOpaque(false);
        paragraph1TextArea.setWrapStyleWord(true);
        paragraph1TextArea.setLineWrap(true);
        paragraph1TextArea.setEditable(false);
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph1TextArea, constraints);

        MultiBitTextArea paragraph2TextArea = new MultiBitTextArea(controller.getLocaliser().getString("welcomePanel.paragraph2"), 4, 40, controller);
        paragraph2TextArea.setOpaque(false);
        paragraph2TextArea.setWrapStyleWord(true);
        paragraph2TextArea.setLineWrap(true);
        paragraph2TextArea.setEditable(false);
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph2TextArea, constraints);

        MultiBitTextArea paragraph3TextArea = new MultiBitTextArea(controller.getLocaliser().getString("welcomePanel.paragraph3"), 3, 40, controller);
        paragraph3TextArea.setOpaque(false);
        paragraph3TextArea.setWrapStyleWord(true);
        paragraph3TextArea.setLineWrap(true);
        paragraph3TextArea.setEditable(false);
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph3TextArea, constraints);

        MultiBitTextArea paragraph4TextArea = new MultiBitTextArea(controller.getLocaliser().getString("welcomePanel.paragraph4"), 4, 40, controller);
        paragraph4TextArea.setOpaque(false);
        paragraph4TextArea.setWrapStyleWord(true);
        paragraph4TextArea.setLineWrap(true);
        paragraph4TextArea.setEditable(false);
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph4TextArea, constraints);

        return welcomePanel;
    }
     
    @Override
    public Data getData() {
        return data;
    }


    @Override
    public void navigateAwayFromView() {
    }
    
    /**
     * show welcome panel
     */
    @Override
    public void displayView() {
    }
    
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.WELCOME_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("welcomePanel.text");
    }
    
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("welcomePanel.title");
    }

    @Override
    public int getViewId() {
        return View.WELCOME_VIEW;
    }
}