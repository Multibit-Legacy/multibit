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

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;

import org.multibit.controller.Controller;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

/**
 * The reset blockchain and transactions view
 */
public class WelcomePanel extends JPanel implements Viewable {

    private static final int TEXT_WIDTH = 48;

    static final String EXAMPLE_TEXT = "The quick brown fox jumps over the lazy dog briskly 0123456789";

    private static final long serialVersionUID = 199992298245057705L;

    private Controller controller;

    /**
     * Creates a new {@link WelcomePanel}.
     */
    public WelcomePanel(Controller controller, MultiBitFrame mainFrame) {
        this.controller = controller;

        setLayout(new BorderLayout());
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        initUI();
    }

    private void initUI() {
        setMinimumSize(new Dimension(400, 500));

        JPanel mainPanel = new JPanel();
        mainPanel.setMinimumSize(new Dimension(400, 500));
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setOpaque(false);
        mainPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 10;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createWelcomePanel(), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);

        Action helpAction;
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
                "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                HelpContentsPanel.HELP_CONTENTS_URL);
        } else {
            helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_RTL_ICON_FILE,
                    "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                    HelpContentsPanel.HELP_CONTENTS_URL);
        }
        HelpButton helpButton = new HelpButton(helpAction, controller);
        helpButton.setText("");

        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] { controller.getLocaliser().getString(
                "multiBitFrame.helpMenuTooltip") });
        helpButton.setToolTipText(tooltipText);
        helpButton.setHorizontalAlignment(SwingConstants.LEADING);
        helpButton.setBorder(BorderFactory.createEmptyBorder(0, AbstractTradePanel.HELP_BUTTON_INDENT,
                AbstractTradePanel.HELP_BUTTON_INDENT, AbstractTradePanel.HELP_BUTTON_INDENT));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BASELINE_LEADING;
        mainPanel.add(helpButton, constraints);

        JLabel filler2 = new JLabel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 10000;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(filler2, constraints);

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        mainScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

        add(mainScrollPane, BorderLayout.CENTER);
    }

    private JPanel createWelcomePanel() {
        MultiBitTitledPanel welcomePanel = new MultiBitTitledPanel(controller.getLocaliser().getString("welcomePanel.title"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        welcomePanel.setOpaque(false);

        FontMetrics fontMetrics = welcomePanel.getFontMetrics(welcomePanel.getFont());
        int preferredWidth = fontMetrics.stringWidth(EXAMPLE_TEXT);
        int fontHeight = fontMetrics.getHeight();

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(" ", 3, welcomePanel);

        String paragraph1 = controller.getLocaliser().getString("welcomePanel.paragraph1");
        int height1 = calculateHeight(paragraph1);

        MultiBitTextArea paragraph1TextArea = new MultiBitTextArea(paragraph1, height1, TEXT_WIDTH, controller);
        paragraph1TextArea.setMinimumSize(new Dimension(preferredWidth, height1 * fontHeight));
        paragraph1TextArea.setPreferredSize(new Dimension(preferredWidth, height1 * fontHeight));
        paragraph1TextArea.setOpaque(false);
        paragraph1TextArea.setWrapStyleWord(true);
        paragraph1TextArea.setLineWrap(true);
        paragraph1TextArea.setEditable(false);
        //paragraph1TextArea.setBorder(BorderFactory.createLineBorder(Color.RED));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph1TextArea, constraints);

        String paragraph2 = controller.getLocaliser().getString("welcomePanel.paragraph2");
        int height2 = calculateHeight(paragraph2);

        MultiBitTextArea paragraph2TextArea = new MultiBitTextArea(paragraph2, height2, TEXT_WIDTH, controller);
        paragraph2TextArea.setMinimumSize(new Dimension(preferredWidth, height2 * fontHeight));
        paragraph2TextArea.setPreferredSize(new Dimension(preferredWidth, height2 * fontHeight));
        paragraph2TextArea.setOpaque(false);
        paragraph2TextArea.setWrapStyleWord(true);
        paragraph2TextArea.setLineWrap(true);
        paragraph2TextArea.setEditable(false);
        //paragraph2TextArea.setBorder(BorderFactory.createLineBorder(Color.BLUE));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph2TextArea, constraints);

        String paragraph25 = controller.getLocaliser().getString("welcomePanel.paragraph2.5");
        int height25 = calculateHeight(paragraph25);

        MultiBitTextArea paragraph25TextArea = new MultiBitTextArea(paragraph25, height25, TEXT_WIDTH, controller);
        paragraph25TextArea.setMinimumSize(new Dimension(preferredWidth, height25 * fontHeight));
        paragraph25TextArea.setPreferredSize(new Dimension(preferredWidth, height25 * fontHeight));
        paragraph25TextArea.setOpaque(false);
        paragraph25TextArea.setWrapStyleWord(true);
        paragraph25TextArea.setLineWrap(true);
        paragraph25TextArea.setEditable(false);
        //paragraph25TextArea.setBorder(BorderFactory.createLineBorder(Color.CYAN));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph25TextArea, constraints);

        String paragraph3 = controller.getLocaliser().getString("welcomePanel.paragraph3");
        int height3 = calculateHeight(paragraph3);

        MultiBitTextArea paragraph3TextArea = new MultiBitTextArea(paragraph3, height3, TEXT_WIDTH, controller);
        paragraph3TextArea.setMinimumSize(new Dimension(preferredWidth, height3 * fontHeight));
        paragraph3TextArea.setPreferredSize(new Dimension(preferredWidth, height3 * fontHeight));
        paragraph3TextArea.setOpaque(false);
        paragraph3TextArea.setWrapStyleWord(true);
        paragraph3TextArea.setLineWrap(true);
        paragraph3TextArea.setEditable(false);
        //paragraph3TextArea.setBorder(BorderFactory.createLineBorder(Color.RED));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph3TextArea, constraints);

        String paragraph4 = controller.getLocaliser().getString("welcomePanel.paragraph4");
        int height4 = calculateHeight(paragraph4);

        MultiBitTextArea paragraph4TextArea = new MultiBitTextArea(paragraph4, height4, TEXT_WIDTH, controller);
        paragraph4TextArea.setMinimumSize(new Dimension(preferredWidth, height4 * fontHeight));
        paragraph4TextArea.setPreferredSize(new Dimension(preferredWidth, height4 * fontHeight));
        paragraph4TextArea.setOpaque(false);
        paragraph4TextArea.setWrapStyleWord(true);
        paragraph4TextArea.setLineWrap(true);
        paragraph4TextArea.setEditable(false);
        //paragraph4TextArea.setBorder(BorderFactory.createLineBorder(Color.MAGENTA));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 8;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        welcomePanel.add(paragraph4TextArea, constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 9;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1000;
        constraints.anchor = GridBagConstraints.CENTER;
        welcomePanel.add(filler1, constraints);

        return welcomePanel;
    }

    public static int calculateHeight(String text) {
        return (int) Math.ceil((text.length() * 0.75) / (TEXT_WIDTH) + 0.5);
    }

    @Override
    public void navigateAwayFromView() {
    }

    /**
     * show welcome panel
     */
    @Override
    public void displayView(DisplayHint displayHint) {
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
    public View getViewId() {
        return View.WELCOME_VIEW;
    }
}