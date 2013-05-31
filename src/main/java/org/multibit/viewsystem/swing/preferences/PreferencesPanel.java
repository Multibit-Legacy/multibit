/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License
 * at
 *
 * http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package org.multibit.viewsystem.swing.preferences;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.SystemColor;
import java.awt.event.ActionEvent;
import java.util.Set;
import javax.swing.AbstractAction;
import static javax.swing.Action.MNEMONIC_KEY;
import static javax.swing.Action.SHORT_DESCRIPTION;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.core.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.core.MnemonicUtil;
import org.multibit.viewsystem.swing.core.components.FontSizer;
import org.multibit.viewsystem.swing.core.components.MultiBitButton;
import org.multibit.viewsystem.swing.core.panels.HelpContentsPanel;

/**
 * The show preferences view.
 */
public class PreferencesPanel extends JPanel implements Viewable {

    private static final Logger log = LoggerFactory.getLogger(PreferencesPanel.class);
    private static final long serialVersionUID = 191352298245057705L;
    private final MultiBitFrame mainFrame;
    private final Controller controller;
    private final Set<PreferencesModule> preferencesModules;
    private final Set<PreferencesAction> preferencesActions;
    private Boolean hasInitialized = false;
    public final RedrawCallback redrawCallback = new RedrawCallback();
    public final GetFontMetricsCallback getFontMetricsCallback = new GetFontMetricsCallback();
    private MultiBitButton undoChangesButton;
    private static final int STENT_DELTA = 0;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public PreferencesPanel(MultiBitFrame mainFrame, Controller controller, Set<PreferencesModule> preferencesModules, Set<PreferencesAction> preferencesActions) {
        log.debug("Construct a new ShowPreferencesPanel");
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.preferencesModules = preferencesModules;
        this.preferencesActions = preferencesActions;

        Setup();
        initUI();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * Update preferences panel.
     */
    @Override
    public void displayView(DisplayHint displayHint) {
        //log.debug("Received a displayView with hint " + displayHint.toString());
        if (DisplayHint.WALLET_TRANSACTIONS_HAVE_CHANGED == displayHint) {
            return;
        }

        for (PreferencesModule preferencesModule : preferencesModules) {
            try {
                preferencesModule.Update();
            } catch (PreferencesModule.SetupNotCalledException ex) {
                log.error("no setup called", ex);
                return;
            }
        }

        String canUndoPreferencesChanges = controller.getModel().getUserPreference(CoreModel.CAN_UNDO_PREFERENCES_CHANGES);
        if (Boolean.TRUE.toString().equals(canUndoPreferencesChanges)) {
            undoChangesButton.setEnabled(true);
            String previousUndoChangesText = controller.getModel().getUserPreference(CoreModel.PREVIOUS_UNDO_CHANGES_TEXT);
            if (previousUndoChangesText != null && !"".equals(previousUndoChangesText)) {
                undoChangesButton.setText(previousUndoChangesText);
            }
            String previousFontName = controller.getModel().getUserPreference(CoreModel.PREVIOUS_FONT_NAME);

            if (previousFontName != null && !"".equals(previousFontName)) {
                undoChangesButton.setFont(new Font(previousFontName, FontSizer.INSTANCE.getAdjustedDefaultFont().getStyle(),
                        FontSizer.INSTANCE.getAdjustedDefaultFont().getSize()));
            }
        } else {
            undoChangesButton.setEnabled(false);
        }

        redrawCallback.redraw();

    }

    @Override
    public void navigateAwayFromView() {
    }

    private void Setup() {

        for (PreferencesModule preferencesModule : this.preferencesModules) {
            preferencesModule.Setup(this.redrawCallback, this.getFontMetricsCallback);
        }


    }

    private void initUI() {
        if (this.hasInitialized == true) {
            return;
        } else {
            this.hasInitialized = true;
        }

        setMinimumSize(new Dimension(550, 160));
        setLayout(new BorderLayout());
        setOpaque(true);
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);
        mainPanel.setLayout(new GridBagLayout());

        {
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.fill = GridBagConstraints.HORIZONTAL;
            constraints.gridx = 0;
            constraints.gridwidth = 2;
            constraints.weightx = 1;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.CENTER;

            Integer gridBagY = 0;

            for (PreferencesModule preferencesModule : this.preferencesModules) {
                Set<JPanel> jPanels;
                try {
                    jPanels = preferencesModule.Init();
                } catch (PreferencesModule.SetupNotCalledException ex) {
                    log.error("setup not called", ex);
                    return;
                }

                for (JPanel jPanel : jPanels) {
                    constraints.gridy = gridBagY++;
                    mainPanel.add(jPanel, constraints);
                }
            }
        }

        {
            JLabel filler1 = new JLabel();
            filler1.setOpaque(false);
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.fill = GridBagConstraints.BOTH;
            constraints.gridx = 0;
            constraints.gridy = 5;
            constraints.gridwidth = 2;
            constraints.weightx = 1;
            constraints.weighty = 100;
            constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
            mainPanel.add(filler1, constraints);
        }

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        mainScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

        add(mainScrollPane, BorderLayout.CENTER);

        JPanel buttonPanel = createButtonPanel();
        buttonPanel.setMinimumSize(new Dimension(60, 60));
        add(buttonPanel, BorderLayout.SOUTH);
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();

        buttonPanel
                .setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(1, 0, 0, 0, SystemColor.windowBorder),
                BorderFactory.createEmptyBorder(2, 0, 2, 0)));
        buttonPanel.setOpaque(true);
        buttonPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        buttonPanel.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        PreferencesSubmitAction submitAction = new PreferencesSubmitAction(ImageLoader.createImageIcon(ImageLoader.PREFERENCES_ICON_FILE));
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        PreferencesUndoChangesSubmitAction undoChangesAction = new PreferencesUndoChangesSubmitAction(ImageLoader.createImageIcon(ImageLoader.UNDO_ICON_FILE));
        undoChangesButton = new MultiBitButton(undoChangesAction, controller);

        buttonPanel.add(undoChangesButton);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        buttonPanel.add(submitButton, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        buttonPanel.add(undoChangesButton, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        buttonPanel.add(fill1, constraints);

        return buttonPanel;
    }

    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.PREFERENCES_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showPreferencesAction.text");
    }

    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showPreferencesAction.tooltip");
    }

    @Override
    public View getViewId() {
        return View.PREFERENCES_VIEW;
    }

    public class RedrawCallback {

        public void redraw() {
            invalidate();
            validate();
            repaint();
        }
    }

    public class GetFontMetricsCallback {

        public FontMetrics get(Font font) {
            return getFontMetrics(font);
        }
    }

    private class PreferencesSubmitAction extends AbstractAction {

        private PreferencesSubmitAction(Icon icon) {
            super(controller.getLocaliser().getString("showPreferencesSubmitAction.text"), icon);

            MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
            putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip")));
            putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
        }

        /**
         * Change preferences.
         */
        @Override
        public void actionPerformed(ActionEvent event) {

            try {
                if (mainFrame != null) {
                    mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                }

                Boolean needToUpdate = false;


                for (PreferencesAction preferencesAction : preferencesActions) {
                    Boolean result = preferencesAction.Submit();
                    if (result) {
                        needToUpdate = true;
                    }
                }

                if (needToUpdate) {
                    ColorAndFontConstants.init();
                    FontSizer.INSTANCE.initialise(controller);
                    HelpContentsPanel.clearBrowser();

                    // Switch off blinks.
                    //bitcoinController.getModel().setBlinkEnabled(false);

                    try {
                        controller.fireDataStructureChanged();
                        SwingUtilities.updateComponentTreeUI(mainFrame);
                    } finally {
                        // Switch blinks back on.
                        //bitcoinController.getModel().setBlinkEnabled(true);
                    }
                }
            } finally {
                if (mainFrame != null) {
                    mainFrame.setCursor(Cursor.getDefaultCursor());
                }
            }
        }
    }

    private class PreferencesUndoChangesSubmitAction extends AbstractAction {

        /**
         * Creates a new {@link UndoPreferencesChangesSubmitAction}.
         */
        public PreferencesUndoChangesSubmitAction(ImageIcon icon) {
            super(controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text"), icon);

            MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
            putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.tooltip")));
            putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("undoPreferencesChangesSubmitAction.mnemonicKey"));
        }

        /**
         * Get the previous language and font changes and undo them
         */
        @Override
        public void actionPerformed(ActionEvent event) {
            String previousFontName = (String) controller.getModel().getUserPreference(CoreModel.PREVIOUS_FONT_NAME);
            String previousFontStyle = (String) controller.getModel().getUserPreference(CoreModel.PREVIOUS_FONT_STYLE);
            int previousFontStyleAsInt = 0;
            try {
                previousFontStyleAsInt = Integer.parseInt(previousFontStyle);
            } catch (NumberFormatException nfe) {
                // just use 0 = plain
            }
            String previousFontSize = (String) controller.getModel().getUserPreference(CoreModel.PREVIOUS_FONT_SIZE);
            int previousFontSizeAsInt = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
            try {
                previousFontSizeAsInt = Integer.parseInt(previousFontSize);
            } catch (NumberFormatException nfe) {
                // just use default
            }

            controller.getModel().setUserPreference(CoreModel.USER_LANGUAGE_CODE,
                    (String) controller.getModel().getUserPreference(CoreModel.PREVIOUS_USER_LANGUAGE_CODE));
            controller.getModel().setUserPreference(CoreModel.FONT_NAME, previousFontName);
            controller.getModel().setUserPreference(CoreModel.FONT_STYLE, previousFontStyle);
            controller.getModel().setUserPreference(CoreModel.FONT_SIZE, previousFontSize);
            controller.getModel().setUserPreference(CoreModel.CAN_UNDO_PREFERENCES_CHANGES, "false");

            // return to the same view but fire data structure change to reset
            // everything
            FontSizer.INSTANCE.initialise(controller);
            UIManager.put("ToolTip.font", new Font(previousFontName, previousFontStyleAsInt, previousFontSizeAsInt));

            controller.fireDataStructureChanged();
            controller.displayView(View.PREFERENCES_VIEW);
        }
    }
}