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
package org.multibit.viewsystem.swing.preferences;

import org.multibit.viewsystem.swing.preferences.modules.PreferencesPanelModule;
import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.SystemColor;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box.Filler;
import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.BasePanel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.preferences.actions.ShowPreferencesSubmitAction;
import org.multibit.viewsystem.swing.preferences.actions.ShowPreferencesSubmitAction.SubmitActionCallback;
import org.multibit.viewsystem.swing.preferences.actions.UndoPreferencesChangesSubmitAction;
import org.multibit.viewsystem.swing.preferences.actions.UndoPreferencesChangesSubmitAction.UndoActionCallback;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MutiBitFiller;

/**
 * The show preferences view.
 */
public class PreferencesPanel extends BasePanel<PreferencesPanelModule> {


    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitButton submitButton;
    private MultiBitButton undoChangesButton;
    
    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public PreferencesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }
    
    @Override
    public void displayView() {
        
        String canUndoPreferencesChanges = controller.getModel().getUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES);
        if (Boolean.TRUE.toString().equals(canUndoPreferencesChanges)) {
            undoChangesButton.setEnabled(true);
            String previousUndoChangesText = controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT);
            if (previousUndoChangesText != null && !"".equals(previousUndoChangesText)) {
                undoChangesButton.setText(previousUndoChangesText);
            }
            String previousFontName = controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_NAME);

            if (previousFontName != null && !"".equals(previousFontName)) {
                undoChangesButton.setFont(new Font(previousFontName, FontSizer.INSTANCE.getAdjustedDefaultFont().getStyle(),
                        FontSizer.INSTANCE.getAdjustedDefaultFont().getSize()));
            }
        } else {
            undoChangesButton.setEnabled(false);
        }
        
        super.displayView();
        
    }
    
     @Override
    protected MultiBitController getCoreController() {
        return this.controller;
    }


    @Override
    public void navigateAwayFromView() {
    }
    

    @Override
    protected void initUI() {
        setMinimumSize(new Dimension(550, 160));
        setLayout(new BorderLayout());
        setOpaque(true);
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

//        String[] keys = new String[]{"showPreferencesPanel.feeLabel.text", "fontChooser.fontName", "fontChooser.fontStyle",
//            "fontChooser.fontSize", "showPreferencesPanel.ticker.exchange", "showPreferencesPanel.ticker.currency",
//            "showPreferencesPanel.lookAndFeel"};
//        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this) + STENT_DELTA;

        {
            JPanel mainPanel = new JPanel();
            mainPanel.setOpaque(false);
            mainPanel.setLayout(new GridBagLayout());

            GridBagConstraints constraints = new GridBagConstraints();
                constraints.fill = GridBagConstraints.HORIZONTAL;
                constraints.gridx = 0;
                constraints.gridy = 0;
                constraints.gridwidth = 2;
                constraints.weightx = 1;
                constraints.weighty = 1;
                constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
            
            
            for (PreferencesPanelModule extention : super.extentions) {
                
                List<JPanel> panels = extention.addPanels();
                
                for (JPanel panel : panels)
                {
                constraints.gridy ++;
                mainPanel.add(panel, constraints);
                }
            }
            {
                Filler filler1 = new MutiBitFiller();
                filler1.setOpaque(false);
                constraints.fill = GridBagConstraints.BOTH;
                constraints.gridy ++;
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
            mainScrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
            mainScrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);

            add(mainScrollPane, BorderLayout.CENTER);
        }
        {
            JPanel buttonPanel = createButtonPanel();
            buttonPanel.setMinimumSize(new Dimension(60, 60));
            this.add(buttonPanel, BorderLayout.SOUTH);
        }
    }

    
    @Override
    protected void applyComponentOrientation()
    {
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
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
        
        ShowPreferencesSubmitAction submitAction = new ShowPreferencesSubmitAction(
                controller,
                mainFrame,
                ImageLoader.createImageIcon(ImageLoader.PREFERENCES_ICON_FILE),
                new SubmitActionCallback() {
                    @Override
                    public void fireSubmitAction() {
                        fireSubmitButtonAction();
                    }
                });
        submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        UndoPreferencesChangesSubmitAction undoChangesAction = new UndoPreferencesChangesSubmitAction(
                controller,
                mainFrame,
                ImageLoader.createImageIcon(ImageLoader.UNDO_ICON_FILE),
                new UndoActionCallback() {
                    @Override
                    public void fireUndoAction() {
                        fireUndoButtonAction();
                    }
                });
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
    

    private void fireSubmitButtonAction()
    {
      for (PreferencesPanelModule extention : super.extentions)
        {
            extention.onSubmitAction();
        }
    }
    
    private void fireUndoButtonAction()
    {
      for (PreferencesPanelModule extention : super.extentions)
        {
            extention.onUndoAction();
        }
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

}