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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.SystemColor;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.plaf.metal.MetalComboBoxUI;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ChooseFontAction;
import org.multibit.viewsystem.swing.action.ShowPreferencesSubmitAction;
import org.multibit.viewsystem.swing.action.UndoPreferencesChangesSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

/**
 * The show preferences view
 */
public class ShowPreferencesPanel extends JPanel implements View, DataProvider {

    private static final int LANGUAGE_CODE_VERTICAL_INSET = 2;

    private static final int LANGUAGE_CODE_IMAGE_HEIGHT = 20;

    private static final int LANGUAGE_CODE_IMAGE_WIDTH = 26;

    private static final long serialVersionUID = 191352298245057705L;

    private static final String A_LONG_LANGUAGE_NAME = "LithuanianXY";
    private static final int LANGUAGE_COMBO_WIDTH_DELTA = 40;
    private static final int LANGUAGE_COMBO_HEIGHT_DELTA = 5;

    private static final int FEE_TEXT_FIELD_HEIGHT = 30;
    private static final int FEE_TEXT_FIELD_WIDTH = 200;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    SortedSet<LanguageData> languageDataSet;

    private JRadioButton useDefaultLocale;
    private JComboBox languageComboBox;

    private MultiBitTextField feeTextField;
    private String originalFee;

    private JRadioButton ignoreAll;
    private JRadioButton fillAutomatically;
    private JRadioButton askEveryTime;

    private MultiBitLabel fontNameTextLabel;
    private MultiBitLabel fontStyleTextLabel;
    private MultiBitLabel fontSizeTextLabel;

    private String originalFontName;
    private String originalFontStyle;
    private String originalFontSize;
    
    private String originalShowDialogString;
    private String originalUseUriString;
    
    private MultiBitButton undoChangesButton;

    private Data data;

    private Font selectedFont;
    
    private static final int STENT_DELTA = 0;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public ShowPreferencesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.controller = controller;

        data = new Data();

        initUI();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * update preferences panel
     */
    public void displayView() {
        String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);

        if (sendFeeString == null || sendFeeString == "") {
            sendFeeString = controller.getLocaliser().bitcoinValueToString4(MultiBitModel.SEND_FEE_DEFAULT, false, false);
        }
        originalFee = sendFeeString;
        feeTextField.setText(sendFeeString);

        originalShowDialogString = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        originalUseUriString = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_USE_URI);

        if (!(Boolean.FALSE.toString().equalsIgnoreCase(originalShowDialogString))) {
            // missing showDialog or it is set to true
            askEveryTime.setSelected(true);
        } else {
            if (!(Boolean.FALSE.toString().equalsIgnoreCase(originalUseUriString))) {
                // missing useUri or it is set to true
                fillAutomatically.setSelected(true);
            } else {
                // useUri set to false
                ignoreAll.setSelected(true);
            }
        }

        String fontNameString = controller.getModel().getUserPreference(MultiBitModel.FONT_NAME);
        if (fontNameString == null || "".equals(fontNameString)) {
            fontNameString = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_NAME;
        }
        originalFontName = fontNameString;

        int fontStyle = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_STYLE;
        String fontStyleString = controller.getModel().getUserPreference(MultiBitModel.FONT_STYLE);
        if (fontStyleString != null && !"".equals(fontStyleString)) {
            try {
                fontStyle = Integer.parseInt(fontStyleString);
            } catch (NumberFormatException nfe) {
                // use default
            }
        }
        originalFontStyle = "" + fontStyle;

        int fontSize = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        if (fontSizeString != null && !"".equals(fontSizeString)) {
            try {
                fontSize = Integer.parseInt(fontSizeString);
            } catch (NumberFormatException nfe) {
                // use default
            }
        }
        originalFontSize = "" + fontSize;

        setSelectedFont(new Font(fontNameString, fontStyle, fontSize));
        
        String canUndoPreferencesChanges = controller.getModel().getUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES);
        if(Boolean.TRUE.toString().equals(canUndoPreferencesChanges)) {
            undoChangesButton.setEnabled(true);
            String previousUndoChangesText = controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT);
            if (previousUndoChangesText != null && !"".equals(previousUndoChangesText)) {
                undoChangesButton.setText(previousUndoChangesText);
            }
            String previousFontName = controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_NAME);

            if (previousFontName != null && !"".equals(previousFontName)) {
                undoChangesButton.setFont(new Font(previousFontName, FontSizer.INSTANCE.getAdjustedDefaultFont().getStyle(), FontSizer.INSTANCE.getAdjustedDefaultFont().getSize()));
            }
        } else {
            undoChangesButton.setEnabled(false);
        }
        
        invalidate();
        validate();
        repaint();
    }

    @Override
    public void navigateAwayFromView() {
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));
        setLayout(new BorderLayout());
        setOpaque(true);
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);

        String[] keys = new String[] {"showPreferencesPanel.feeLabel.text", "fontChooser.fontName",
                "fontChooser.fontStyle", "fontChooser.fontSize"};
        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this) + STENT_DELTA;

        GridBagConstraints constraints = new GridBagConstraints();
        mainPanel.setLayout(new GridBagLayout());
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(createFeePanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(createLanguagePanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(createFontChooserPanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(createBrowserIntegrationPanel(stentWidth), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 100;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(filler1, constraints);

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        //mainScrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, ColorAndFontConstants.DARK_BACKGROUND_COLOR));
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);

        add(mainScrollPane, BorderLayout.CENTER);

        JPanel buttonPanel = createButtonPanel();
        buttonPanel.setMinimumSize(new Dimension(60, 60));
        add(buttonPanel, BorderLayout.SOUTH);
    }

    private JPanel createLanguagePanel(int stentWidth) {
        // language radios
        MultiBitTitledPanel languagePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "showPreferencesPanel.languageTitle"));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel indent = MultiBitTitledPanel.getIndentPanel(1);
        languagePanel.add(indent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth);
        languagePanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        languagePanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        ButtonGroup languageUsageGroup = new ButtonGroup();
        useDefaultLocale = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.useDefault"));
        useDefaultLocale.setOpaque(false);
        useDefaultLocale.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        JRadioButton useSpecific = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.useSpecific"));
        useSpecific.setOpaque(false);
        useSpecific.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        ItemListener itemListener = new ChangeLanguageUsageListener();
        useDefaultLocale.addItemListener(itemListener);
        useSpecific.addItemListener(itemListener);
        languageUsageGroup.add(useDefaultLocale);
        languageUsageGroup.add(useSpecific);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        languagePanel.add(useDefaultLocale, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        languagePanel.add(useSpecific, constraints);

        // language combo box
        int numberOfLanguages = Integer.parseInt(controller.getLocaliser().getString("showPreferencesPanel.numberOfLanguages"));

        // languages are added to the combo box in alphabetic order
        languageDataSet = new TreeSet<LanguageData>();

        for (int i = 0; i < numberOfLanguages; i++) {
            String languageCode = controller.getLocaliser().getString("showPreferencesPanel.languageCode." + (i + 1));
            String language = controller.getLocaliser().getString("showPreferencesPanel.language." + (i + 1));

            LanguageData languageData = new LanguageData();
            languageData.languageCode = languageCode;
            languageData.language = language;
            languageData.image = createImageIcon(languageCode);
            languageData.image.setDescription(language);
            languageDataSet.add(languageData);
        }

        Integer[] indexArray = new Integer[languageDataSet.size()];
        int index = 0;
        for (@SuppressWarnings("unused")
        LanguageData languageData : languageDataSet) {
            indexArray[index] = index;
            index++;
        }
        languageComboBox = new JComboBox(indexArray);
        languageComboBox.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        if (mainFrame.getApplication().isMac()) {
            languageComboBox.setUI(new MetalComboBoxUI());
        }
        languageComboBox.setOpaque(false);
        ComboBoxRenderer renderer = new ComboBoxRenderer();

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        Dimension preferredSize = new Dimension(fontMetrics.stringWidth(A_LONG_LANGUAGE_NAME) + LANGUAGE_COMBO_WIDTH_DELTA + LANGUAGE_CODE_IMAGE_WIDTH,
                fontMetrics.getHeight() + LANGUAGE_COMBO_HEIGHT_DELTA);
        renderer.setPreferredSize(preferredSize);

        languageComboBox.setRenderer(renderer);
        languageComboBox.setBackground(Color.WHITE);

        // get the languageCode value stored in the model
        String userLanguageCode = controller.getModel().getUserPreference(MultiBitModel.USER_LANGUAGE_CODE);
        if (userLanguageCode == null || MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            useDefaultLocale.setSelected(true);
            languageComboBox.setEnabled(false);
        } else {
            useSpecific.setSelected(true);
            int startingIndex = 0;
            Integer languageCodeIndex = 0;
            for (LanguageData languageData : languageDataSet) {
                if (languageData.languageCode.equals(userLanguageCode)) {
                    languageCodeIndex = startingIndex;
                    break;
                }
                startingIndex++;
            }
            if (languageCodeIndex != 0) {
                languageComboBox.setSelectedItem(languageCodeIndex.intValue());
                languageComboBox.setEnabled(true);
            }
        }

        // store original value for use by submit action
        Item languageItem = new Item(MultiBitModel.USER_LANGUAGE_CODE);
        languageItem.setOriginalValue(userLanguageCode);
        data.addItem(MultiBitModel.USER_LANGUAGE_CODE, languageItem);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 6;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        languagePanel.add(languageComboBox, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 6;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        languagePanel.add(fill1, constraints);

        return languagePanel;
    }

    private JPanel createFeePanel(int stentWidth) {
        MultiBitTitledPanel feePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "showPreferencesPanel.feeTitle"));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel indent = MultiBitTitledPanel.getIndentPanel(1);
        feePanel.add(indent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth);
        feePanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        feePanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        MultiBitLabel feeLabel = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.text"));
        feeLabel.setToolTipText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.tooltip"));
        MultiBitLabel feeCurrencyLabel = new MultiBitLabel("BTC");

        String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);

        if (sendFeeString == null || sendFeeString == "") {
            sendFeeString = controller.getLocaliser().bitcoinValueToString4(MultiBitModel.SEND_MINIMUM_FEE, false, false);
        }
        originalFee = sendFeeString;

        feeTextField = new MultiBitTextField("", 10, controller);
        feeTextField.setHorizontalAlignment(JLabel.TRAILING);
        feeTextField.setMinimumSize(new Dimension(FEE_TEXT_FIELD_WIDTH, FEE_TEXT_FIELD_HEIGHT));
        feeTextField.setPreferredSize(new Dimension(FEE_TEXT_FIELD_WIDTH, FEE_TEXT_FIELD_HEIGHT));
        feeTextField.setMaximumSize(new Dimension(FEE_TEXT_FIELD_WIDTH, FEE_TEXT_FIELD_HEIGHT));

        feeTextField.setText(sendFeeString);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        feePanel.add(feeLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        feePanel.add(feeTextField, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 4;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        feePanel.add(feeCurrencyLabel, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 5;
        constraints.gridy = 4;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        feePanel.add(fill1, constraints);

        return feePanel;
    }

    private JPanel createFontChooserPanel(int stentWidth) {
        MultiBitTitledPanel fontChooserPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "showPreferencesPanel.fontChooserTitle"));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel indent = MultiBitTitledPanel.getIndentPanel(1);
        fontChooserPanel.add(indent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth);
        fontChooserPanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        fontChooserPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        MultiBitLabel fontNameLabel = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontName"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        fontChooserPanel.add(fontNameLabel, constraints);

        fontNameTextLabel = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        fontChooserPanel.add(fontNameTextLabel, constraints);

        MultiBitLabel fontStyleLabel = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontStyle"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        fontChooserPanel.add(fontStyleLabel, constraints);

        fontStyleTextLabel = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        fontChooserPanel.add(fontStyleTextLabel, constraints);

        MultiBitLabel fontSizeLabel = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontSize"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        fontChooserPanel.add(fontSizeLabel, constraints);

        fontSizeTextLabel = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        fontChooserPanel.add(fontSizeTextLabel, constraints);

        ChooseFontAction chooseFontAction = new ChooseFontAction(controller, this, null);
        MultiBitButton fontChooserButton = new MultiBitButton(chooseFontAction, controller);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        fontChooserPanel.add(fontChooserButton, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 7;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        fontChooserPanel.add(fill1, constraints);
        
        return fontChooserPanel;
    }

    private JPanel createBrowserIntegrationPanel(int stentWidth) {
        MultiBitTitledPanel browserIntegrationPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "showPreferencesPanel.browserIntegrationTitle"));

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
                controller.getLocaliser().getString("showPreferencesPanel.browserIntegration.messageText"), 3, browserIntegrationPanel);

        ButtonGroup browserIntegrationGroup = new ButtonGroup();
        ignoreAll = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.ignoreAll"));
        ignoreAll.setOpaque(false);
        ignoreAll.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        fillAutomatically = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.fillAutomatically"));
        fillAutomatically.setOpaque(false);
        fillAutomatically.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        askEveryTime = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.askEveryTime"));
        askEveryTime.setOpaque(false);
        askEveryTime.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        browserIntegrationGroup.add(ignoreAll);
        browserIntegrationGroup.add(fillAutomatically);
        browserIntegrationGroup.add(askEveryTime);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        browserIntegrationPanel.add(ignoreAll, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        browserIntegrationPanel.add(fillAutomatically, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        browserIntegrationPanel.add(askEveryTime, constraints);

        return browserIntegrationPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();
 
        buttonPanel.setBorder(BorderFactory.createCompoundBorder( BorderFactory.createMatteBorder(1, 0, 0, 0, SystemColor.windowBorder),
                BorderFactory.createEmptyBorder(2, 0, 2, 0)
               ));
        buttonPanel.setOpaque(true);
        buttonPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        buttonPanel.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));       

        ShowPreferencesSubmitAction submitAction = new ShowPreferencesSubmitAction(controller, this, ImageLoader.createImageIcon(ImageLoader.PREFERENCES_ICON_FILE));
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        UndoPreferencesChangesSubmitAction undoChangesAction = new UndoPreferencesChangesSubmitAction(controller, this, ImageLoader.createImageIcon(ImageLoader.UNDO_ICON_FILE));
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

    class ChangeLanguageUsageListener implements ItemListener {
        public ChangeLanguageUsageListener() {

        }

        public void itemStateChanged(ItemEvent e) {
            if (e.getSource().equals(useDefaultLocale)) {
                languageComboBox.setEnabled(false);
            } else {
                languageComboBox.setEnabled(true);
            }
        }
    }

    private ImageIcon createImageIcon(String text) {
        Font font = new Font("Dialog", Font.PLAIN, LANGUAGE_CODE_IMAGE_HEIGHT - 2 * LANGUAGE_CODE_VERTICAL_INSET);

        BufferedImage bimg = new BufferedImage(LANGUAGE_CODE_IMAGE_WIDTH, LANGUAGE_CODE_IMAGE_HEIGHT, BufferedImage.TYPE_INT_RGB);
        Graphics2D g2 = bimg.createGraphics();

        g2.setColor(Color.WHITE);
        g2.setFont(font);
        g2.drawString(text, LANGUAGE_CODE_VERTICAL_INSET + 1, LANGUAGE_CODE_IMAGE_HEIGHT - 2 * LANGUAGE_CODE_VERTICAL_INSET);

        return new ImageIcon(bimg);
    }

    public Data getData() {
        Item previousUndoChangesTextItem = new Item(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT);
        previousUndoChangesTextItem.setOriginalValue(controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text"));
        data.addItem(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT, previousUndoChangesTextItem);
        
        Item languageItem = data.getItem(MultiBitModel.USER_LANGUAGE_CODE);
        if (useDefaultLocale.isSelected()) {
            languageItem.setNewValue(MultiBitModel.USER_LANGUAGE_IS_DEFAULT);
        } else {
            Integer selectedLanguageIndex = (Integer) languageComboBox.getSelectedItem();
            if (selectedLanguageIndex != null) {
                int loopIndex = 0;
                for (LanguageData languageData : languageDataSet) {
                    if (selectedLanguageIndex.intValue() == loopIndex) {
                        String newLanguageCode = languageData.languageCode;
                        languageItem.setNewValue(newLanguageCode);
                        break;
                    }
                    loopIndex++;
                }
            }
        }

        Item feeItem = new Item(MultiBitModel.SEND_FEE);
        feeItem.setOriginalValue(originalFee);
        feeItem.setNewValue(feeTextField.getText());
        data.addItem(MultiBitModel.SEND_FEE, feeItem);

        Item showDialogItem = new Item(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        showDialogItem.setOriginalValue(originalShowDialogString);
        showDialogItem.setNewValue((new Boolean((askEveryTime.isSelected()))).toString());
        data.addItem(MultiBitModel.OPEN_URI_SHOW_DIALOG, showDialogItem);

        Item useUriItem = new Item(MultiBitModel.OPEN_URI_USE_URI);
        boolean useUri = true;
        if (ignoreAll.isSelected()) {
            useUri = false;
        }
        useUriItem.setOriginalValue(originalUseUriString);
        useUriItem.setNewValue((new Boolean(useUri)).toString());
        data.addItem(MultiBitModel.OPEN_URI_USE_URI, useUriItem);

        // put in for convenience - not stored in user properties so no original
        Item fontItem = new Item(MultiBitModel.FONT);
        fontItem.setNewValue(selectedFont);
        data.addItem(MultiBitModel.FONT, fontItem);

        Item fontNameItem = new Item(MultiBitModel.FONT_NAME);
        fontNameItem.setOriginalValue(originalFontName);
        fontNameItem.setNewValue(selectedFont.getFamily());
        data.addItem(MultiBitModel.FONT_NAME, fontNameItem);

        Item fontStyleItem = new Item(MultiBitModel.FONT_STYLE);
        fontStyleItem.setOriginalValue(originalFontStyle);
        fontStyleItem.setNewValue("" + selectedFont.getStyle());
        data.addItem(MultiBitModel.FONT_STYLE, fontStyleItem);

        Item fontSizeItem = new Item(MultiBitModel.FONT_SIZE);
        fontSizeItem.setOriginalValue(originalFontSize);
        fontSizeItem.setNewValue("" + selectedFont.getSize());
        data.addItem(MultiBitModel.FONT_SIZE, fontSizeItem);

        return data;
    }

    class ComboBoxRenderer extends MultiBitLabel implements ListCellRenderer {
        private static final long serialVersionUID = -3301957214353702172L;

        public ComboBoxRenderer() {
            super("");
            setOpaque(true);
            setHorizontalAlignment(LEADING);
            setVerticalAlignment(CENTER);

            setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        }

        /*
         * This method finds the image and text corresponding to the selected
         * value and returns the label, set up to display the text and image.
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            int selectedIndex = 0;
            if (value != null) {
                selectedIndex = (Integer) value;
            }
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            // Set the icon and text. If icon was null, say so.
            int loopIndex = 0;
            for (LanguageData languageData : languageDataSet) {
                if (selectedIndex == loopIndex) {
                    ImageIcon icon = languageData.image;
                    String language = languageData.language;
                    setIcon(icon);
                    setText(language);
                    break;
                }
                loopIndex++;
            }

            setFont(list.getFont());

            return this;
        }
    }

    class LanguageData implements Comparable<LanguageData> {
        public String languageCode;
        public String language;
        public ImageIcon image;

        @Override
        public int compareTo(LanguageData other) {
            return languageCode.compareTo(other.languageCode);
        }
    }

    public void setSelectedFont(Font selectedFont) {
        this.selectedFont = selectedFont;

        fontNameTextLabel.setText(selectedFont.getFamily());
        fontSizeTextLabel.setText("" + selectedFont.getSize());
        setFontStyleText(selectedFont.getStyle());

        invalidate();
        validate();
        repaint();
    }

    private void setFontStyleText(int fontStyle) {
        switch (fontStyle) {
        case 0:
            fontStyleTextLabel.setText(controller.getLocaliser().getString("fontChooser.plain"));
            break;
        case 1:
            fontStyleTextLabel.setText(controller.getLocaliser().getString("fontChooser.bold"));
            break;
        case 2:
            fontStyleTextLabel.setText(controller.getLocaliser().getString("fontChooser.italic"));
            break;
        case 3:
            fontStyleTextLabel.setText(controller.getLocaliser().getString("fontChooser.boldItalic"));
            break;

        default:
            fontStyleTextLabel.setText("");
            break;
        }
    }

    public Font getSelectedFont() {
        return selectedFont;
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
    public int getViewId() {
        return View.PREFERENCES_VIEW;
    }
}