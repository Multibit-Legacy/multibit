/*
 * The MIT License
 *
 * Copyright 2013 Development.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.viewsystem.swing.preferences.modules;

import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.ListCellRenderer;
import static javax.swing.SwingConstants.CENTER;
import static javax.swing.SwingConstants.LEADING;
import javax.swing.UIManager;
import org.multibit.controller.core.CoreController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.CoreModel;
import org.multibit.viewsystem.dataproviders.core.CorePreferencesDataProvider;
import org.multibit.viewsystem.swing.core.ChooseFontAction;
import org.multibit.viewsystem.swing.core.ColorAndFontConstants;
import org.multibit.viewsystem.swing.core.components.FontSizer;
import org.multibit.viewsystem.swing.core.components.MultiBitButton;
import org.multibit.viewsystem.swing.core.components.MultiBitLabel;
import org.multibit.viewsystem.swing.core.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.preferences.AbstractPreferencesModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham <da2ce7@gmail.com>
 */
public class CorePreferencesModule extends AbstractPreferencesModule<CoreController> implements CorePreferencesDataProvider {

    private static final Logger log = LoggerFactory.getLogger(CorePreferencesModule.class);
    private final CorePreferencesModule corePreferencesModule = this;
    private final CorePreferencesPanels corePreferencesPanels = new CorePreferencesPanels();
    private Set<JPanel> jPanels = null;

    public CorePreferencesModule(CoreController coreController) {
        super(coreController);
    }

    @Override
    public Set<JPanel> Init() throws SetupNotCalledException {
        if (!super.setupHasBeenCalled) {
            throw new SetupNotCalledException("Core Init()");
        }

        if (jPanels != null) {
            return jPanels;
        } else {
            jPanels = new LinkedHashSet<JPanel>();
            jPanels.add(this.corePreferencesPanels.createAppearancePanel());
            jPanels.add(this.corePreferencesPanels.createBrowserIntegrationPanel());
            jPanels.add(this.corePreferencesPanels.createLanguagePanel());
            return jPanels;
        }
    }

    @Override
    public void Update() {

        String showDialogString = controller.getModel().getUserPreference(BitcoinModel.OPEN_URI_SHOW_DIALOG);
        String useUriString = controller.getModel().getUserPreference(BitcoinModel.OPEN_URI_USE_URI);

        if (!(Boolean.FALSE.toString().equalsIgnoreCase(showDialogString))) {
            // missing showDialog or it is set to true
            this.corePreferencesPanels.askEveryTime.setSelected(true);
        } else {
            if (!(Boolean.FALSE.toString().equalsIgnoreCase(useUriString))) {
                // missing useUri or it is set to true
                this.corePreferencesPanels.fillAutomatically.setSelected(true);
            } else {
                // useUri set to false
                this.corePreferencesPanels.ignoreAll.setSelected(true);
            }
        }


        String fontNameString = controller.getModel().getUserPreference(CoreModel.FONT_NAME);
        if (fontNameString == null || "".equals(fontNameString)) {
            fontNameString = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_NAME;
        }
        this.corePreferencesPanels.originalFontName = fontNameString;

        int fontStyle = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_STYLE;
        String fontStyleString = controller.getModel().getUserPreference(CoreModel.FONT_STYLE);
        if (fontStyleString != null && !"".equals(fontStyleString)) {
            try {
                fontStyle = Integer.parseInt(fontStyleString);
            } catch (NumberFormatException nfe) {
                // Use default.
            }
        }
        this.corePreferencesPanels.originalFontStyle = "" + fontStyle;

        int fontSize = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        String fontSizeString = controller.getModel().getUserPreference(CoreModel.FONT_SIZE);
        if (fontSizeString != null && !"".equals(fontSizeString)) {
            try {
                fontSize = Integer.parseInt(fontSizeString);
            } catch (NumberFormatException nfe) {
                // Use default.
            }
        }
        this.corePreferencesPanels.originalFontSize = "" + fontSize;

        setSelectedFont(new Font(fontNameString, fontStyle, fontSize));


    }

    @Override
    public String getPreviousUndoChangesText() {
        return controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text");
    }

    @Override
    public String getPreviousUserLanguageCode() {
        return this.corePreferencesPanels.originalUserLanguageCode;
    }

    @Override
    public String getNewUserLanguageCode() {
        if (this.corePreferencesPanels.useDefaultLocale.isSelected()) {
            return CoreModel.USER_LANGUAGE_IS_DEFAULT;
        } else {
            Integer selectedLanguageIndex = (Integer) this.corePreferencesPanels.languageComboBox.getSelectedItem();
            if (selectedLanguageIndex != null) {
                int loopIndex = 0;
                for (CorePreferencesPanels.LanguageData languageData : this.corePreferencesPanels.languageDataSet) {
                    if (selectedLanguageIndex.intValue() == loopIndex) {
                        String newLanguageCode = languageData.languageCode;
                        return newLanguageCode;
                    }
                    loopIndex++;
                }
            }
        }
        return null;
    }

    @Override
    public String getOpenUriDialog() {
        return (new Boolean((this.corePreferencesPanels.askEveryTime.isSelected()))).toString();
    }

    @Override
    public String getOpenUriUseUri() {
        boolean useUri = true;
        if (this.corePreferencesPanels.ignoreAll.isSelected()) {
            useUri = false;
        }
        return (new Boolean(useUri)).toString();
    }

    @Override
    public String getPreviousFontName() {
        return this.corePreferencesPanels.originalFontName;
    }

    @Override
    public String getNewFontName() {
        return this.corePreferencesPanels.selectedFont.getFamily();
    }

    @Override
    public String getPreviousFontStyle() {
        return this.corePreferencesPanels.originalFontStyle;
    }

    @Override
    public String getNewFontStyle() {
        return "" + this.corePreferencesPanels.selectedFont.getStyle();
    }

    @Override
    public String getPreviousFontSize() {
        return this.corePreferencesPanels.originalFontSize;
    }

    @Override
    public String getNewFontSize() {
        return "" + this.corePreferencesPanels.selectedFont.getSize();
    }

    @Override
    public Font getSelectedFont() {
        return this.corePreferencesPanels.selectedFont;
    }

    public void setSelectedFont(Font selectedFont) {
        this.corePreferencesPanels.setSelectedFont(selectedFont);

    }

    @Override
    public String getPreviousLookAndFeel() {
        return this.corePreferencesPanels.originalLookAndFeel;
    }

    @Override
    public String getNewLookAndFeel() {
        String lookAndFeel = (String) this.corePreferencesPanels.lookAndFeelComboBox.getSelectedItem();
        if (this.corePreferencesPanels.localisedSystemLookAndFeelName.equals(lookAndFeel)) {
            lookAndFeel = CoreModel.SYSTEM_LOOK_AND_FEEL;
        }
        return lookAndFeel;
    }

    private class CorePreferencesPanels {

        private static final String A_LONG_LANGUAGE_NAME = "LithuanianXY";
        private static final int LANGUAGE_COMBO_WIDTH_DELTA = 40;
        private static final int COMBO_HEIGHT_DELTA = 5;
        private static final int LANGUAGE_CODE_VERTICAL_INSET = 2;
        private static final int LANGUAGE_CODE_IMAGE_HEIGHT = 20;
        private static final int LANGUAGE_CODE_IMAGE_WIDTH = 26;
        SortedSet<LanguageData> languageDataSet;
        private JRadioButton useDefaultLocale;
        private JComboBox languageComboBox;
        private MultiBitLabel fontNameTextLabel;
        private MultiBitLabel fontStyleTextLabel;
        private MultiBitLabel fontSizeTextLabel;
        private String originalUserLanguageCode;
        private String originalLookAndFeel;
        private JComboBox lookAndFeelComboBox;
        private String localisedSystemLookAndFeelName;
        private JRadioButton ignoreAll;
        private JRadioButton fillAutomatically;
        private JRadioButton askEveryTime;
        
        private Font selectedFont;
        private String originalFontName;
        private String originalFontStyle;
        private String originalFontSize;

        private CorePreferencesPanels() {
            localisedSystemLookAndFeelName = controller.getLocaliser().getString("showPreferencesPanel.systemLookAndFeel");
        }

        private JPanel createLanguagePanel() {
            // language radios
            MultiBitTitledPanel languagePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                    "showPreferencesPanel.languageTitle"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

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
            JPanel stent = MultiBitTitledPanel.createStent(100);
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
            for (@SuppressWarnings("unused") LanguageData languageData : languageDataSet) {
                indexArray[index] = index;
                index++;
            }
            languageComboBox = new JComboBox(indexArray);
            languageComboBox.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            languageComboBox.setOpaque(false);
            LanguageComboBoxRenderer renderer = new LanguageComboBoxRenderer();

            FontMetrics fontMetrics = fontMetricsCallback.get(FontSizer.INSTANCE.getAdjustedDefaultFont());
            Dimension preferredSize = new Dimension(fontMetrics.stringWidth(A_LONG_LANGUAGE_NAME) + LANGUAGE_COMBO_WIDTH_DELTA
                    + LANGUAGE_CODE_IMAGE_WIDTH, fontMetrics.getHeight() + COMBO_HEIGHT_DELTA);
            renderer.setPreferredSize(preferredSize);

            languageComboBox.setRenderer(renderer);

            // get the languageCode value stored in the model
            String userLanguageCode = controller.getModel().getUserPreference(CoreModel.USER_LANGUAGE_CODE);
            if (userLanguageCode == null || CoreModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
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
            originalUserLanguageCode = userLanguageCode;

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

        private JPanel createAppearancePanel() {
            MultiBitTitledPanel appearancePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                    "showPreferencesPanel.appearanceTitle"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

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
            appearancePanel.add(indent, constraints);

            constraints.fill = GridBagConstraints.BOTH;
            constraints.gridx = 1;
            constraints.gridy = 4;
            constraints.weightx = 0.3;
            constraints.weighty = 0.3;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            JPanel stent = MultiBitTitledPanel.createStent(100);
            appearancePanel.add(stent, constraints);

            constraints.fill = GridBagConstraints.BOTH;
            constraints.gridx = 2;
            constraints.gridy = 3;
            constraints.weightx = 0.05;
            constraints.weighty = 0.3;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.CENTER;
            appearancePanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

            MultiBitLabel fontNameLabel = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontName"));
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            constraints.gridy = 4;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            appearancePanel.add(fontNameLabel, constraints);

            fontNameTextLabel = new MultiBitLabel("");
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 3;
            constraints.gridy = 4;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            appearancePanel.add(fontNameTextLabel, constraints);

            MultiBitLabel fontStyleLabel = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontStyle"));
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            constraints.gridy = 5;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            appearancePanel.add(fontStyleLabel, constraints);

            fontStyleTextLabel = new MultiBitLabel("");
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 3;
            constraints.gridy = 5;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            appearancePanel.add(fontStyleTextLabel, constraints);

            MultiBitLabel fontSizeLabel = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontSize"));
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            constraints.gridy = 6;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            appearancePanel.add(fontSizeLabel, constraints);

            fontSizeTextLabel = new MultiBitLabel("");
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 3;
            constraints.gridy = 6;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            appearancePanel.add(fontSizeTextLabel, constraints);

            ChooseFontAction chooseFontAction = new ChooseFontAction(controller, appearancePanel, corePreferencesModule, null);
            MultiBitButton fontChooserButton = new MultiBitButton(chooseFontAction, controller);

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 3;
            constraints.gridy = 7;
            constraints.weightx = 1;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            appearancePanel.add(fontChooserButton, constraints);

            constraints.fill = GridBagConstraints.VERTICAL;
            constraints.gridx = 4;
            constraints.gridy = 8;
            constraints.weightx = 0.2;
            constraints.weighty = 0.3;
            constraints.gridwidth = 3;
            constraints.anchor = GridBagConstraints.LINE_START;
            appearancePanel.add(MultiBitTitledPanel.createStent(1, 30), constraints);

            MultiBitLabel lookAndFeelLabel = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.lookAndFeel"));
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            constraints.gridy = 9;
            constraints.weightx = 0.3;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            appearancePanel.add(lookAndFeelLabel, constraints);

            originalLookAndFeel = controller.getModel().getUserPreference(CoreModel.LOOK_AND_FEEL);
            UIManager.LookAndFeelInfo[] lookAndFeels = UIManager.getInstalledLookAndFeels();

            lookAndFeelComboBox = new JComboBox();
            lookAndFeelComboBox.addItem(localisedSystemLookAndFeelName);
            if (lookAndFeels != null) {
                for (UIManager.LookAndFeelInfo info : lookAndFeels) {
                    lookAndFeelComboBox.addItem(info.getName());
                    if (info.getName().equalsIgnoreCase(originalLookAndFeel)) {
                        lookAndFeelComboBox.setSelectedItem(info.getName());
                    }
                }
            }

            if (originalLookAndFeel == null || originalLookAndFeel.equals("")
                    || CoreModel.SYSTEM_LOOK_AND_FEEL.equalsIgnoreCase(originalLookAndFeel)) {
                lookAndFeelComboBox.setSelectedItem(localisedSystemLookAndFeelName);
            }

            lookAndFeelComboBox.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            lookAndFeelComboBox.setOpaque(false);

            FontMetrics fontMetrics = fontMetricsCallback.get(FontSizer.INSTANCE.getAdjustedDefaultFont());
            int textWidth = Math.max(fontMetrics.stringWidth("CDE/Motif"), fontMetrics.stringWidth("Windows classic"));
//            Dimension preferredSize = new Dimension(textWidth + TICKER_COMBO_WIDTH_DELTA, fontMetrics.getHeight()
//                    + EXCHANGE_COMBO_HEIGHT_DELTA);

            Dimension preferredSize = new Dimension(textWidth + 100, fontMetrics.getHeight() + 100);

            lookAndFeelComboBox.setPreferredSize(preferredSize);

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 3;
            constraints.gridy = 9;
            constraints.weightx = 0.8;
            constraints.weighty = 0.6;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            appearancePanel.add(lookAndFeelComboBox, constraints);

            JPanel fill1 = new JPanel();
            fill1.setOpaque(false);
            constraints.fill = GridBagConstraints.BOTH;
            constraints.gridx = 4;
            constraints.gridy = 10;
            constraints.weightx = 20;
            constraints.weighty = 1;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            appearancePanel.add(fill1, constraints);

            return appearancePanel;
        }

        private JPanel createBrowserIntegrationPanel() {
            MultiBitTitledPanel browserIntegrationPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                    "showPreferencesPanel.browserIntegrationTitle"), ComponentOrientation.getOrientation(controller.getLocaliser()
                    .getLocale()));

            GridBagConstraints constraints = new GridBagConstraints();

            MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
                    controller.getLocaliser().getString("showPreferencesPanel.browserIntegration.messageText"), 3,
                    browserIntegrationPanel);

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

        public void setSelectedFont(Font selectedFont) {

            //log.debug("setSelectedFont called");
            this.selectedFont = selectedFont;

            fontNameTextLabel.setText(selectedFont.getFamily());
            fontSizeTextLabel.setText("" + selectedFont.getSize());
            setFontStyleText(selectedFont.getStyle());

            redrawCallback.redraw();
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

        class ChangeLanguageUsageListener implements ItemListener {

            public ChangeLanguageUsageListener() {
            }

            @Override
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

        class LanguageComboBoxRenderer extends MultiBitLabel implements ListCellRenderer {

            private static final long serialVersionUID = -3301957214353702172L;

            public LanguageComboBoxRenderer() {
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
            @Override
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
    }
}