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
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.dataproviders.PreferencesDataProvider;
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
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

/**
 * The show preferences view.
 */
public class ShowPreferencesPanel extends JPanel implements View, PreferencesDataProvider {

    private static final int LANGUAGE_CODE_VERTICAL_INSET = 2;

    private static final int LANGUAGE_CODE_IMAGE_HEIGHT = 20;

    private static final int LANGUAGE_CODE_IMAGE_WIDTH = 26;

    private static final long serialVersionUID = 191352298245057705L;

    private static final String A_LONG_LANGUAGE_NAME = "LithuanianXY";
    private static final int LANGUAGE_COMBO_WIDTH_DELTA = 40;
    private static final int COMBO_HEIGHT_DELTA = 5;

    private static final int EXCHANGE_COMBO_HEIGHT_DELTA = 15;

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
    
    private MultiBitLabel exchangeInformationLabel;

    private String originalFontName;
    private String originalFontStyle;
    private String originalFontSize;

    private MultiBitButton undoChangesButton;

    private String originalUserLanguageCode;

    private boolean originalShowTicker;
    private boolean originalShowBitcoinConvertedToFiat;
    private boolean originalShowCurrency;
    private boolean originalShowRate;
    private boolean originalShowBid;
    private boolean originalShowAsk;
    private boolean originalShowExchange;

    private JCheckBox showTicker;
    private JCheckBox showBitcoinConvertedToFiat;

    private JCheckBox showCurrency;
    private JCheckBox showLastPrice;
    private JCheckBox showBid;
    private JCheckBox showAsk;
    private JCheckBox showExchange;

    private String originalExchange1;
    private String originalCurrency1;
    private boolean originalShowSecondRow;
    private String originalExchange2;
    private String originalCurrency2;

    private JComboBox exchangeComboBox1;
    private JComboBox currencyComboBox1;
    private JCheckBox showSecondRowCheckBox;
    private MultiBitLabel exchangeLabel2;
    private MultiBitLabel currencyLabel2;
    private JComboBox exchangeComboBox2;
    private JComboBox currencyComboBox2;
    private static final int TICKER_COMBO_WIDTH_DELTA = 80;
    
    private String originalLookAndFeel;
    private JComboBox lookAndFeelComboBox;
    private String localisedSystemLookAndFeelName;
 
    private Font selectedFont;

    private static final int STENT_DELTA = 0;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public ShowPreferencesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.controller = controller;

        localisedSystemLookAndFeelName = controller.getLocaliser().getString("showPreferencesPanel.systemLookAndFeel");

        initUI();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * Update preferences panel.
     */
    @Override
    public void displayView() {
        originalShowTicker = !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW));
        showTicker.setSelected(originalShowTicker);
        
        originalShowBitcoinConvertedToFiat =  !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.SHOW_BITCOIN_CONVERTED_TO_FIAT));
        showBitcoinConvertedToFiat.setSelected(originalShowBitcoinConvertedToFiat);
        
        String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);

        if (sendFeeString == null || sendFeeString == "") {
            sendFeeString = controller.getLocaliser().bitcoinValueToStringNotLocalised(MultiBitModel.SEND_FEE_DEFAULT, false, false);
        }
        originalFee = sendFeeString;
        
        String sendFeeStringLocalised;
        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(sendFeeString);

        if (converterResult.isBtcMoneyValid()) {
            sendFeeStringLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(converterResult.getBtcMoney());
        } else {
            // BTC did not parse - just use the original text
            sendFeeStringLocalised = sendFeeString;
        }
        feeTextField.setText(sendFeeStringLocalised);

        String showDialogString = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        String useUriString = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_USE_URI);

        if (!(Boolean.FALSE.toString().equalsIgnoreCase(showDialogString))) {
            // missing showDialog or it is set to true
            askEveryTime.setSelected(true);
        } else {
            if (!(Boolean.FALSE.toString().equalsIgnoreCase(useUriString))) {
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
                // Use default.
            }
        }
        originalFontStyle = "" + fontStyle;

        int fontSize = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        if (fontSizeString != null && !"".equals(fontSizeString)) {
            try {
                fontSize = Integer.parseInt(fontSizeString);
            } catch (NumberFormatException nfe) {
                // Use default.
            }
        }
        originalFontSize = "" + fontSize;

        setSelectedFont(new Font(fontNameString, fontStyle, fontSize));

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

        String[] keys = new String[] { "showPreferencesPanel.feeLabel.text", "fontChooser.fontName", "fontChooser.fontStyle",
                "fontChooser.fontSize", "showPreferencesPanel.ticker.exchange", "showPreferencesPanel.ticker.currency", "showPreferencesPanel.lookAndFeel" };
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
        mainPanel.add(createAppearancePanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(createTickerPanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(createBrowserIntegrationPanel(stentWidth), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 100;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        mainPanel.add(filler1, constraints);

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        mainScrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);

        add(mainScrollPane, BorderLayout.CENTER);

        JPanel buttonPanel = createButtonPanel();
        buttonPanel.setMinimumSize(new Dimension(60, 60));
        add(buttonPanel, BorderLayout.SOUTH);
    }

    private JPanel createLanguagePanel(int stentWidth) {
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
        languageComboBox.setOpaque(false);
        LanguageComboBoxRenderer renderer = new LanguageComboBoxRenderer();

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        Dimension preferredSize = new Dimension(fontMetrics.stringWidth(A_LONG_LANGUAGE_NAME) + LANGUAGE_COMBO_WIDTH_DELTA
                + LANGUAGE_CODE_IMAGE_WIDTH, fontMetrics.getHeight() + COMBO_HEIGHT_DELTA);
        renderer.setPreferredSize(preferredSize);

        languageComboBox.setRenderer(renderer);

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

    private JPanel createFeePanel(int stentWidth) {
        MultiBitTitledPanel feePanel = new MultiBitTitledPanel(controller.getLocaliser().getString("showPreferencesPanel.feeTitle"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

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
            sendFeeString = controller.getLocaliser().bitcoinValueToString(MultiBitModel.SEND_FEE_DEFAULT, false, false);
        }
        originalFee = sendFeeString;

        String sendFeeStringLocalised;
        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(sendFeeString);

        if (converterResult.isBtcMoneyValid()) {
            sendFeeStringLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(converterResult.getBtcMoney());
        } else {
            // BTC did not parse - just use the original text
            sendFeeStringLocalised = sendFeeString;
        }
        
        feeTextField = new MultiBitTextField("", 10, controller);
        feeTextField.setHorizontalAlignment(JLabel.TRAILING);
        feeTextField.setMinimumSize(new Dimension(FEE_TEXT_FIELD_WIDTH, FEE_TEXT_FIELD_HEIGHT));
        feeTextField.setPreferredSize(new Dimension(FEE_TEXT_FIELD_WIDTH, FEE_TEXT_FIELD_HEIGHT));
        feeTextField.setMaximumSize(new Dimension(FEE_TEXT_FIELD_WIDTH, FEE_TEXT_FIELD_HEIGHT));

        feeTextField.setText(sendFeeStringLocalised);
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

    private JPanel createAppearancePanel(int stentWidth) {
        MultiBitTitledPanel appearancePanel= new MultiBitTitledPanel(controller.getLocaliser().getString(
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
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth);
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

        ChooseFontAction chooseFontAction = new ChooseFontAction(controller, this, null);
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
        appearancePanel.add(MultiBitTitledPanel.createStent(1,30), constraints);

        MultiBitLabel lookAndFeelLabel = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.lookAndFeel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 9;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        appearancePanel.add(lookAndFeelLabel, constraints);

        originalLookAndFeel = controller.getModel().getUserPreference(MultiBitModel.LOOK_AND_FEEL);
        LookAndFeelInfo[] lookAndFeels = UIManager.getInstalledLookAndFeels();

        lookAndFeelComboBox = new JComboBox();
        lookAndFeelComboBox.addItem(localisedSystemLookAndFeelName);
        if (lookAndFeels != null) {
            for (LookAndFeelInfo info : lookAndFeels) {
                lookAndFeelComboBox.addItem(info.getName());
                if (info.getName().equalsIgnoreCase(originalLookAndFeel)) {
                    lookAndFeelComboBox.setSelectedItem(info.getName());
                }
            }
        }
        
        if (originalLookAndFeel == null || originalLookAndFeel.equals("") || MultiBitModel.SYSTEM_LOOK_AND_FEEL.equalsIgnoreCase(originalLookAndFeel)) {
            lookAndFeelComboBox.setSelectedItem(localisedSystemLookAndFeelName);
        }
        
        lookAndFeelComboBox.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        lookAndFeelComboBox.setOpaque(false);

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int textWidth = Math.max(fontMetrics.stringWidth("CDE/Motif"), fontMetrics.stringWidth("Windows classic"));
        Dimension preferredSize = new Dimension(textWidth + TICKER_COMBO_WIDTH_DELTA, fontMetrics.getHeight() + EXCHANGE_COMBO_HEIGHT_DELTA);
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

    private JPanel createTickerPanel(int stentWidth) {
        // load up the original values
        originalShowTicker = !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW));
        originalExchange1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_EXCHANGE);
        originalCurrency1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY);
        originalShowSecondRow = Boolean.TRUE.toString().equals(
                controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW_SECOND_ROW));
        originalExchange2 = controller.getModel().getUserPreference(MultiBitModel.TICKER_SECOND_ROW_EXCHANGE);
        originalCurrency2 = controller.getModel().getUserPreference(MultiBitModel.TICKER_SECOND_ROW_CURRENCY);

        MultiBitTitledPanel tickerPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                "showPreferencesPanel.ticker.title2"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        tickerPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        String showTickerText = controller.getLocaliser().getString("multiBitFrame.ticker.show.text");
        if (showTickerText != null && showTickerText.length() >= 1) {
            // Capitalise text (this is to save adding a new I18n term.
            showTickerText =  Character.toUpperCase(showTickerText.charAt(0)) + showTickerText.toLowerCase().substring(1);
        }
        showTicker = new JCheckBox(showTickerText);
        showTicker.setOpaque(false);
        showTicker.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        showTicker.setSelected(originalShowTicker);
        
        exchangeInformationLabel = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.ticker.exchangeInformation"));
        exchangeInformationLabel.setVisible(originalShowBitcoinConvertedToFiat);
        
        showBitcoinConvertedToFiat = new JCheckBox(controller.getLocaliser().getString("showPreferencesPanel.ticker.showBitcoinConvertedToFiat"));
        showBitcoinConvertedToFiat.setOpaque(false);
        showBitcoinConvertedToFiat.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        showBitcoinConvertedToFiat.setSelected(originalShowBitcoinConvertedToFiat);

        showBitcoinConvertedToFiat.addItemListener(
                new ItemListener() {
            @Override
                    public void itemStateChanged(ItemEvent e) {
                        boolean selectedChange = (e.getStateChange() == ItemEvent.SELECTED);
                        boolean unSelectedChange = (e.getStateChange() == ItemEvent.DESELECTED);
                        if (exchangeInformationLabel != null) {
                            if (selectedChange) {
                                exchangeInformationLabel.setVisible(true);
                            }
                            if (unSelectedChange) {
                                exchangeInformationLabel.setVisible(false);
                            }
                        }
                    }
                }
            );
         
        showExchange = new JCheckBox(controller.getLocaliser().getString("tickerTableModel.exchange"));
        showExchange.setOpaque(false);
        showExchange.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        showCurrency = new JCheckBox(controller.getLocaliser().getString("tickerTableModel.currency"));
        showCurrency.setOpaque(false);
        showCurrency.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        showLastPrice = new JCheckBox(controller.getLocaliser().getString("tickerTableModel.lastPrice"));
        showLastPrice.setOpaque(false);
        showLastPrice.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        showBid = new JCheckBox(controller.getLocaliser().getString("tickerTableModel.bid"));
        showBid.setOpaque(false);
        showBid.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        showAsk = new JCheckBox(controller.getLocaliser().getString("tickerTableModel.ask"));
        showAsk.setOpaque(false);
        showAsk.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        String tickerColumnsToShow = controller.getModel().getUserPreference(MultiBitModel.TICKER_COLUMNS_TO_SHOW);
        if (tickerColumnsToShow == null || tickerColumnsToShow.equals("")) {
            tickerColumnsToShow = TickerTableModel.DEFAULT_COLUMNS_TO_SHOW;
        }

        originalShowCurrency = tickerColumnsToShow.indexOf(TickerTableModel.TICKER_COLUMN_CURRENCY) > -1;
        showCurrency.setSelected(originalShowCurrency);

        originalShowRate = tickerColumnsToShow.indexOf(TickerTableModel.TICKER_COLUMN_LAST_PRICE) > -1;
        showLastPrice.setSelected(originalShowRate);

        originalShowBid = tickerColumnsToShow.indexOf(TickerTableModel.TICKER_COLUMN_BID) > -1;
        showBid.setSelected(originalShowBid);

        originalShowAsk = tickerColumnsToShow.indexOf(TickerTableModel.TICKER_COLUMN_ASK) > -1;
        showAsk.setSelected(originalShowAsk);

        originalShowExchange = tickerColumnsToShow.indexOf(TickerTableModel.TICKER_COLUMN_EXCHANGE) > -1;
        showExchange.setSelected(originalShowExchange);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showTicker, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showBitcoinConvertedToFiat, constraints);

        constraints.fill = GridBagConstraints.VERTICAL;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(MultiBitTitledPanel.createStent(1, 12), constraints);

        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
                controller.getLocaliser().getString("showPreferencesPanel.ticker.columnsToShow"), 7, tickerPanel);
        
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 8;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showExchange, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 9;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showCurrency, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 10;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showLastPrice, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 11;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showBid, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 12;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showAsk, constraints);

        constraints.fill = GridBagConstraints.VERTICAL;
        constraints.gridx = 1;
        constraints.gridy = 13;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(MultiBitTitledPanel.createStent(1, 13), constraints);

        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(controller.getLocaliser()
                .getString("showPreferencesPanel.ticker.firstRow"), 14, tickerPanel);

        MultiBitLabel exchangeLabel1 = new MultiBitLabel(controller.getLocaliser()
                .getString("showPreferencesPanel.ticker.exchange"));
        exchangeLabel1.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 15;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        tickerPanel.add(exchangeLabel1, constraints);

        String exchangeToUse1;
        if (originalExchange1 == null | "".equals(originalExchange1)) {
            exchangeToUse1 = ExchangeData.DEFAULT_EXCHANGE;
        } else {
            exchangeToUse1 = originalExchange1;
        }

        String exchangeToUse2;
        if (originalExchange2 == null | "".equals(originalExchange2)) {
            exchangeToUse2 = ExchangeData.DEFAULT_EXCHANGE;
        } else {
            exchangeToUse2 = originalExchange2;
        }

        exchangeComboBox1 = new JComboBox(controller.getModel().getExchangeData().getAvailableExchanges());
        exchangeComboBox1.setSelectedItem(exchangeToUse1);

        exchangeComboBox1.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        exchangeComboBox1.setOpaque(false);

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int textWidth = Math.max(fontMetrics.stringWidth(ExchangeData.MT_GOX_EXCHANGE_NAME), fontMetrics.stringWidth("USD"));
        Dimension preferredSize = new Dimension(textWidth + TICKER_COMBO_WIDTH_DELTA, fontMetrics.getHeight() + EXCHANGE_COMBO_HEIGHT_DELTA);
        exchangeComboBox1.setPreferredSize(preferredSize);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 15;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth);
        tickerPanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 15;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(exchangeComboBox1, constraints);

        MultiBitLabel currencyLabel1 = new MultiBitLabel(controller.getLocaliser()
                .getString("showPreferencesPanel.ticker.currency"));
        currencyLabel1.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 16;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        tickerPanel.add(currencyLabel1, constraints);
        
        // Make sure the exchange has initialised the list of currencies.
        if (mainFrame != null && mainFrame.getTickerTimerTask() != null) {
            TickerTimerTask tickerTimerTask = mainFrame.getTickerTimerTask();
            synchronized(tickerTimerTask) {
                if (tickerTimerTask.getMtGox() == null) {
                    tickerTimerTask.createExchange();
                }
            }  
        }
        
        currencyComboBox1 = new JComboBox(controller.getModel().getExchangeData()
                .getAvailableCurrenciesForExchange(exchangeToUse1));
        if (originalCurrency1 == null | "".equals(originalCurrency1)) {
            currencyComboBox1.setSelectedItem(ExchangeData.DEFAULT_CURRENCY);
        } else {
            currencyComboBox1.setSelectedItem(originalCurrency1);
            // The currency may have disappeared if the exchange has removed it.
            // Add it back in, otherwise currency choice is lost.
            if (!originalCurrency1.equals(((String)currencyComboBox1.getSelectedItem()))) {
                currencyComboBox1.addItem(originalCurrency1);
                currencyComboBox1.setSelectedItem(originalCurrency1);
            }
        }
        currencyComboBox1.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        currencyComboBox1.setOpaque(false);
        currencyComboBox1.setPreferredSize(preferredSize);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 16;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(currencyComboBox1, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 17;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 18;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(MultiBitTitledPanel.createStent(fontMetrics.stringWidth(exchangeInformationLabel.getText()), fontMetrics.getHeight()), constraints);
        tickerPanel.add(exchangeInformationLabel, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 19;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);


        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
                controller.getLocaliser().getString("showPreferencesPanel.ticker.secondRow"), 20, tickerPanel);

        showSecondRowCheckBox = new JCheckBox(controller.getLocaliser().getString("showPreferencesPanel.ticker.showSecondRow"));
        showSecondRowCheckBox.setOpaque(false);
        showSecondRowCheckBox.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        showSecondRowCheckBox.addItemListener(new ChangeTickerShowSecondRowListener());

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 21;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(showSecondRowCheckBox, constraints);

        exchangeLabel2 = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.ticker.exchange"));
        exchangeLabel2.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 22;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        tickerPanel.add(exchangeLabel2, constraints);

        exchangeComboBox2 = new JComboBox(controller.getModel().getExchangeData().getAvailableExchanges());
        exchangeComboBox2.setSelectedItem(exchangeToUse2);

        exchangeComboBox2.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        exchangeComboBox2.setOpaque(false);
        exchangeComboBox2.setPreferredSize(preferredSize);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 22;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(exchangeComboBox2, constraints);

        currencyLabel2 = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.ticker.currency"));
        currencyLabel2.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 23;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        tickerPanel.add(currencyLabel2, constraints);

        currencyComboBox2 = new JComboBox(controller.getModel().getExchangeData()
                .getAvailableCurrenciesForExchange(exchangeToUse2));
        if (originalCurrency2 == null | "".equals(originalCurrency2)) {
            currencyComboBox2.setSelectedItem(ExchangeData.DEFAULT_CURRENCY);
        } else {
            currencyComboBox2.setSelectedItem(originalCurrency2);
            // The currency may have disappeared if the exchange has removed it.
            // Add it back in, otherwise currency choice is lost.
            if (!originalCurrency2.equals(((String)currencyComboBox2.getSelectedItem()))) {
                currencyComboBox2.addItem(originalCurrency2);
                currencyComboBox2.setSelectedItem(originalCurrency2);
            }
        }

        currencyComboBox2.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        currencyComboBox2.setOpaque(false);
        currencyComboBox2.setPreferredSize(preferredSize);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 23;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(currencyComboBox2, constraints);

        showSecondRowCheckBox.setSelected(originalShowSecondRow);
        enableTickerSecondRow(originalShowSecondRow);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 24;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        tickerPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 5;
        constraints.gridy = 25;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        tickerPanel.add(fill1, constraints);

        return tickerPanel;
    }

    private JPanel createBrowserIntegrationPanel(int stentWidth) {
        MultiBitTitledPanel browserIntegrationPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                "showPreferencesPanel.browserIntegrationTitle"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

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

        ShowPreferencesSubmitAction submitAction = new ShowPreferencesSubmitAction(controller, this,
                ImageLoader.createImageIcon(ImageLoader.PREFERENCES_ICON_FILE), mainFrame);
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        UndoPreferencesChangesSubmitAction undoChangesAction = new UndoPreferencesChangesSubmitAction(controller,
                ImageLoader.createImageIcon(ImageLoader.UNDO_ICON_FILE));
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

        @Override
        public void itemStateChanged(ItemEvent e) {
            if (e.getSource().equals(useDefaultLocale)) {
                languageComboBox.setEnabled(false);
            } else {
                languageComboBox.setEnabled(true);
            }
        }
    }

    class ChangeTickerShowSecondRowListener implements ItemListener {
        public ChangeTickerShowSecondRowListener() {

        }

        @Override
        public void itemStateChanged(ItemEvent e) {
            enableTickerSecondRow(showSecondRowCheckBox.isSelected());
        }
    }

    private void enableTickerSecondRow(boolean enableTickerSecondRow) {
        if (enableTickerSecondRow) {
            exchangeLabel2.setEnabled(true);
            exchangeComboBox2.setEnabled(true);
            currencyLabel2.setEnabled(true);
            currencyComboBox2.setEnabled(true);
        } else {
            exchangeLabel2.setEnabled(false);
            exchangeComboBox2.setEnabled(false);
            currencyLabel2.setEnabled(false);
            currencyComboBox2.setEnabled(false);
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

    // PreferencesDataProvider methods
    @Override
    public String getPreviousUndoChangesText() {
        return controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text");
    }

    @Override
    public String getPreviousSendFee() {
        return originalFee;
    }

    @Override
    public String getNewSendFee() {
        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(feeTextField.getText());
        if (converterResult.isBtcMoneyValid()) {
            return controller.getLocaliser().bitcoinValueToStringNotLocalised(converterResult.getBtcMoney().getAmount().toBigInteger(), false, false);
        } else {
            // Return the unparsable fee for the action to deal with it.
            return feeTextField.getText();
        }
    }

    @Override
    public String getPreviousUserLanguageCode() {
        return originalUserLanguageCode;
    }

    @Override
    public String getNewUserLanguageCode() {
        if (useDefaultLocale.isSelected()) {
            return MultiBitModel.USER_LANGUAGE_IS_DEFAULT;
        } else {
            Integer selectedLanguageIndex = (Integer) languageComboBox.getSelectedItem();
            if (selectedLanguageIndex != null) {
                int loopIndex = 0;
                for (LanguageData languageData : languageDataSet) {
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
        return (new Boolean((askEveryTime.isSelected()))).toString();
    }

    @Override
    public String getOpenUriUseUri() {
        boolean useUri = true;
        if (ignoreAll.isSelected()) {
            useUri = false;
        }
        return (new Boolean(useUri)).toString();
    }

    @Override
    public String getPreviousFontName() {
        return originalFontName;
    }

    @Override
    public String getNewFontName() {
        return selectedFont.getFamily();
    }

    @Override
    public String getPreviousFontStyle() {
        return originalFontStyle;
    }

    @Override
    public String getNewFontStyle() {
        return "" + selectedFont.getStyle();
    }

    @Override
    public String getPreviousFontSize() {
        return originalFontSize;
    }

    @Override
    public String getNewFontSize() {
        return "" + selectedFont.getSize();
    }

    @Override
    public Font getSelectedFont() {
        return selectedFont;
    }

    @Override
    public boolean getPreviousShowRate() {
        return originalShowRate;
    }

    @Override
    public boolean getNewShowRate() {
        return showLastPrice.isSelected();
    }

    @Override
    public String getPreviousExchange1() {
        return originalExchange1;
    }

    @Override
    public String getNewExchange1() {
        return (String) exchangeComboBox1.getSelectedItem();
    }

    @Override
    public String getPreviousCurrency1() {
        return originalCurrency1;
    }

    @Override
    public String getNewCurrency1() {
        return (String) currencyComboBox1.getSelectedItem();
    }

    @Override
    public boolean getPreviousShowSecondRow() {
        return originalShowSecondRow;
    }

    @Override
    public boolean getNewShowSecondRow() {
        return showSecondRowCheckBox.isSelected();
    }

    @Override
    public String getPreviousExchange2() {
        return originalExchange2;
    }

    @Override
    public String getNewExchange2() {
        return (String) exchangeComboBox2.getSelectedItem();
    }

    @Override
    public String getPreviousCurrency2() {
        return originalCurrency2;
    }

    @Override
    public String getNewCurrency2() {
        return (String) currencyComboBox2.getSelectedItem();
    }

    @Override
    public boolean getPreviousShowCurrency() {
        return originalShowCurrency;
    }

    @Override
    public boolean getNewShowCurrency() {
        return showCurrency.isSelected();
    }

    @Override
    public boolean getPreviousShowTicker() {
        return originalShowTicker;
    }

    @Override
    public boolean getNewShowTicker() {
        return showTicker.isSelected();
    }

    @Override
    public boolean getPreviousShowBitcoinConvertedToFiat() {
        return originalShowBitcoinConvertedToFiat;
    }

    @Override
    public boolean getNewShowBitcoinConvertedToFiat() {
        return showBitcoinConvertedToFiat.isSelected();
    }

    @Override
    public boolean isTickerVisible() {
        return mainFrame.getTickerTablePanel().isVisible();
    }
    
    @Override
    public boolean getPreviousShowBid() {
        return originalShowBid;
    }

    @Override
    public boolean getNewShowBid() {
        return showBid.isSelected();
    }

    @Override
    public boolean getPreviousShowAsk() {
        return originalShowAsk;
    }

    @Override
    public boolean getNewShowAsk() {
        return showAsk.isSelected();
    }

    @Override
    public boolean getPreviousShowExchange() {
        return originalShowExchange;
    }

    @Override
    public boolean getNewShowExchange() {
        return showExchange.isSelected();
    }
    
    @Override
    public String getPreviousLookAndFeel() {
        return originalLookAndFeel;
    }
    
    @Override
    public String getNewLookAndFeel() {
        String lookAndFeel = (String)lookAndFeelComboBox.getSelectedItem();
        if (localisedSystemLookAndFeelName.equals(lookAndFeel)) {
            lookAndFeel = MultiBitModel.SYSTEM_LOOK_AND_FEEL;
        }
        return lookAndFeel;
    }
}