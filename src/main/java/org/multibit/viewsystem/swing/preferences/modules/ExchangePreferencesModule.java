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
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.Set;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.exchange.ExchangeData;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.dataproviders.exchange.ExchangePreferencesDataProvider;
import org.multibit.viewsystem.swing.core.components.FontSizer;
import org.multibit.viewsystem.swing.core.components.MultiBitButton;
import org.multibit.viewsystem.swing.core.components.MultiBitLabel;
import org.multibit.viewsystem.swing.core.components.MultiBitTextField;
import org.multibit.viewsystem.swing.core.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.core.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.exchange.TickerTableModel;
import org.multibit.viewsystem.swing.preferences.AbstractPreferencesModule;
import org.multibit.viewsystem.swing.preferences.PreferencesPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham <da2ce7@gmail.com>
 */
public class ExchangePreferencesModule extends AbstractPreferencesModule<ExchangeController> implements ExchangePreferencesDataProvider {

    private static final Logger log = LoggerFactory.getLogger(ExchangePreferencesModule.class);
    private final ExchangePreferencesPanels exchangePreferencesPanels = new ExchangePreferencesPanels();

    public ExchangePreferencesModule(ExchangeController exchangeController) {
        super(exchangeController);
    }

    @Override
    public Set<JPanel> Init() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void Update() {

        this.exchangePreferencesPanels.originalShowTicker = !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW));
        this.exchangePreferencesPanels.showTicker.setSelected(this.exchangePreferencesPanels.originalShowTicker);

        this.exchangePreferencesPanels.originalShowBitcoinConvertedToFiat = !Boolean.FALSE.toString().equals(
                controller.getModel().getUserPreference(ExchangeModel.SHOW_BITCOIN_CONVERTED_TO_FIAT));
        this.exchangePreferencesPanels.showBitcoinConvertedToFiat.setSelected(this.exchangePreferencesPanels.originalShowBitcoinConvertedToFiat);

        this.exchangePreferencesPanels.originalOERApiCode = controller.getModel().getUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE);
        this.exchangePreferencesPanels.oerApiCodeTextField.setText(this.exchangePreferencesPanels.originalOERApiCode);
    }

    @Override
    public void Submit() throws ValidationException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void Undo() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean getPreviousShowCurrency() {
        return this.exchangePreferencesPanels.originalShowCurrency;
    }

    @Override
    public boolean getNewShowCurrency() {
        return this.exchangePreferencesPanels.showCurrency.isSelected();
    }

    @Override
    public boolean getPreviousShowRate() {
        return this.exchangePreferencesPanels.originalShowRate;
    }

    @Override
    public boolean getNewShowRate() {
        return this.exchangePreferencesPanels.showLastPrice.isSelected();
    }

    @Override
    public boolean getPreviousShowBid() {
        return this.exchangePreferencesPanels.originalShowBid;
    }

    @Override
    public boolean getNewShowBid() {
        return this.exchangePreferencesPanels.showBid.isSelected();
    }

    @Override
    public boolean getPreviousShowAsk() {
        return this.exchangePreferencesPanels.originalShowAsk;
    }

    @Override
    public boolean getNewShowAsk() {
        return this.exchangePreferencesPanels.showAsk.isSelected();
    }

    @Override
    public boolean getPreviousShowExchange() {
        return this.exchangePreferencesPanels.originalShowExchange;
    }

    @Override
    public boolean getNewShowExchange() {
        return this.exchangePreferencesPanels.showExchange.isSelected();
    }

    @Override
    public String getPreviousExchange1() {
        return this.exchangePreferencesPanels.originalExchange1;
    }

    @Override
    public String getNewExchange1() {
        return (String) this.exchangePreferencesPanels.exchangeComboBox1.getSelectedItem();
    }

    @Override
    public String getPreviousCurrency1() {
        return this.exchangePreferencesPanels.originalCurrency1;
    }

    @Override
    public String getNewCurrency1() {
        String currency = (String) this.exchangePreferencesPanels.currencyComboBox1.getSelectedItem();
        if (currency != null && currency.length() > 3) {
            currency = currency.substring(0, 3);
        }
        return currency;
    }

    @Override
    public boolean getPreviousShowSecondRow() {
        return this.exchangePreferencesPanels.originalShowSecondRow;
    }

    @Override
    public boolean getNewShowSecondRow() {
        return this.exchangePreferencesPanels.showSecondRowCheckBox.isSelected();
    }

    @Override
    public String getPreviousExchange2() {
        return this.exchangePreferencesPanels.originalExchange2;
    }

    @Override
    public String getNewExchange2() {
        return (String) this.exchangePreferencesPanels.exchangeComboBox2.getSelectedItem();
    }

    @Override
    public String getPreviousCurrency2() {
        return this.exchangePreferencesPanels.originalCurrency2;
    }

    @Override
    public String getNewCurrency2() {
        String currency = (String) this.exchangePreferencesPanels.currencyComboBox2.getSelectedItem();
        if (currency != null && currency.length() > 3) {
            currency = currency.substring(0, 3);
        }
        return currency;
    }

    @Override
    public boolean getPreviousShowTicker() {
        return this.exchangePreferencesPanels.originalShowTicker;
    }

    @Override
    public boolean getNewShowTicker() {
        return this.exchangePreferencesPanels.showTicker.isSelected();
    }

    @Override
    public boolean getNewShowBitcoinConvertedToFiat() {
        return this.exchangePreferencesPanels.showBitcoinConvertedToFiat.isSelected();
    }

    @Override
    public boolean getPreviousShowBitcoinConvertedToFiat() {
        return this.exchangePreferencesPanels.originalShowBitcoinConvertedToFiat;
    }

    @Override
    public String getNewOpenExchangeRatesApiCode() {
        return this.exchangePreferencesPanels.oerApiCodeTextField.getText();
    }

    @Override
    public String getPreviousOpenExchangeRatesApiCode() {
        return this.exchangePreferencesPanels.originalOERApiCode;
    }

    private class ExchangePreferencesPanels {

        private static final int EXCHANGE_COMBO_HEIGHT_DELTA = 15;
        private static final int COMBO_WIDTH_DELTA = 150;
        private static final int TICKER_COMBO_WIDTH_DELTA = 80;
        private static final int API_CODE_FIELD_HEIGHT = 30;
        private static final int API_CODE_FIELD_WIDTH = 200;
        public static final String OPEN_EXCHANGE_RATES_SIGN_UP_URI = "https://openexchangerates.org/signup/free?r=multibit";
        public static final int LENGTH_OF_OPEN_EXCHANGE_RATE_APP_ID = 32;
        private MultiBitLabel exchangeInformationLabel;
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
        private MultiBitLabel oerMessageLabel1, oerMessageLabel2;
        private JPanel oerStent;
        private MultiBitTextField oerApiCodeTextField;
        private String haveShownErrorMessageForApiCode;
        private MultiBitButton getOerAppIdButton;
        private MultiBitLabel oerApiCodeLabel;
        private String originalOERApiCode;

        private ExchangePreferencesPanels() {
            originalOERApiCode = controller.getModel().getUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE);
        }

        private JPanel createTickerPanel(int stentWidth) {
            // load up the original values
            originalShowTicker = !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW));
            originalExchange1 = controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE);
            originalCurrency1 = controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY);
            originalShowSecondRow = Boolean.TRUE.toString().equals(
                    controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW_SECOND_ROW));
            originalExchange2 = controller.getModel().getUserPreference(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE);
            originalCurrency2 = controller.getModel().getUserPreference(ExchangeModel.TICKER_SECOND_ROW_CURRENCY);

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
                showTickerText = Character.toUpperCase(showTickerText.charAt(0)) + showTickerText.toLowerCase().substring(1);
            }
            showTicker = new JCheckBox(showTickerText);
            showTicker.setOpaque(false);
            showTicker.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            showTicker.setSelected(originalShowTicker);

            exchangeInformationLabel = new MultiBitLabel(controller.getLocaliser().getString(
                    "showPreferencesPanel.ticker.exchangeInformation"));
            exchangeInformationLabel.setVisible(originalShowBitcoinConvertedToFiat);

            showBitcoinConvertedToFiat = new JCheckBox(controller.getLocaliser().getString(
                    "showPreferencesPanel.ticker.showBitcoinConvertedToFiat"));
            showBitcoinConvertedToFiat.setOpaque(false);
            showBitcoinConvertedToFiat.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            showBitcoinConvertedToFiat.setSelected(originalShowBitcoinConvertedToFiat);

            showBitcoinConvertedToFiat.addItemListener(new ItemListener() {
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
            });

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

            String tickerColumnsToShow = controller.getModel().getUserPreference(ExchangeModel.TICKER_COLUMNS_TO_SHOW);
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

            exchangeComboBox1 = new JComboBox(ExchangeData.getAvailableExchanges());
            exchangeComboBox1.setSelectedItem(exchangeToUse1);

            exchangeComboBox1.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            exchangeComboBox1.setOpaque(false);

            int textWidth = Math.max(fontMetrics.stringWidth(ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME), fontMetrics.stringWidth("USD"))
                    + COMBO_WIDTH_DELTA;
            Dimension preferredSize = new Dimension(textWidth + TICKER_COMBO_WIDTH_DELTA, fontMetrics.getHeight()
                    + EXCHANGE_COMBO_HEIGHT_DELTA);
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

            oerMessageLabel1 = new MultiBitLabel("    " + controller.getLocaliser().getString("showPreferencesPanel.getAppId.label"));
            oerMessageLabel1.setForeground(Color.GREEN.darker().darker());
            boolean showMessageLabel1 = isBrowserSupported() && ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeToUse1)
                    && (originalOERApiCode == null || originalOERApiCode.trim().length() == 0);
            oerMessageLabel1.setVisible(showMessageLabel1);
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 5;
            constraints.gridy = 15;
            constraints.weightx = 0.8;
            constraints.weighty = 0.6;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            tickerPanel.add(oerMessageLabel1, constraints);

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

            // Make sure the exchange1 has been created and initialised the list of
            // currencies.
            if (controller.getModel() != null && controller.getModel().getTickerTimerTask1() != null) {
                TickerTimerTask tickerTimerTask = controller.getModel().getTickerTimerTask1();
                synchronized (tickerTimerTask) {
                    if (tickerTimerTask.getExchange() == null) {
                        tickerTimerTask.createExchangeObjects(exchangeToUse1);
                    }
                }
            }

            currencyComboBox1 = new JComboBox();
            Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeToUse1);
            if (currenciesToUse != null) {
                for (String currency : currenciesToUse) {
                    String item = currency;
                    String description = CurrencyConverter.INSTANCE.getCurrencyCodeToDescriptionMap().get(currency);
                    if (description != null && description.trim().length() > 0) {
                        item = item + " (" + description + ")";
                    }
                    currencyComboBox1.addItem(item);
                    if (currency.equals(originalCurrency1)) {
                        currencyComboBox1.setSelectedItem(item);
                    }
                }
            }

            currencyComboBox1.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            currencyComboBox1.setOpaque(false);
            currencyComboBox1.setPreferredSize(preferredSize);

            exchangeComboBox1.addItemListener(new ItemListener() {
                @Override
                public void itemStateChanged(ItemEvent event) {
                    if (event.getStateChange() == ItemEvent.SELECTED) {
                        Object item = event.getItem();
                        String exchangeShortName = item.toString();
                        // Make sure the exchange1 has been created and initialised
                        // the list of currencies.
                        if (controller.getModel() != null && controller.getModel().getTickerTimerTask1() != null) {
                            TickerTimerTask tickerTimerTask = controller.getModel().getTickerTimerTask1();
                            synchronized (tickerTimerTask) {
                                tickerTimerTask.createExchangeObjects(exchangeShortName);
                                currencyComboBox1.removeAllItems();
                                Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeShortName);
                                if (currenciesToUse != null) {
                                    for (String currency : currenciesToUse) {
                                        String loopItem = currency;
                                        String description = CurrencyConverter.INSTANCE.getCurrencyCodeToDescriptionMap().get(currency);
                                        if (description != null && description.trim().length() > 0) {
                                            loopItem = loopItem + " (" + description + ")";
                                        }
                                        currencyComboBox1.addItem(loopItem);
                                    }
                                }
                            }
                        }

                        // Enable the OpenExchangeRates App ID if required
                        boolean showOER = ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeShortName)
                                || ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase((String) exchangeComboBox2.getSelectedItem());
                        oerStent.setVisible(showOER);
                        oerApiCodeLabel.setVisible(showOER);
                        oerApiCodeTextField.setVisible(showOER);
                        getOerAppIdButton.setVisible(showOER);

                        boolean showMessageLabel = isBrowserSupported() && ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeShortName)
                                && (oerApiCodeTextField.getText() == null || oerApiCodeTextField.getText().trim().length() == 0);
                        oerMessageLabel1.setVisible(showMessageLabel);
                    }
                }
            });

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
            constraints.gridwidth = 2;
            constraints.anchor = GridBagConstraints.LINE_START;
            tickerPanel.add(
                    MultiBitTitledPanel.createStent(fontMetrics.stringWidth(exchangeInformationLabel.getText()),
                    fontMetrics.getHeight()), constraints);
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

            exchangeComboBox2 = new JComboBox(ExchangeData.getAvailableExchanges());
            exchangeComboBox2.setSelectedItem(exchangeToUse2);

            exchangeComboBox2.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            exchangeComboBox2.setOpaque(false);
            exchangeComboBox2.setPreferredSize(preferredSize);

            exchangeComboBox2.addItemListener(new ItemListener() {
                @Override
                public void itemStateChanged(ItemEvent event) {
                    if (event.getStateChange() == ItemEvent.SELECTED) {
                        Object item = event.getItem();
                        String exchangeShortName = item.toString();
                        // Make sure the exchange2 has been created and initialised
                        // the list of currencies.
                        if (controller.getModel() != null && controller.getModel().getTickerTimerTask2() != null) {
                            TickerTimerTask tickerTimerTask = controller.getModel().getTickerTimerTask2();
                            synchronized (tickerTimerTask) {
                                tickerTimerTask.createExchangeObjects(exchangeShortName);
                                currencyComboBox2.removeAllItems();
                                Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeShortName);
                                if (currenciesToUse != null) {
                                    for (String currency : currenciesToUse) {
                                        String loopItem = currency;
                                        String description = CurrencyConverter.INSTANCE.getCurrencyCodeToDescriptionMap().get(currency);
                                        if (description != null && description.trim().length() > 0) {
                                            loopItem = loopItem + " (" + description + ")";
                                        }
                                        currencyComboBox2.addItem(loopItem);
                                    }
                                }
                            }
                        }

                        // Enable the OpenExchangeRates App ID if required
                        boolean showOER = ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeShortName)
                                || ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase((String) exchangeComboBox1.getSelectedItem());
                        oerStent.setVisible(showOER);
                        oerApiCodeLabel.setVisible(showOER);
                        oerApiCodeTextField.setVisible(showOER);
                        getOerAppIdButton.setVisible(showOER);

                        boolean showMessageLabel = isBrowserSupported() && ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeShortName)
                                && (oerApiCodeTextField.getText() == null || oerApiCodeTextField.getText().trim().length() == 0);
                        oerMessageLabel2.setVisible(showMessageLabel);
                    }
                }
            });

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 4;
            constraints.gridy = 22;
            constraints.weightx = 0.8;
            constraints.weighty = 0.6;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            tickerPanel.add(exchangeComboBox2, constraints);

            oerMessageLabel2 = new MultiBitLabel("    " + controller.getLocaliser().getString("showPreferencesPanel.getAppId.label"));
            oerMessageLabel2.setForeground(Color.GREEN.darker().darker());
            boolean showMessageLabel2 = isBrowserSupported() && ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeToUse2)
                    && (originalOERApiCode == null || originalOERApiCode.trim().length() == 0);
            oerMessageLabel2.setVisible(showMessageLabel2);
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 5;
            constraints.gridy = 22;
            constraints.weightx = 0.8;
            constraints.weighty = 0.6;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            tickerPanel.add(oerMessageLabel2, constraints);


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

            // Make sure the exchange2 has been created and initialised the list of
            // currencies.
            if (controller.getModel() != null && controller.getModel().getTickerTimerTask2() != null) {
                TickerTimerTask tickerTimerTask = controller.getModel().getTickerTimerTask2();
                synchronized (tickerTimerTask) {
                    if (tickerTimerTask.getExchange() == null) {
                        tickerTimerTask.createExchangeObjects(exchangeToUse2);
                    }
                }
            }
            currencyComboBox2 = new JComboBox();
            currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeToUse2);
            if (currenciesToUse != null) {
                for (String currency : currenciesToUse) {
                    String loopItem = currency;
                    String description = CurrencyConverter.INSTANCE.getCurrencyCodeToDescriptionMap().get(currency);
                    if (description != null && description.trim().length() > 0) {
                        loopItem = loopItem + " (" + description + ")";
                    }
                    currencyComboBox2.addItem(loopItem);
                    if (currency.equals(originalCurrency2)) {
                        currencyComboBox2.setSelectedItem(loopItem);
                    }
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

            boolean showOerSignup = ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeToUse1)
                    || ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equalsIgnoreCase(exchangeToUse2);

            oerStent = MultiBitTitledPanel.createStent(12, 12);
            constraints.fill = GridBagConstraints.BOTH;
            constraints.gridx = 1;
            constraints.gridy = 26;
            constraints.weightx = 0.3;
            constraints.weighty = 0.3;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            oerStent.setVisible(showOerSignup);
            tickerPanel.add(oerStent, constraints);

            oerApiCodeLabel = new MultiBitLabel(controller.getLocaliser().getString("showPreferencesPanel.oerLabel.text"));
            oerApiCodeLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showPreferencesPanel.oerLabel.tooltip")));
            oerApiCodeLabel.setVisible(showOerSignup);

            oerApiCodeTextField = new MultiBitTextField("", 25, controller);
            oerApiCodeTextField.setHorizontalAlignment(JLabel.LEADING);
            oerApiCodeTextField.setMinimumSize(new Dimension(API_CODE_FIELD_WIDTH, API_CODE_FIELD_HEIGHT));
            oerApiCodeTextField.setPreferredSize(new Dimension(API_CODE_FIELD_WIDTH, API_CODE_FIELD_HEIGHT));
            oerApiCodeTextField.setMaximumSize(new Dimension(API_CODE_FIELD_WIDTH, API_CODE_FIELD_HEIGHT));
            oerApiCodeTextField.setVisible(showOerSignup);
            oerApiCodeTextField.setText(originalOERApiCode);
            oerApiCodeTextField.addFocusListener(new FocusListener() {
                @Override
                public void focusGained(FocusEvent arg0) {
                }

                @Override
                public void focusLost(FocusEvent arg0) {
                    String apiCode = oerApiCodeTextField.getText();
                    if (apiCode != null && !(WhitespaceTrimmer.trim(apiCode).length() == 0)
                            && !apiCode.equals(controller.getModel().getUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE))) {
                        // New API code.
                        // Check its length
                        if (!(apiCode.trim().length() == LENGTH_OF_OPEN_EXCHANGE_RATE_APP_ID)
                                && !apiCode.equals(haveShownErrorMessageForApiCode)) {
                            haveShownErrorMessageForApiCode = apiCode;
                            // Give user a message that App ID is not in the correct format.
                            JOptionPane.showMessageDialog(null, new String[]{controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.text1"), " ",
                                controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.text2"),
                                controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.text3")},
                                    controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.title"),
                                    JOptionPane.ERROR_MESSAGE, ImageLoader.createImageIcon(ImageLoader.EXCLAMATION_MARK_ICON_FILE));
                            return;
                        }
                    }
                    updateApiCode();
                }
            });
            oerApiCodeTextField.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    String apiCode = oerApiCodeTextField.getText();
                    if (apiCode != null && !(WhitespaceTrimmer.trim(apiCode).length() == 0) && !apiCode.equals(controller.getModel().getUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE))) {
                        // New API code.
                        // Check its length
                        if (!(apiCode.trim().length() == LENGTH_OF_OPEN_EXCHANGE_RATE_APP_ID)
                                && !apiCode.equals(haveShownErrorMessageForApiCode)) {
                            haveShownErrorMessageForApiCode = apiCode;
                            // Give user a message that App ID is not in the correct format.
                            JOptionPane.showMessageDialog(null, new String[]{controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.text1"),
                                controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.text2"),
                                controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.text3")},
                                    controller.getLocaliser().getString("showPreferencesPanel.oerValidationError.title"),
                                    JOptionPane.ERROR_MESSAGE, ImageLoader.createImageIcon(ImageLoader.EXCLAMATION_MARK_ICON_FILE));
                            return;
                        }
                    }
                    updateApiCode();
                }
            });

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            constraints.gridy = 27;
            constraints.weightx = 0.3;
            constraints.weighty = 0.3;
            constraints.gridwidth = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            tickerPanel.add(oerApiCodeLabel, constraints);

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 4;
            constraints.gridy = 27;
            constraints.weightx = 0.3;
            constraints.weighty = 0.3;
            constraints.gridwidth = 2;
            constraints.anchor = GridBagConstraints.LINE_START;
            tickerPanel.add(oerApiCodeTextField, constraints);

            if (isBrowserSupported()) {
                getOerAppIdButton = new MultiBitButton(controller.getLocaliser().getString("showPreferencesPanel.getAppId.text"));
                getOerAppIdButton.setToolTipText(controller.getLocaliser().getString("showPreferencesPanel.getAppId.tooltip"));
                getOerAppIdButton.setVisible(showOerSignup);

                getOerAppIdButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent arg0) {
                        try {
                            openURI(new URI(OPEN_EXCHANGE_RATES_SIGN_UP_URI));
                        } catch (URISyntaxException e) {
                            log.debug(e.getMessage());
                        }
                    }
                });

                constraints.fill = GridBagConstraints.NONE;
                constraints.gridx = 4;
                constraints.gridy = 28;
                constraints.weightx = 1;
                constraints.weighty = 1;
                constraints.gridwidth = 1;
                constraints.anchor = GridBagConstraints.LINE_START;
                tickerPanel.add(getOerAppIdButton, constraints);
            }

            return tickerPanel;
        }

        private boolean isBrowserSupported() {
            if (!java.awt.Desktop.isDesktopSupported()) {
                return false;
            }

            java.awt.Desktop desktop = java.awt.Desktop.getDesktop();

            if (!desktop.isSupported(java.awt.Desktop.Action.BROWSE)) {
                return false;
            }

            return true;
        }

        private void openURI(URI uri) {
            try {
                java.awt.Desktop desktop = java.awt.Desktop.getDesktop();
                desktop.browse(uri);
            } catch (IOException ioe) {
                log.debug(ioe.getMessage());
                Message message = new Message("Cannot display URL '" + uri.toString() + "'. Error was '" + ioe.getMessage() + "'");
                MessageManager.INSTANCE.addMessage(message);
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
            exchangeLabel2.setEnabled(enableTickerSecondRow);
            exchangeComboBox2.setEnabled(enableTickerSecondRow);
            currencyLabel2.setEnabled(enableTickerSecondRow);
            currencyComboBox2.setEnabled(enableTickerSecondRow);
            oerMessageLabel2.setEnabled(enableTickerSecondRow);
        }

        private void updateApiCode() {
            String apiCode = oerApiCodeTextField.getText();
            if (apiCode != null && !(WhitespaceTrimmer.trim(apiCode).length() == 0) && !apiCode.equals(controller.getModel().getUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE))) {
                // New API code.
                apiCode = WhitespaceTrimmer.trim(apiCode);
                oerApiCodeTextField.setText(apiCode);

                controller.getModel().setUserPreference(ExchangeModel.OPEN_EXCHANGE_RATES_API_CODE, apiCode);
                if (ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equals((String) exchangeComboBox1.getSelectedItem())) {
                    if (controller.getModel() != null && controller.getModel().getTickerTimerTask1() != null) {
                        TickerTimerTask tickerTimerTask = controller.getModel().getTickerTimerTask1();
                        synchronized (tickerTimerTask) {
                            tickerTimerTask.createExchangeObjects(ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME);
                            currencyComboBox1.removeAllItems();
                            Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME);
                            if (currenciesToUse != null) {
                                for (String currency : currenciesToUse) {
                                    String loopItem = currency;
                                    String description = CurrencyConverter.INSTANCE.getCurrencyCodeToDescriptionMap().get(currency);
                                    if (description != null && description.trim().length() > 0) {
                                        loopItem = loopItem + " (" + description + ")";
                                    }
                                    currencyComboBox1.addItem(loopItem);
                                }
                            }
                        }
                    }
                    oerMessageLabel1.setVisible(false);
                }
                if (ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME.equals((String) exchangeComboBox2.getSelectedItem())) {
                    if (controller.getModel() != null && controller.getModel().getTickerTimerTask2() != null) {
                        TickerTimerTask tickerTimerTask = controller.getModel().getTickerTimerTask2();
                        synchronized (tickerTimerTask) {
                            tickerTimerTask.createExchangeObjects(ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME);
                            currencyComboBox2.removeAllItems();
                            Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(ExchangeData.OPEN_EXCHANGE_RATES_EXCHANGE_NAME);
                            if (currenciesToUse != null) {
                                for (String currency : currenciesToUse) {
                                    String loopItem = currency;
                                    String description = CurrencyConverter.INSTANCE.getCurrencyCodeToDescriptionMap().get(currency);
                                    if (description != null && description.trim().length() > 0) {
                                        loopItem = loopItem + " (" + description + ")";
                                    }
                                    currencyComboBox2.addItem(loopItem);
                                }
                            }
                        }
                    }
                    oerMessageLabel2.setVisible(false);
                }
            }
        }
    }
}