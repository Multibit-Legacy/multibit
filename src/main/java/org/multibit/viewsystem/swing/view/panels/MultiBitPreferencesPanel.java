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
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.dataproviders.MultiBitPreferencesDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.MultiBitPreferencesSubmitAction;
import org.multibit.viewsystem.swing.action.PreferencesSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

/**
 * The show preferences view.
 */
public class MultiBitPreferencesPanel implements MultiBitPreferencesDataProvider, PreferencePlugin {


    private static final long serialVersionUID = 191352298245057705L;

    private static final int EXCHANGE_COMBO_HEIGHT_DELTA = 15;

    private static final int FEE_TEXT_FIELD_HEIGHT = 30;
    private static final int FEE_TEXT_FIELD_WIDTH = 200;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    private PreferencesPanel preferencesPanel;


    private MultiBitTextField feeTextField;
    private String originalFee;
    
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
    private static final int TICKER_COMBO_WIDTH_DELTA = 80;
 

    private static final int STENT_DELTA = 0;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public MultiBitPreferencesPanel(MultiBitController controller, MultiBitFrame mainFrame, PreferencesPanel preferencesPanel) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.preferencesPanel = preferencesPanel;
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
    }

    @Override
    public void addToMainPanel(JPanel mainPanel) {

        String[] keys = new String[]{"showPreferencesPanel.feeLabel.text", "showPreferencesPanel.ticker.exchange", "showPreferencesPanel.ticker.currency"};
        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, mainPanel) + STENT_DELTA;

        {
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.fill = GridBagConstraints.HORIZONTAL;
            constraints.gridx = 0;
            constraints.gridy = 0;
            constraints.gridwidth = 2;
            constraints.weightx = 1;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
            mainPanel.add(createFeePanel(stentWidth), constraints);
        }
        {
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.fill = GridBagConstraints.HORIZONTAL;
            constraints.gridx = 0;
            constraints.gridy = 3;
            constraints.gridwidth = 2;
            constraints.weightx = 1;
            constraints.weighty = 1.6;
            constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
            mainPanel.add(createTickerPanel(stentWidth), constraints);
        }
    }
    
    @Override
    public PreferencesSubmitAction getPreferencesSubmitAction() {
        return new MultiBitPreferencesSubmitAction(controller, this, mainFrame);
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
        
        feeTextField = new MultiBitTextField("", 10);
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

        FontMetrics fontMetrics = this.preferencesPanel.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
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
}