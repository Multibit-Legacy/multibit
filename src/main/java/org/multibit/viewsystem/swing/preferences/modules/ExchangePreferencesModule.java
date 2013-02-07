/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham.
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

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Timer;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import org.joda.money.CurrencyUnit;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.dataproviders.preferences.TickerPreferencesDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

/**
 *
 * @author Cameron Garnham
 */
public class ExchangePreferencesModule  extends AbstractPreferencesModule implements TickerPreferencesDataProvider {

    
    private static final int EXCHANGE_COMBO_HEIGHT_DELTA = 15;
    private static final int COMBO_WIDTH_DELTA = 50;
    
    private static final int TICKER_COMBO_WIDTH_DELTA = 80;
    
    
    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    
    
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
    
    private MultiBitLabel exchangeInformationLabel;

    private JComboBox exchangeComboBox1;
    private JComboBox currencyComboBox1;
    private JCheckBox showSecondRowCheckBox;
    private MultiBitLabel exchangeLabel2;
    private MultiBitLabel currencyLabel2;
    private JComboBox exchangeComboBox2;
    private JComboBox currencyComboBox2;

    
     public ExchangePreferencesModule(MultiBitController controller, MultiBitFrame mainFrame){
        this.controller = controller;
        this.mainFrame = mainFrame;
     }
    
    

    @Override
    public void displayView() {
        if (super.getIsInitialised()) {
        
        originalShowTicker = !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW));
        showTicker.setSelected(originalShowTicker);

        originalShowBitcoinConvertedToFiat = !Boolean.FALSE.toString().equals(
                controller.getModel().getUserPreference(MultiBitModel.SHOW_BITCOIN_CONVERTED_TO_FIAT));
        showBitcoinConvertedToFiat.setSelected(originalShowBitcoinConvertedToFiat);
        }
    }
    
    
    @Override
    public List<JPanel> addPanels() {
        List<JPanel> panels = new ArrayList<JPanel>();
        
        panels.add(createTickerPanel(0));
        
        return panels;
    }

    @Override
    public PreferencesModule getModuleEnum() {
        return PreferencesModule.EXCHANGE;
    }

    
    @Override
    public void onSubmitAction() {

        boolean wantToFireDataStructureChanged = false;
        
        

 
            // Currency ticker.
            boolean showTicker = this.getNewShowTicker();
            boolean showBitcoinConvertedToFiat = this.getNewShowBitcoinConvertedToFiat();
            boolean showCurrency = this.getNewShowCurrency();
            boolean showRate = this.getNewShowRate();
            boolean showBid = this.getNewShowBid();
            boolean showAsk = this.getNewShowAsk();
            boolean showExchange = this.getNewShowExchange();

            boolean restartTickerTimer = false;

            if (this.getPreviousShowCurrency() != showCurrency) {
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            } else if (this.getPreviousShowBitcoinConvertedToFiat() != showBitcoinConvertedToFiat) {
                wantToFireDataStructureChanged = true;
            } else if (this.getPreviousShowTicker() != showTicker || showTicker != this.isTickerVisible()) {
                wantToFireDataStructureChanged = true;
            } else if (this.getPreviousShowRate() != showRate) {
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            } else if (this.getPreviousShowBid() != showBid) {
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            } else if (this.getPreviousShowAsk() != showAsk) {
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            } else if (this.getPreviousShowExchange() != showExchange) {
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW, new Boolean(showTicker).toString());
            controller.getModel().setUserPreference(MultiBitModel.SHOW_BITCOIN_CONVERTED_TO_FIAT,
                    new Boolean(showBitcoinConvertedToFiat).toString());

            String columnsToShow = "";
            if (showCurrency)
                columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_CURRENCY;
            if (showRate)
                columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_LAST_PRICE;
            if (showBid)
                columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_BID;
            if (showAsk)
                columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_ASK;
            if (showExchange)
                columnsToShow = columnsToShow + " " + TickerTableModel.TICKER_COLUMN_EXCHANGE;

            if ("".equals(columnsToShow)) {
                // A user could just switch all the columns off in the settings
                // so
                // put a 'none' in the list of columns
                // this is to stop the default columns appearing.
                columnsToShow = TickerTableModel.TICKER_COLUMN_NONE;
            }
            controller.getModel().setUserPreference(MultiBitModel.TICKER_COLUMNS_TO_SHOW, columnsToShow);

            String previousExchange1 = this.getPreviousExchange1();
            String newExchange1 = this.getNewExchange1();
            if (newExchange1 != null && !newExchange1.equals(previousExchange1)) {
                controller.getModel().setUserPreference(MultiBitModel.TICKER_FIRST_ROW_EXCHANGE, newExchange1);
                ExchangeData newExchangeData = new ExchangeData();
                newExchangeData.setShortExchangeName(newExchange1);
                controller.getModel().setExchangeData1(newExchangeData);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousCurrency1 = this.getPreviousCurrency1();
            String newCurrency1 = this.getNewCurrency1();
            if (newCurrency1 != null && !newCurrency1.equals(previousCurrency1)) {
                controller.getModel().setUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY, newCurrency1);
                CurrencyConverter.INSTANCE.setCurrencyUnit(CurrencyUnit.of(newCurrency1));
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousShowSecondRow = new Boolean(this.getPreviousShowSecondRow()).toString();
            String newShowSecondRow = new Boolean(this.getNewShowSecondRow()).toString();
            if (newShowSecondRow != null && !newShowSecondRow.equals(previousShowSecondRow)) {
                // New show second row is set on model.
                controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW_SECOND_ROW, newShowSecondRow);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousExchange2 = this.getPreviousExchange2();
            String newExchange2 = this.getNewExchange2();
            if (newExchange2 != null && !newExchange2.equals(previousExchange2)) {
                controller.getModel().setUserPreference(MultiBitModel.TICKER_SECOND_ROW_EXCHANGE, newExchange2);
                ExchangeData newExchangeData = new ExchangeData();
                newExchangeData.setShortExchangeName(newExchange2);
                controller.getModel().setExchangeData2(newExchangeData);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            String previousCurrency2 = this.getPreviousCurrency2();
            String newCurrency2 = this.getNewCurrency2();
            if (newCurrency2 != null && !newCurrency2.equals(previousCurrency2)) {
                controller.getModel().setUserPreference(MultiBitModel.TICKER_SECOND_ROW_CURRENCY, newCurrency2);
                wantToFireDataStructureChanged = true;
                restartTickerTimer = true;
            }

            // Set on the model the currencies we are interested in - only these
            // get downloaded to save bandwidth/ server time.
            Collection<String> currencies1 = new ArrayList<String>();
            currencies1.add(newCurrency1);
            controller.getModel().getExchangeData1().setCurrenciesWeAreInterestedIn(currencies1);
            if (this.getNewShowSecondRow()) {
                Collection<String> currencies2 = new ArrayList<String>();
                currencies2.add(newCurrency2);
                controller.getModel().getExchangeData2().setCurrenciesWeAreInterestedIn(currencies2);
            }

            // Can undo.
            controller.getModel().setUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES, "true");

            if (restartTickerTimer) {
                // Reinitialise the currency converter.
                CurrencyConverter.INSTANCE.initialise(controller);

                // Cancel any existing timer.
                if (mainFrame.getTickerTimer() != null) {
                    mainFrame.getTickerTimer().cancel();
                }
                // Start ticker timer.
                Timer tickerTimer = new Timer();
                mainFrame.setTickerTimer(tickerTimer);
                tickerTimer.schedule(new TickerTimerTask(controller, mainFrame), 0, TickerTimerTask.DEFAULT_REPEAT_RATE);
            }


            
            if (wantToFireDataStructureChanged) {
                ColorAndFontConstants.init();
                FontSizer.INSTANCE.initialise(controller);
                HelpContentsPanel.clearBrowser();

                controller.fireDataStructureChanged();
                SwingUtilities.updateComponentTreeUI(mainFrame);
            }
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

        exchangeComboBox1 = new JComboBox(ExchangeData.getAvailableExchanges());
        exchangeComboBox1.setSelectedItem(exchangeToUse1);

        exchangeComboBox1.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        exchangeComboBox1.setOpaque(false);

        FontMetrics fontMetrics = tickerPanel.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int textWidth = Math.max(fontMetrics.stringWidth(ExchangeData.MT_GOX_EXCHANGE_NAME), fontMetrics.stringWidth("USD"))
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
        if (mainFrame != null && mainFrame.getTickerTimerTask() != null) {
            TickerTimerTask tickerTimerTask = mainFrame.getTickerTimerTask();
            synchronized (tickerTimerTask) {
                if (tickerTimerTask.getExchange1() == null) {
                    tickerTimerTask.createExchange1(exchangeToUse1);
                }
            }
        }

        currencyComboBox1 = new JComboBox();
        Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeToUse1);
        if (currenciesToUse != null) {
            for (String currency : currenciesToUse) {
                currencyComboBox1.addItem(currency);
            }
        }
        if (originalCurrency1 == null | "".equals(originalCurrency1)) {
            currencyComboBox1.setSelectedItem(ExchangeData.DEFAULT_CURRENCY);
        } else {
            currencyComboBox1.setSelectedItem(originalCurrency1);
            // The currency may have disappeared if the exchange has removed it.
            // Add it back in, otherwise currency choice is lost.
            if (!originalCurrency1.equals(((String) currencyComboBox1.getSelectedItem()))) {
                currencyComboBox1.addItem(originalCurrency1);
                currencyComboBox1.setSelectedItem(originalCurrency1);
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
                    if (mainFrame != null && mainFrame.getTickerTimerTask() != null) {
                        TickerTimerTask tickerTimerTask = mainFrame.getTickerTimerTask();
                        synchronized (tickerTimerTask) {
                            tickerTimerTask.createExchange1(exchangeShortName);
                            currencyComboBox1.removeAllItems();
                            Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeShortName);
                            if (currenciesToUse != null) {
                                for (String currency : currenciesToUse) {
                                    currencyComboBox1.addItem(currency);
                                }
                            }
                        }
                    }
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
        constraints.gridwidth = 1;
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
                    if (mainFrame != null && mainFrame.getTickerTimerTask() != null) {
                        TickerTimerTask tickerTimerTask = mainFrame.getTickerTimerTask();
                        synchronized (tickerTimerTask) {
                            tickerTimerTask.createExchange2(exchangeShortName);
                            currencyComboBox2.removeAllItems();
                            Collection<String> currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeShortName);
                            if (currenciesToUse != null) {
                                for (String currency : currenciesToUse) {
                                    currencyComboBox2.addItem(currency);
                                }
                            }
                        }
                    }
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
        if (mainFrame != null && mainFrame.getTickerTimerTask() != null) {
            TickerTimerTask tickerTimerTask = mainFrame.getTickerTimerTask();
            synchronized (tickerTimerTask) {
                if (tickerTimerTask.getExchange2() == null) {
                    tickerTimerTask.createExchange2(exchangeToUse2);
                }
            }
        }
        currencyComboBox2 = new JComboBox();
        currenciesToUse = ExchangeData.getAvailableCurrenciesForExchange(exchangeToUse2);
        if (currenciesToUse != null) {
            for (String currency : currenciesToUse) {
                currencyComboBox2.addItem(currency);
            }
        }
        if (originalCurrency2 == null | "".equals(originalCurrency2)) {
            currencyComboBox2.setSelectedItem(ExchangeData.DEFAULT_CURRENCY);
        } else {
            currencyComboBox2.setSelectedItem(originalCurrency2);
            // The currency may have disappeared if the exchange has removed it.
            // Add it back in, otherwise currency choice is lost.
            if (!originalCurrency2.equals(((String) currencyComboBox2.getSelectedItem()))) {
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
    public boolean getPreviousShowCurrency() {
        return originalShowCurrency;
    }

    @Override
    public boolean getNewShowCurrency() {
        return showCurrency.isSelected();
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
    public boolean isTickerVisible() {
        return mainFrame.getTickerTablePanel().isVisible();
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
    
}
