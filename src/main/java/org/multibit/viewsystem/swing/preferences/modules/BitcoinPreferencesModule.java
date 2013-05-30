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

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.LinkedHashSet;
import java.util.Set;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.joda.money.Money;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.viewsystem.dataproviders.bitcoin.BitcoinPreferencesDataProvider;
import org.multibit.viewsystem.swing.core.components.MultiBitLabel;
import org.multibit.viewsystem.swing.core.components.MultiBitTextField;
import org.multibit.viewsystem.swing.core.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.core.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.preferences.AbstractPreferencesModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham <da2ce7@gmail.com>
 */
public class BitcoinPreferencesModule extends AbstractPreferencesModule<BitcoinController> implements BitcoinPreferencesDataProvider {

    private static final Logger log = LoggerFactory.getLogger(BitcoinPreferencesModule.class);

    private final BitcoinPreferencesPanels bitcoinPreferencesPanels = new BitcoinPreferencesPanels();
    
    private Set<JPanel> jPanels = null;
    
    public static String UNPARSEABLE_FEE = "UNPARSEABLE_FEE";
    
    public BitcoinPreferencesModule(BitcoinController bitcoinController) {
        super(bitcoinController);
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
            jPanels.add(this.bitcoinPreferencesPanels.createFeePanel());
            return jPanels;
        }
    }

    @Override
    public void Update() throws SetupNotCalledException {
    
    String sendFeeString = controller.getModel().getUserPreference(BitcoinModel.SEND_FEE);

        if (sendFeeString == null || sendFeeString == "") {
            sendFeeString = controller.getLocaliser()
                    .bitcoinValueToStringNotLocalised(BitcoinModel.SEND_FEE_DEFAULT, false, false);
        }
        this.bitcoinPreferencesPanels.originalFee = sendFeeString;

        String sendFeeStringLocalised;
        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(sendFeeString);

        if (converterResult.isBtcMoneyValid()) {
            // Check that the fee is at least the minimum fee and smaller than the maximum fee.
            BigInteger feeAsBigInteger = converterResult.getBtcMoney().getAmount().toBigInteger();
            if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MINIMUM_FEE) < 0) {
                // Set the fee to the default fee.
                sendFeeStringLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(
                        Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, new BigDecimal(BitcoinModel.SEND_FEE_DEFAULT)));

            } else {
                if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MAXIMUM_FEE) >= 0) {
                    // Set the fee to the default fee.
                    sendFeeStringLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(
                            Money.of(CurrencyConverter.INSTANCE.BITCOIN_CURRENCY_UNIT, new BigDecimal(BitcoinModel.SEND_FEE_DEFAULT)));
                } else {
                    // Fee is ok.
                    sendFeeStringLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(converterResult.getBtcMoney());
                }
            }
        } else {
            // BTC did not parse - just use the original text
            sendFeeStringLocalised = sendFeeString;
        }
        this.bitcoinPreferencesPanels.feeTextField.setText(sendFeeStringLocalised);
    
    }

    @Override
    public String getPreviousSendFee() {
        return this.bitcoinPreferencesPanels.originalFee;
    }

    @Override
    public String getNewSendFee() {
        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(this.bitcoinPreferencesPanels.feeTextField.getText());
        if (converterResult.isBtcMoneyValid()) {
            return controller.getLocaliser().bitcoinValueToStringNotLocalised(
                    converterResult.getBtcMoney().getAmount().toBigInteger(), false, false);
        } else {
            // Return UNPARSEABLE for the action to deal with it.
            return UNPARSEABLE_FEE + " " + this.bitcoinPreferencesPanels.feeTextField.getText();
        }
    }

    private class BitcoinPreferencesPanels {

        private static final int FEE_TEXT_FIELD_HEIGHT = 30;
        private static final int FEE_TEXT_FIELD_WIDTH = 200;
        private MultiBitTextField feeTextField;
        private String originalFee;

        private JPanel createFeePanel() {
            MultiBitTitledPanel feePanel = new MultiBitTitledPanel(
                    controller.getLocaliser().getString("showPreferencesPanel.feeTitle"),
                    ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

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
            JPanel stent = MultiBitTitledPanel.createStent(100);
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
            feeLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.tooltip")));
            MultiBitLabel feeCurrencyLabel = new MultiBitLabel("BTC");

            String sendFeeString = controller.getModel().getUserPreference(BitcoinModel.SEND_FEE);

            if (sendFeeString == null || sendFeeString == "") {
                sendFeeString = controller.getLocaliser().bitcoinValueToString(BitcoinModel.SEND_FEE_DEFAULT, false, false);
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
    }
}