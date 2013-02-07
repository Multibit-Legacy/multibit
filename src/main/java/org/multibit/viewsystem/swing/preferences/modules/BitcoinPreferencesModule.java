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

import com.google.bitcoin.core.Utils;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.dataproviders.preferences.BitcoinPreferencesDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 *
 * @author Cameron Garnham
 */
public class BitcoinPreferencesModule extends AbstractPreferencesModule implements BitcoinPreferencesDataProvider {

    
    private static final int FEE_TEXT_FIELD_HEIGHT = 30;
    private static final int FEE_TEXT_FIELD_WIDTH = 200;
    
    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    
    
    private MultiBitTextField feeTextField;
    private String originalFee;
    
    private JRadioButton ignoreAll;
    private JRadioButton fillAutomatically;
    private JRadioButton askEveryTime;

    
     public BitcoinPreferencesModule(MultiBitController controller, MultiBitFrame mainFrame){
        this.controller = controller;
        this.mainFrame = mainFrame;
     }
    

    @Override
    public void displayView() {

        if (super.getIsInitialised()) {

            String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);

            if (sendFeeString == null || sendFeeString == "") {
                sendFeeString = controller.getLocaliser()
                        .bitcoinValueToStringNotLocalised(MultiBitModel.SEND_FEE_DEFAULT, false, false);
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
        }
    }
    
    @Override
    public List<JPanel> addPanels() {
        List<JPanel> panels = new ArrayList<JPanel>();
        
        panels.add(createFeePanel(0));
        panels.add(createBrowserIntegrationPanel(0));
        
        return panels;
    }

    @Override
    public PreferencesModule getModuleEnum() {
        return PreferencesModule.BITCOIN;
    }

    
    @Override
    public void onSubmitAction() {

        boolean feeValidationError = false;
        String updateStatusText = "";

        String previousSendFee = this.getPreviousSendFee();
        String newSendFee = this.getNewSendFee();
        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_SEND_FEE, previousSendFee);

        // Check fee is set.
        if (newSendFee == null || "".equals(newSendFee)) {
            // Fee must be set validation error.
            controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
            feeValidationError = true;
            updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.aFeeMustBeSet");
        }

        if (!feeValidationError) {
            try {
                // Check fee is a number.
                BigInteger feeAsBigInteger = Utils.toNanoCoins(newSendFee);

                // Check fee is at least the minimum fee.
                if (feeAsBigInteger.compareTo(MultiBitModel.SEND_MINIMUM_FEE) < 0) {
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString(
                            "showPreferencesPanel.feeCannotBeSmallerThanMinimumFee");
                } else {
                    // Fee is ok.
                    controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, newSendFee);
                }
            } catch (NumberFormatException nfe) {
                // Recycle the old fee and set status message.
                controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                feeValidationError = true;
                updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                        new Object[]{newSendFee});
            } catch (ArithmeticException ae) {
                // Recycle the old fee and set status message.
                controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                feeValidationError = true;
                updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                        new Object[]{newSendFee});
            }
        }

        // Open URI - use dialog.
        String openUriDialog = this.getOpenUriDialog();
        if (openUriDialog != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG, openUriDialog);
        }

        // Open URI - use URI.
        String openUriUseUri = this.getOpenUriUseUri();
        if (openUriUseUri != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI, openUriUseUri);
        }

        if (feeValidationError) {
            MessageManager.INSTANCE.addMessage(new Message(updateStatusText));
        } else {
            // Clear any previous validation error.
            MessageManager.INSTANCE.addMessage(new Message(" "));
        }
    }


    private JPanel createFeePanel(int stentWidth) {
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

    private JPanel createBrowserIntegrationPanel(int stentWidth) {
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


    @Override
    public String getPreviousSendFee() {
        return originalFee;
    }

    @Override
    public String getNewSendFee() {
        CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(feeTextField.getText());
        if (converterResult.isBtcMoneyValid()) {
            return controller.getLocaliser().bitcoinValueToStringNotLocalised(
                    converterResult.getBtcMoney().getAmount().toBigInteger(), false, false);
        } else {
            // Return the unparsable fee for the action to deal with it.
            return feeTextField.getText();
        }
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





    
}
