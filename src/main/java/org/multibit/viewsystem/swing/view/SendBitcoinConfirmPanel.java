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
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.SendBitcoinNowAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

import com.google.bitcoin.core.EncryptionType;

/**
 * The send bitcoin confirm panel.
 */
public class SendBitcoinConfirmPanel extends JPanel {

    private static final long serialVersionUID = 191435612399957705L;
   
    private static final int STENT_WIDTH = 10;
    
    private MultiBitFrame mainFrame;
    private MultiBitDialog sendBitcoinConfirmDialog;

    private MultiBitController controller;

    private MultiBitLabel sendAddressText;
    private MultiBitLabel sendLabelText;
    private MultiBitLabel sendAmountText;
    private MultiBitLabel sendFeeText;

    private String sendAddress;
    private String sendLabel;
    private String sendAmount;
    private String sendFee;

    private MultiBitLabel confirmText1, confirmText2;
    
    private SendBitcoinNowAction sendBitcoinNowAction;
    private MultiBitButton sendButton;
    private MultiBitButton cancelButton;
    
    private JPasswordField walletPasswordField;
    private MultiBitLabel walletPasswordPromptLabel;

    /**
     * Creates a new {@link SendBitcoinConfirmPanel}.
     */
    public SendBitcoinConfirmPanel(MultiBitController controller, MultiBitFrame mainFrame, MultiBitDialog sendBitcoinConfirmDialog) {
        super();
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.sendBitcoinConfirmDialog = sendBitcoinConfirmDialog;
        
        initUI();
        
        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * Initialise bitcoin confirm panel.
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);
        
        setLayout(new BorderLayout());
        add(mainPanel, BorderLayout.CENTER);
        
        mainPanel.setLayout(new GridBagLayout());
        
        String[] keys = new String[] { "sendBitcoinPanel.addressLabel",
                "sendBitcoinPanel.labelLabel", "sendBitcoinPanel.amountLabel",
                "showPreferencesPanel.feeLabel.text", "showExportPrivateKeysPanel.walletPasswordPrompt"};

        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, mainPanel)
                + ExportPrivateKeysPanel.STENT_DELTA;


        // Get the data out of the wallet preferences.
        sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        sendLabel = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
        sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT) + " " + controller.getLocaliser(). getString("sendBitcoinPanel.amountUnitLabel");
        String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
        if (fee == null || fee == "") {
            fee = controller.getLocaliser().bitcoinValueToString(MultiBitModel.SEND_FEE_DEFAULT, false, false);
        }
        sendFee = fee + " " + controller.getLocaliser(). getString("sendBitcoinPanel.amountUnitLabel");
    
        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(MultiBitTitledPanel.createStent(STENT_WIDTH), constraints);

        ImageIcon bigIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_128_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel bigIconLabel = new JLabel(bigIcon);
        mainPanel.add(bigIconLabel, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(MultiBitTitledPanel.createStent(STENT_WIDTH), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 7;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(MultiBitTitledPanel.createStent(STENT_WIDTH), constraints);
       
        MultiBitLabel explainLabel = new MultiBitLabel("");
        explainLabel.setText(controller.getLocaliser().getString("sendBitcoinConfirmView.message"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 1.0;
        constraints.gridwidth = 5;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(explainLabel, constraints);

        JPanel detailPanel = new JPanel(new GridBagLayout());
        detailPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 2;
        constraints.weightx = 0.6;
        constraints.weighty = 0.8;
        constraints.gridwidth = 3;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;        
        mainPanel.add(detailPanel, constraints);
        
        GridBagConstraints constraints2 = new GridBagConstraints();
       
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(MultiBitTitledPanel.createStent(stentWidth), constraints2);

        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.05;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        detailPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS),
                constraints2);
        
        JLabel forcer1 = new JLabel();
        forcer1.setOpaque(false);
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 10;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(forcer1, constraints2);

        MultiBitLabel sendAddressLabel = new MultiBitLabel("");
        sendAddressLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.addressLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendAddressLabel, constraints2);

        sendAddressText = new MultiBitLabel("");
        sendAddressText.setText(sendAddress);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendAddressText, constraints2);

        MultiBitLabel sendLabelLabel = new MultiBitLabel("");
        sendLabelLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.labelLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendLabelLabel, constraints2);

        sendLabelText = new MultiBitLabel("");
        sendLabelText.setText(sendLabel);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendLabelText, constraints2);

        MultiBitLabel sendAmountLabel = new MultiBitLabel("");
        sendAmountLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 3;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendAmountLabel, constraints2);

        sendAmountText = new MultiBitLabel("");
        sendAmountText.setText(sendAmount);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 3;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendAmountText, constraints2);

        MultiBitLabel sendFeeLabel = new MultiBitLabel("");
        sendFeeLabel.setText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.text"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 4;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendFeeLabel, constraints2);

        sendFeeText = new MultiBitLabel("");
        sendFeeText.setText(sendFee);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 4;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendFeeText, constraints2);
     
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 5;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(MultiBitTitledPanel.createStent(stentWidth), constraints2);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(MultiBitTitledPanel.createStent(stentWidth), constraints);

        // Add wallet password field.
        walletPasswordPromptLabel = new MultiBitLabel(controller.getLocaliser().getString("showExportPrivateKeysPanel.walletPasswordPrompt"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 8;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(walletPasswordPromptLabel, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 4;
        constraints.gridy = 7;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS),
                constraints);

        JLabel forcer2 = new JLabel();
        forcer2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 5;
        constraints.gridy = 7;
        constraints.weightx = 10;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(forcer2, constraints);

        JPanel filler4 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.01;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler4, constraints);

        walletPasswordField = new JPasswordField(24);
        walletPasswordField.setMinimumSize(new Dimension(200, 20));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 5;
        constraints.gridy = 8;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(walletPasswordField, constraints);

        JPanel filler5 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 9;
        constraints.weightx = 0.3;
        constraints.weighty = 0.01;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler5, constraints);

        if (controller.getModel().getActiveWallet() != null) {
            if (controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
                // Need wallet password.
                walletPasswordField.setEnabled(true);
                walletPasswordPromptLabel.setEnabled(true);
            } else {
                // No wallet password required.
                walletPasswordField.setEnabled(false);
                walletPasswordPromptLabel.setEnabled(false);
            }
        }
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 10;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(buttonPanel, constraints);

        CancelBackToParentAction cancelAction = new CancelBackToParentAction(controller, ImageLoader.createImageIcon(ImageLoader.CROSS_ICON_FILE), sendBitcoinConfirmDialog);
        cancelButton = new MultiBitButton(cancelAction, controller);
        buttonPanel.add(cancelButton);

        sendBitcoinNowAction = new SendBitcoinNowAction(mainFrame, controller, this, walletPasswordField, ImageLoader.createImageIcon(ImageLoader.SEND_BITCOIN_ICON_FILE));
        sendButton = new MultiBitButton(sendBitcoinNowAction, controller);
        buttonPanel.add(sendButton);

        confirmText1 = new MultiBitLabel("");
        confirmText1.setText(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 11;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 6;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(confirmText1, constraints);
        
        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 7;
        constraints.gridy = 11;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler3, constraints);

        confirmText2 = new MultiBitLabel(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 12;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 6;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(confirmText2, constraints);
        
        JLabel filler6 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 7;
        constraints.gridy = 12;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler6, constraints);
    }

    public void setMessageText(final String message1, final String message2) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                confirmText1.setText(message1);
                confirmText2.setText(" " + message2);
            }});
        invalidate();
        validate();
        repaint();
    }    
    
    public void clearPassword() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                walletPasswordField.setText("");
            }});
    }
    
    public void showOkButton() {
        OkBackToParentAction okAction = new OkBackToParentAction(controller, sendBitcoinConfirmDialog);
        sendButton.setAction(okAction);
        
        cancelButton.setVisible(false);
    }
    
    public MultiBitButton getCancelButton() {
        return cancelButton;
    }
    
    // Used in testing.
    public SendBitcoinNowAction getSendBitcoinNowAction() {
        return sendBitcoinNowAction;
    }
    
    public String getMessageText1() {
        return confirmText1.getText();
    }    
    
    public String getMessageText2() {
        return confirmText2.getText();
    }
    
    public void setWalletPassword(char[] password) {
        walletPasswordField.setText(new String(password));
    }
    
    public boolean isWalletPasswordFieldEnabled() {
        return walletPasswordField.isEnabled();
    }
}