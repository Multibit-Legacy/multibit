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
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressSubmitAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.SendBitcoinNowAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

import com.google.bitcoin.core.WalletType;

/**
 * The dialog for creating new receiving addresses
 */
public class CreateNewReceivingAddressDialog extends MultiBitDialog {

    private static final long serialVersionUID = 191439652345057705L;

    private static final int HEIGHT_DELTA = 40;
    private static final int WIDTH_DELTA = 80;
    
    private static final int STENT_WIDTH = 10;
    
    private MultiBitFrame mainFrame;
    private ReceiveBitcoinPanel receiveBitcoinPanel;

    private MultiBitController controller;

    private MultiBitLabel messageText;
    
    private MultiBitButton createNewReceivingAddressSubmitButton;
    private MultiBitButton cancelButton;
    
    private JPasswordField walletPasswordField;
    private MultiBitLabel walletPasswordPromptLabel;
    
    private JComboBox numberOfAddresses;

    /**
     * Creates a new {@link CreateNewReceivingAddressDialog}.
     */
    public CreateNewReceivingAddressDialog(MultiBitController controller, MultiBitFrame mainFrame, ReceiveBitcoinPanel receiveBitcoinPanel) {
        super(mainFrame, controller.getLocaliser().getString("createNewReceivingAddressDialog.title"));
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.receiveBitcoinPanel = receiveBitcoinPanel;
      
        initUI();
        
        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * Initialise dialog.
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        int minimumHeight = fontMetrics.getHeight() * 8 + HEIGHT_DELTA;
        int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinConfirmView.message"))) + WIDTH_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);
        
        setLayout(new BorderLayout());
        add(mainPanel, BorderLayout.CENTER);
        
        mainPanel.setLayout(new GridBagLayout());

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
        explainLabel.setText(controller.getLocaliser().getString("createNewReceivingAddressDialog.message"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(explainLabel, constraints);

        numberOfAddresses = new JComboBox();
        numberOfAddresses.addItem(new Integer(1));
        numberOfAddresses.addItem(new Integer(5));
        numberOfAddresses.addItem(new Integer(20));
        numberOfAddresses.addItem(new Integer(100));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 5;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(numberOfAddresses, constraints);
               
        // Add wallet password field.
        walletPasswordPromptLabel = new MultiBitLabel(controller.getLocaliser().getString("showExportPrivateKeysPanel.walletPasswordPrompt"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 8;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridheight = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
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
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(walletPasswordField, constraints);

        JPanel filler5 = new JPanel();
        filler5.setOpaque(false);
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
            if (controller.getModel().getActiveWallet().getWalletType() == WalletType.ENCRYPTED) {
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

        CancelBackToParentAction cancelAction = new CancelBackToParentAction(controller, ImageLoader.createImageIcon(ImageLoader.CROSS_ICON_FILE), this);
        cancelButton = new MultiBitButton(cancelAction, controller);
        buttonPanel.add(cancelButton);

        CreateNewReceivingAddressSubmitAction createNewReceivingAddressSubmitAction = new CreateNewReceivingAddressSubmitAction(controller, this, walletPasswordField);
        createNewReceivingAddressSubmitButton = new MultiBitButton(createNewReceivingAddressSubmitAction, controller);
        buttonPanel.add(createNewReceivingAddressSubmitButton);

        messageText = new MultiBitLabel("");
        messageText.setText(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 11;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(messageText, constraints);
        
        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 7;
        constraints.gridy = 11;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler3, constraints);
        
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

    public void setMessageText(String message) {
        messageText.setText(message);
    }

    public ReceiveBitcoinPanel getReceiveBitcoinPanel() {
        return receiveBitcoinPanel;
    }
    
    public int getNumberOfAddressesToCreate() {
        return ((Integer)numberOfAddresses.getSelectedItem()).intValue();
    }
}