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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyReceiveAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressAction;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.action.MoreOrLessAction;
import org.multibit.viewsystem.swing.action.SendBitcoinConfirmAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReceiveBitcoinPanel extends AbstractTradePanel implements View {

    static final Logger log = LoggerFactory.getLogger(ReceiveBitcoinPanel.class);

    private static final long serialVersionUID = -2065108865497842662L;

    private CreateNewReceivingAddressAction createNewReceivingAddressAction;
    
    public ReceiveBitcoinPanel(MultiBitFrame mainFrame, MultiBitController controller) {
        super(mainFrame, controller);
    }
    
    @Override
    protected boolean isReceiveBitcoin() {
        return true;
    }
    
    @Override
    protected Action getCreateNewAddressAction() {
        createNewReceivingAddressAction = new CreateNewReceivingAddressAction(controller, this);
        return createNewReceivingAddressAction;
    }
    
    @Override
    public String getAddressConstant() {
        return MultiBitModel.RECEIVE_ADDRESS;
    }
    
    @Override
    public String getLabelConstant() {
        return MultiBitModel.RECEIVE_LABEL;
    }
    @Override
    public String getAmountConstant() {
        return MultiBitModel.RECEIVE_AMOUNT;
    }
    
    /**
     * method for concrete impls to populate the localisation map
     */
    @Override
    protected void populateLocalisationMap() {
        localisationKeyConstantToKeyMap.put(ADDRESSES_TITLE, "receiveBitcoinPanel.receivingAddressesTitle");      
        localisationKeyConstantToKeyMap.put(CREATE_NEW_TOOLTIP, "createOrEditAddressAction.createReceiving.tooltip");       
    }

    protected JPanel createFormPanel(JPanel formPanel, GridBagConstraints constraints) {
        formPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEADING);
        buttonPanel.setLayout(flowLayout);

        formPanel.setLayout(new GridBagLayout());
        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);

        // create stents and forcers
        createFormPanelStentsAndForcers(formPanel,constraints);

        MultiBitLabel addressLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel.tooltip"));
        addressLabel.setBorder(BorderFactory.createMatteBorder((int)(TEXTFIELD_VERTICAL_DELTA * 0.5), 0, (int)(TEXTFIELD_VERTICAL_DELTA * 0.5), 0, ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR));
        addressLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(addressLabel, constraints);

        FontMetrics fontMetric = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int longFieldWidth = fontMetric.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT);
        addressTextArea = new MultiBitTextArea("", 24, 1, controller);
        addressTextArea.setEditable(false);
        addressTextArea.setMinimumSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weightx = 0.2;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(addressTextArea, constraints);

        ImageIcon copyIcon = ImageLoader.createImageIcon(ImageLoader.COPY_ICON_FILE);
        CopyReceiveAddressAction copyAddressAction = new CopyReceiveAddressAction(controller, this, copyIcon);
        MultiBitButton copyAddressButton = new MultiBitButton(copyAddressAction, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 6;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(copyAddressButton, constraints);
        
        JPanel pasteButtonStent = MultiBitTitledPanel.createStent((int)copyAddressButton.getPreferredSize().getWidth(), (int)copyAddressButton.getPreferredSize().getHeight());
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 8;
        constraints.gridy = 1;
        constraints.weightx = 10.0;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(pasteButtonStent, constraints);

        MultiBitLabel labelLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(labelLabel, constraints);

        JTextField aTextField = new JTextField();
        labelTextArea = new MultiBitTextArea("", 2, 20, controller);
        labelTextArea.setBorder(aTextField.getBorder());
        labelTextArea.addKeyListener(new QRCodeKeyListener());
 
        JScrollPane labelScrollPane = new JScrollPane(labelTextArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        labelScrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, ColorAndFontConstants.DARK_BACKGROUND_COLOR));
        labelScrollPane.setOpaque(true);
        labelScrollPane.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.6;
        constraints.weighty = 2.0;
        constraints.gridwidth = 4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(labelScrollPane, constraints);

        JPanel amountPanel = new JPanel();
        amountPanel.setOpaque(false);
        amountPanel.setLayout(new BorderLayout());
        //amountPanel.setBorder(BorderFactory.createLineBorder(Color.BLUE));
        
        MultiBitLabel amountLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel"));
        amountLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.TRAILING);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(amountLabel, constraints);

        amountTextField = new MultiBitTextField("", 10, controller);
        amountTextField.setHorizontalAlignment(JTextField.TRAILING);
        amountTextField.setMinimumSize(new Dimension((int) (longFieldWidth * 0.45), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountTextField.setPreferredSize(new Dimension((int) (longFieldWidth * 0.45), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountTextField.setMaximumSize(new Dimension((int) (longFieldWidth * 0.45), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountTextField.addKeyListener(new QRCodeKeyListener());

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 1.0;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        amountPanel.add(amountTextField, BorderLayout.WEST);
        formPanel.add(amountPanel, constraints);

        MultiBitLabel amountUnitLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel"));
        amountUnitLabel.setHorizontalTextPosition(SwingConstants.LEFT);
        amountUnitLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel.tooltip"));
        //amountUnitLabel.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        JPanel amountUnitLabelPanel = new JPanel();
        amountUnitLabelPanel.setOpaque(false);
        amountUnitLabelPanel.setLayout(new BorderLayout());
        amountUnitLabelPanel.add(MultiBitTitledPanel.createStent(AbstractTradePanel.AMOUNT_BTC_INDENT), BorderLayout.WEST);
        amountUnitLabelPanel.add(amountUnitLabel, BorderLayout.CENTER);

        amountPanel.add(amountUnitLabelPanel, BorderLayout.CENTER);

        Action helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
                "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                HelpContentsPanel.HELP_RECEIVING_URL);
        HelpButton helpButton = new HelpButton(helpAction, controller);
        helpButton.setText("");
 
        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[]{controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel1.message"),
                controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel2.message"), controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel3.message"),
                "\n", controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        helpButton.setToolTipText(tooltipText);
        helpButton.setHorizontalAlignment(SwingConstants.LEADING);
        helpButton.setBorder(BorderFactory.createEmptyBorder(0, HELP_BUTTON_INDENT, 0, 0));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 8;
        constraints.weightx = 1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_LEADING;
        formPanel.add(helpButton, constraints);
        
        SendBitcoinConfirmAction sendBitcoinConfirmAction = new SendBitcoinConfirmAction(controller, mainFrame, this);
        MultiBitButton notUsedSendButton = new MultiBitButton(sendBitcoinConfirmAction, controller);
        JPanel sendButtonStent = MultiBitTitledPanel.createStent((int)notUsedSendButton.getPreferredSize().getWidth(), (int)notUsedSendButton.getPreferredSize().getHeight());
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 6;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(sendButtonStent, constraints);

        Action sidePanelAction = new MoreOrLessAction(controller, this);
        sidePanelButton = new MultiBitButton(sidePanelAction, controller);
        sidePanelButton.setBorder(BorderFactory.createEmptyBorder());
        displaySidePanel();

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 8;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.BASELINE_TRAILING;
        formPanel.add(sidePanelButton, constraints);

        // disable any new changes if another process has changed the wallet
        if (controller.getModel().getActivePerWalletModelData() != null
                && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
            // files have been changed by another process - disallow edits
            labelTextArea.setToolTipText(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip"));
            mainFrame.setUpdatesStoppedTooltip(labelTextArea);

            labelTextArea.setEditable(false);
            labelTextArea.setEnabled(false);
            mainFrame.setUpdatesStoppedTooltip(amountTextField);
            amountTextField.setEditable(false);
            amountTextField.setEnabled(false);
        } else {
            labelTextArea.setToolTipText(null);
            labelTextArea.setEditable(true);
            labelTextArea.setEnabled(true);
            amountTextField.setToolTipText(null);
            amountTextField.setEditable(true);
            amountTextField.setEnabled(true);
        }

        return formPanel;
    }

    public String getReceiveAddress() {
        if (addressTextArea != null) {
            return addressTextArea.getText();
        } else {
            return "";
        }
    }
    
    public void loadForm() {
        // get the current address, label and amount from the model
        String address = controller.getModel().getActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS);
        String label = controller.getModel().getActiveWalletPreference(MultiBitModel.RECEIVE_LABEL);
        String amount = controller.getModel().getActiveWalletPreference(MultiBitModel.RECEIVE_AMOUNT);

        // if the currently stored address is missing or is not in this wallet,
        // pick
        // the address book's first receiving address
        boolean pickFirstReceivingAddress = false;
        if (address == null || address == "") {
            pickFirstReceivingAddress = true;
        } else {
            WalletInfo addressBook = controller.getModel().getActiveWalletWalletInfo();
            if (addressBook != null) {
                if (!addressBook.containsReceivingAddress(address)) {
                    pickFirstReceivingAddress = true;
                }
            }
        }

        if (pickFirstReceivingAddress) {
            WalletInfo addressBook = controller.getModel().getActiveWalletWalletInfo();
            if (addressBook != null) {
                ArrayList<AddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        AddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            address = addressBookData.getAddress();
                            label = addressBookData.getLabel();
                            controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS, address);
                            controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL, label);
                        }
                    }
                }
            }
        }

        if (address != null) {
            addressTextArea.setText(address);
        }
        if (label != null) {
            labelTextArea.setText(label);
        }
        if (amount != null) {
            amountTextField.setText(amount);
        }
    }

    @Override
    public void displayView() {
        super.displayView();
        // disable any new changes if another process has changed the wallet
        if (controller.getModel().getActivePerWalletModelData() != null
                && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
            // files have been changed by another process - disallow edits
            titleLabel.setText(controller.getLocaliser()
                    .getString("receiveBitcoinPanel.receivingAddressesTitle.mayBeOutOfDate"));
            mainFrame.setUpdatesStoppedTooltip(titleLabel);
        } else {
            titleLabel.setText(controller.getLocaliser().getString("receiveBitcoinPanel.receivingAddressesTitle"));
            titleLabel.setToolTipText(null);
        }
    }
    
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.RECEIVE_BITCOIN_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("receiveBitcoinAction.textShort");
    }
  
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("receiveBitcoinAction.tooltip");
    }

    @Override
    public int getViewId() {
        return View.RECEIVE_BITCOIN_VIEW;
    }

    public String getPreviousCurrency2() {
        return null;
    }

    public String getPreviousExchange2() {
        return null;
    }

    public boolean getPreviousShowSecondRow() {
        return false;
    }

    public String getPreviousCurrency1() {
        return null;
    }

    public CreateNewReceivingAddressAction getCreateNewReceivingAddressAction() {
        return createNewReceivingAddressAction;
    }
}
