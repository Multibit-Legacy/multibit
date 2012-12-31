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

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyReceiveAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressAction;
import org.multibit.viewsystem.swing.action.DeleteSendingAddressAction;
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

    public ReceiveBitcoinPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        super(mainFrame, controller);
    }
    
    @Override
    protected boolean isReceiveBitcoin() {
        return true;
    }
    
    @Override
    protected Action getCreateNewAddressAction() {
        createNewReceivingAddressAction = new CreateNewReceivingAddressAction(controller, mainFrame, this);
        return createNewReceivingAddressAction;
    }
    
    @Override
    protected Action getDeleteAddressAction() {
        // Return a delete sending address action - it gets turned into a stent
        return new DeleteSendingAddressAction(controller, mainFrame, null);
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

    @Override
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
        constraints.fill = GridBagConstraints.BOTH;
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
        addressTextField = new MultiBitTextField("", 24, controller);

        //addressTextField = new MultiBitTextArea("", 24, 1, controller);
        addressTextField.setEditable(false);
        addressTextField.setOpaque(false);
        Insets insets = addressTextField.getBorder().getBorderInsets(addressTextField);
        addressTextField.setBorder(BorderFactory.createEmptyBorder(insets.top, insets.left, insets.bottom, insets.right));
        addressTextField.setMinimumSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()));
        
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weightx = 0.2;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(addressTextField, constraints);

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
        labelTextArea = new MultiBitTextArea("", AbstractTradePanel.PREFERRED_NUMBER_OF_LABEL_ROWS , 20, controller);
        labelTextArea.setBorder(aTextField.getBorder());
        labelTextArea.addKeyListener(new QRCodeKeyListener());
 
        final JScrollPane labelScrollPane = new JScrollPane(labelTextArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        labelScrollPane.setOpaque(true);
        labelScrollPane.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        labelScrollPane.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1)); 
        labelScrollPane.getViewport().addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (labelScrollPane.getVerticalScrollBar().isVisible()) {
                    labelScrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.LIGHT_GRAY));          
                } else {
                    labelScrollPane.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));              
                }
            }
        });
        labelScrollPane.setMinimumSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont())
                .getHeight() * AbstractTradePanel.PREFERRED_NUMBER_OF_LABEL_ROWS + TEXTFIELD_VERTICAL_DELTA));

        labelScrollPane.setMinimumSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont())
                .getHeight() * AbstractTradePanel.PREFERRED_NUMBER_OF_LABEL_ROWS + TEXTFIELD_VERTICAL_DELTA + 6));
        labelScrollPane.setPreferredSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont())
                .getHeight() * AbstractTradePanel.PREFERRED_NUMBER_OF_LABEL_ROWS + TEXTFIELD_VERTICAL_DELTA + 6));
        labelScrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        labelScrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.6;
        constraints.weighty = 1.0;
        constraints.gridwidth = 4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(labelScrollPane, constraints);

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

        JPanel amountPanel = createAmountPanel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.20;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(amountPanel, constraints);

        notificationLabel = new MultiBitLabel("");
        notificationLabel.setForeground(Color.RED);
        //notificationLabel.setBorder(BorderFactory.createLineBorder(Color.GREEN));
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 6;
        constraints.gridwidth = 8;
        constraints.gridheight = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        formPanel.add(notificationLabel, constraints);
        
        Action helpAction;
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
                    "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                    HelpContentsPanel.HELP_RECEIVING_URL);
        } else {
            helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_RTL_ICON_FILE,
                    "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                    HelpContentsPanel.HELP_RECEIVING_URL);
        }
        HelpButton helpButton = new HelpButton(helpAction, controller);
        helpButton.setText("");
 
        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[]{controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel1.message"),
                controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel2.message"), controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel3.message"),
                "\n", controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        helpButton.setToolTipText(tooltipText);
        helpButton.setHorizontalAlignment(SwingConstants.LEADING);
        helpButton.setBorder(BorderFactory.createEmptyBorder(0, HELP_BUTTON_INDENT, HELP_BUTTON_INDENT, HELP_BUTTON_INDENT));
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
        sidePanelButton.setBorderPainted(false);
        sidePanelButton.setFocusPainted(false);
        sidePanelButton.setContentAreaFilled(false);

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
            mainFrame.setUpdatesStoppedTooltip(amountBTCTextField);
            amountBTCTextField.setEditable(false);
            amountBTCTextField.setEnabled(false);
        } else {
            labelTextArea.setToolTipText(null);
            labelTextArea.setEditable(true);
            labelTextArea.setEnabled(true);
            amountBTCTextField.setToolTipText(null);
            amountBTCTextField.setEditable(true);
            amountBTCTextField.setEnabled(true);
        }

        return formPanel;
    }

    public String getReceiveAddress() {
        if (addressTextField != null) {
            return addressTextField.getText();
        } else {
            return "";
        }
    }
    
    @Override
    public void loadForm() {
        // get the current address, label and amount from the model
        String address = controller.getModel().getActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS);
        String label = controller.getModel().getActiveWalletPreference(MultiBitModel.RECEIVE_LABEL);

        String amountNotLocalised = controller.getModel().getActiveWalletPreference(MultiBitModel.RECEIVE_AMOUNT);

        if (amountBTCTextField != null) {
            CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(amountNotLocalised);
            
            if (converterResult.isBtcMoneyValid()) {
                parsedAmountBTC = converterResult.getBtcMoney();
                String amountLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(converterResult.getBtcMoney());
                amountBTCTextField.setText(amountLocalised);
                if (notificationLabel != null) {
                    notificationLabel.setText("");
                }
            } else {
                parsedAmountBTC = null;
                amountBTCTextField.setText("");
                if (notificationLabel != null) {
                    notificationLabel.setText(converterResult.getBtcMessage());
                }
            }
        }
        
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
            addressTextField.setText(address);
        }
        if (label != null) {
            labelTextArea.setText(label);
        }
    }

    @Override
    public void displayView() {
        super.displayView();
        
        JTextField aTextField = new JTextField();
        labelTextArea.setBorder(aTextField.getBorder());

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
    
    public CreateNewReceivingAddressAction getCreateNewReceivingAddressAction() {
        return createNewReceivingAddressAction;
    }

    @Override
    public void checkDeleteSendingEnabled() {
        // Not used on receive bitcoin panel.
    }
}
