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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Vector;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyReceiveAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressAction;
import org.multibit.viewsystem.swing.view.components.DashedBorder;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReceiveBitcoinPanel extends AbstractTradePanel implements DataProvider, View {

    static final Logger log = LoggerFactory.getLogger(ReceiveBitcoinPanel.class);

    private static final long serialVersionUID = -2065108865497842662L;

    private static final String RECEIVE_BITCOIN_BIG_ICON_FILE = "/images/receive-big.jpg";

    public ReceiveBitcoinPanel(MultiBitFrame mainFrame, MultiBitController controller) {
        super(mainFrame, controller);
    }
    
    @Override
    protected boolean isReceiveBitcoin() {
        return true;
    }
    
    @Override
    protected Action getCreateNewAddressAction() {
        return new CreateNewReceivingAddressAction(controller, this);
    }
    
    @Override
    protected String getAddressConstant() {
        return MultiBitModel.RECEIVE_ADDRESS;
    }
    
    @Override
    protected String getLabelConstant() {
        return MultiBitModel.RECEIVE_LABEL;
    }
    @Override
    protected String getAmountConstant() {
        return MultiBitModel.RECEIVE_AMOUNT;
    }
    
    @Override
    protected String getUriImageConstant() {
        return MultiBitModel.RECEIVE_URI_IMAGE;
    }
    
    /**
     * method for concrete impls to populate the localisation map
     */
    @Override
    protected void populateLocalisationMap() {
        localisationKeyConstantToKeyMap.put(ADDRESSES_TITLE, "receiveBitcoinPanel.receivingAddressesTitle");      
        localisationKeyConstantToKeyMap.put(CREATE_NEW_TOOLTIP, "createOrEditAddressAction.createReceiving.tooltip");       
    }

    protected JPanel createFormPanel() {
        formPanel = new JPanel();
        formPanel.setBorder(new DashedBorder(controller.getLocaliser().getLocale()));
        formPanel.setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);

        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEADING);
        buttonPanel.setLayout(flowLayout);

        formPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 0.10;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler1, constraints);

        ImageIcon bigIcon = MultiBitFrame.createImageIcon(RECEIVE_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        formPanel.add(new JLabel(bigIcon), constraints);

        MultiBitLabel helpLabel1 = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel1.message"), controller);
        helpLabel1.setHorizontalAlignment(JLabel.LEADING);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel1, constraints);

        MultiBitLabel helpLabel2 = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel2.message"), controller);
        helpLabel2.setHorizontalAlignment(JLabel.LEADING);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel2, constraints);

        MultiBitLabel helpLabel3 = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel3.message"), controller);
        helpLabel3.setHorizontalAlignment(JLabel.LEADING);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel3, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler2, constraints);

        MultiBitLabel addressLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel"), controller);
        addressLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel.tooltip"));
        addressLabel.setBorder(BorderFactory.createMatteBorder((int)(TEXTFIELD_VERTICAL_DELTA * 0.5), 0, (int)(TEXTFIELD_VERTICAL_DELTA * 0.5), 0, MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR));
        addressLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 1;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        formPanel.add(addressLabel, constraints);

        JLabel filler4 = new JLabel("");
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 1;
        constraints.weighty = 0.5;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler4, constraints);

        FontMetrics fontMetric = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int longFieldWidth = fontMetric.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT);
        addressTextArea = new MultiBitTextArea("", 40, 1, controller);
        addressTextArea.setEditable(false);
        addressTextArea.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(0, 2, 0, 4, MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR),
                BorderFactory.createMatteBorder(2, 0, 0, 0, Color.WHITE)));
        addressTextArea.setMinimumSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight() + TEXTFIELD_VERTICAL_DELTA));
        addressTextArea.setPreferredSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight() + TEXTFIELD_VERTICAL_DELTA));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(addressTextArea, constraints);

        ImageIcon copyIcon = MultiBitFrame.createImageIcon(MultiBitFrame.COPY_ICON_FILE);
        CopyReceiveAddressAction copyAddressAction = new CopyReceiveAddressAction(controller, this, copyIcon);
        MultiBitButton copyAddressButton = new MultiBitButton(copyAddressAction, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 6;
        constraints.gridy = 4;
        constraints.weightx = 3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(copyAddressButton, constraints);

        MultiBitLabel labelLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel"), controller);
        labelLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel.tooltip"));
        labelLabel.setBorder(BorderFactory.createMatteBorder(6, 0, 0, 0, MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR));
        labelLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_TRAILING;
        formPanel.add(labelLabel, constraints);

        JTextField aTextField = new JTextField();
        labelTextArea = new MultiBitTextArea("", 2, 20, controller);
        labelTextArea.setBorder(aTextField.getBorder());
        labelTextArea.addKeyListener(new QRCodeKeyListener());
 
        JScrollPane labelScrollPane = new JScrollPane(labelTextArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        labelScrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, MultiBitFrame.DARK_BACKGROUND_COLOR));
        labelScrollPane.setOpaque(true);
        labelScrollPane.setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 5;
        constraints.weightx = 0.15;
        constraints.weighty = 0.40;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(labelScrollPane, constraints);

        JPanel filler5 = new JPanel();
        filler5.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 5;
        constraints.gridy = 5;
        constraints.weightx = 1;
        constraints.weighty = 0.4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler5, constraints);

        MultiBitLabel amountLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel"), controller);
        amountLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.TRAILING);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.gridwidth = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.30;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(amountLabel, constraints);

        amountTextField = new MultiBitTextField("", 20, controller);
        amountTextField.setHorizontalAlignment(JTextField.TRAILING);
        amountTextField.setMinimumSize(new Dimension((int)(longFieldWidth * 0.5), getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight() + TEXTFIELD_VERTICAL_DELTA));
        amountTextField.setPreferredSize(new Dimension((int)(longFieldWidth * 0.5), getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight() + TEXTFIELD_VERTICAL_DELTA));
        
        amountTextField.addKeyListener(new QRCodeKeyListener());

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 6;
        constraints.weightx = 0.1;
        constraints.weighty = 0.5;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(amountTextField, constraints);

        MultiBitLabel amountUnitLabel = new MultiBitLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel"), controller);
        amountUnitLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel.tooltip"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 6;
        constraints.weightx = 2.0;
        constraints.weighty = 0.30;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(amountUnitLabel, constraints);

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
                Vector<AddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
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
        updateView();
    }

    @Override
    public void updateView() {
        super.updateView();
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
}
