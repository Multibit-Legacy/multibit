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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

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
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.qrcode.BitcoinURI;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopySendAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewSendingAddressAction;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.action.MoreOrLessAction;
import org.multibit.viewsystem.swing.action.PasteAddressAction;
import org.multibit.viewsystem.swing.action.SendBitcoinConfirmAction;
import org.multibit.viewsystem.swing.view.components.DashedBorder;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;

public class SendBitcoinPanel extends AbstractTradePanel implements DataProvider, View {

    private static final long serialVersionUID = -2065108865497111662L;

    private final Logger log = LoggerFactory.getLogger(SendBitcoinPanel.class);

    private MultiBitButton pasteAddressButton;
    private MultiBitButton sendButton;

    public SendBitcoinPanel(MultiBitFrame mainFrame, MultiBitController controller) {
        super(mainFrame, controller);
    }

    @Override
    protected boolean isReceiveBitcoin() {
        return false;
    }

    @Override
    protected Action getCreateNewAddressAction() {
        return new CreateNewSendingAddressAction(controller, this);
    }

    @Override
    public String getAddressConstant() {
        return MultiBitModel.SEND_ADDRESS;
    }

    @Override
    public String getLabelConstant() {
        return MultiBitModel.SEND_LABEL;
    }

    @Override
    public String getAmountConstant() {
        return MultiBitModel.SEND_AMOUNT;
    }

    @Override
    protected String getUriImageConstant() {
        return MultiBitModel.SEND_URI_IMAGE;
    }

    /**
     * method for concrete impls to populate the localisation map
     */
    @Override
    protected void populateLocalisationMap() {
        localisationKeyConstantToKeyMap.put(ADDRESSES_TITLE, "sendBitcoinPanel.sendingAddressesTitle");
        localisationKeyConstantToKeyMap.put(CREATE_NEW_TOOLTIP, "createOrEditAddressAction.createSending.tooltip");
    }

    protected JPanel createFormPanel(JPanel formPanel, GridBagConstraints constraints) {
        formPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEADING);
        buttonPanel.setLayout(flowLayout);

        formPanel.setLayout(new GridBagLayout());

        // create stents and forcers
        createFormPanelStentsAndForcers(formPanel, constraints);

        MultiBitLabel addressLabel = new MultiBitLabel(controller.getLocaliser().getString("sendBitcoinPanel.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 4.0;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(addressLabel, constraints);

        int longFieldWidth = fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT);
        addressTextField = new MultiBitTextField("", 24, controller);
        addressTextField.setHorizontalAlignment(JTextField.LEADING);
        addressTextField.setMinimumSize(new Dimension(longFieldWidth, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont())
                .getHeight() + TEXTFIELD_VERTICAL_DELTA));

        addressTextField.addKeyListener(new QRCodeKeyListener());
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.2;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(addressTextField, constraints);

        ImageIcon copyIcon = ImageLoader.createImageIcon(ImageLoader.COPY_ICON_FILE);
        CopySendAddressAction copyAddressAction = new CopySendAddressAction(controller, this, copyIcon);
        MultiBitButton copyAddressButton = new MultiBitButton(copyAddressAction, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 6;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(copyAddressButton, constraints);

        ImageIcon pasteIcon = ImageLoader.createImageIcon(ImageLoader.PASTE_ICON_FILE);
        PasteAddressAction pasteAddressAction = new PasteAddressAction(controller, this, pasteIcon);
        pasteAddressButton = new MultiBitButton(pasteAddressAction, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 8;
        constraints.gridy = 1;
        constraints.weightx = 0.2;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(pasteAddressButton, constraints);

        MultiBitLabel labelLabel = new MultiBitLabel(controller.getLocaliser().getString("sendBitcoinPanel.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_TRAILING;
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
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(labelScrollPane, constraints);

        JPanel amountPanel = new JPanel();
        amountPanel.setOpaque(false);
        // amountPanel.setBorder(BorderFactory.createLineBorder(Color.MAGENTA));
        amountPanel.setLayout(new BorderLayout());

        MultiBitLabel amountLabel = new MultiBitLabel(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel"));
        amountLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.TRAILING);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.20;
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
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        amountPanel.add(amountTextField, BorderLayout.CENTER);
        formPanel.add(amountPanel, constraints);

        MultiBitLabel amountUnitLabel = new MultiBitLabel(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel"));
        amountUnitLabel.setHorizontalTextPosition(SwingConstants.LEFT);
        amountUnitLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel.tooltip"));
        // amountUnitLabel.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        amountPanel.add(amountUnitLabel, BorderLayout.EAST);

        Action helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
                "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                HelpContentsPanel.HELP_SENDING_URL);
        MultiBitButton helpButton = new MultiBitButton(helpAction, controller);
        helpButton.setText("");

        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("sendBitcoinPanel.helpLabel1.message"),
                controller.getLocaliser().getString("sendBitcoinPanel.helpLabel2.message"),
                controller.getLocaliser().getString("sendBitcoinPanel.helpLabel3.message"), "\n",
                controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        helpButton.setToolTipText(tooltipText);
        helpButton.setHorizontalAlignment(SwingConstants.LEADING);
        helpButton.setBorder(BorderFactory.createEmptyBorder(0, 8, 0, 0));
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
        sendButton = new MultiBitButton(sendBitcoinConfirmAction, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 6;
        constraints.gridy = 5;
        constraints.weightx = 3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(sendButton, constraints);

        Action sidePanelAction = new MoreOrLessAction(controller, this);
        sidePanelButton = new MultiBitButton(sidePanelAction, controller);
        displaySidePanel();

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 6;
        constraints.gridy = 8;
        constraints.weightx = 0.1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        formPanel.add(sidePanelButton, constraints);

        return formPanel;
    }

    public String getSendAddress() {
        if (addressTextField != null) {
            return addressTextField.getText();
        } else {
            return "";
        }
    }

    public void loadForm() {
        // get the current address, label and amount from the model
        String address = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        String label = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
        String amount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);

        if (address != null) {
            addressTextField.setText(address);
        } else {
            addressTextField.setText("");
        }
        if (label != null) {
            labelTextArea.setText(label);
        } else {
            labelTextArea.setText("");
        }
        if (amount != null) {
            amountTextField.setText(amount);
        } else {
            amountTextField.setText("");
        }

        // if there is a pending 'handleopenURI' that needs pasting into the
        // send form, do it
        String performPasteNow = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW);
        if (Boolean.TRUE.toString().equalsIgnoreCase(performPasteNow)) {
            try {
                Address decodeAddress = new Address(controller.getMultiBitService().getNetworkParameters(), address);
                processDecodedString(BitcoinURI.convertToBitcoinURI(decodeAddress, Utils.toNanoCoins(amount), label, null), null);
                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW, "false");
                sendButton.requestFocusInWindow();

                mainFrame.bringToFront();
            } catch (AddressFormatException e) {
                throw new RuntimeException(e);
            }
        }

        String showSidePanelText = controller.getModel().getUserPreference(MultiBitModel.SHOW_SIDE_PANEL);
        if (Boolean.TRUE.toString().equals(showSidePanelText)) {
            showSidePanel = true;
        }
        displaySidePanel();
    }

    public void setAddressBookDataByRow(AddressBookData addressBookData) {
        addressTextField.setText(addressBookData.getAddress());
        addressesTableModel.setAddressBookDataByRow(addressBookData, selectedAddressRow, false);
    }

    @Override
    public void displayView() {
        super.displayView();

        String bringToFront = controller.getModel().getUserPreference(MultiBitModel.BRING_TO_FRONT);
        if (Boolean.TRUE.toString().equals(bringToFront)) {
            controller.getModel().setUserPreference(MultiBitModel.BRING_TO_FRONT, "false");
            mainFrame.bringToFront();
        }

        // disable any new changes if another process has changed the wallet
        if (controller.getModel().getActivePerWalletModelData() != null
                && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
            // files have been changed by another process - disallow edits
            mainFrame.setUpdatesStoppedTooltip(addressTextField);
            addressTextField.setEditable(false);
            addressTextField.setEnabled(false);

            if (sendButton != null) {
                sendButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(sendButton);
            }
            if (pasteAddressButton != null) {
                pasteAddressButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(pasteAddressButton);
            }
            titleLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.sendingAddressesTitle.mayBeOutOfDate"));
            mainFrame.setUpdatesStoppedTooltip(titleLabel);
        } else {
            addressTextField.setToolTipText(null);
            addressTextField.setEditable(true);
            addressTextField.setEnabled(true);

            if (sendButton != null) {
                sendButton.setEnabled(true);
                sendButton.setToolTipText(controller.getLocaliser().getString("sendBitcoinAction.tooltip"));
            }
            if (pasteAddressButton != null) {
                pasteAddressButton.setEnabled(true);
                pasteAddressButton.setToolTipText(controller.getLocaliser().getString("pasteAddressAction.tooltip"));
            }
            titleLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.sendingAddressesTitle"));
            titleLabel.setToolTipText(null);
        }
    }

    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.SEND_BITCOIN_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("sendBitcoinConfirmAction.text");
    }

    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("sendBitcoinConfirmAction.tooltip");
    }

    @Override
    public int getViewId() {
        return View.SEND_BITCOIN_VIEW;
    }
}
