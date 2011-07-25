package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OpenAddressBookAction;
import org.multibit.viewsystem.swing.action.PasteAddressAction;
import org.multibit.viewsystem.swing.action.SendBitcoinConfirmAction;

public class SendBitcoinDialog extends MultiBitDialog {

    private static final long serialVersionUID = -2065108657497842662L;

    private static final String SEND_BITCOIN_BIG_ICON_FILE = "/images/send-big.jpg";

    private MultiBitController controller;

    private JTextField addressTextField;

    private JTextField labelTextField;

    private JTextField amountTextField;

    public SendBitcoinDialog(JFrame mainFrame, MultiBitController controller) {
        super(mainFrame);
        this.controller = controller;

        initUI();

        pack();
        addressTextField.requestFocusInWindow();

    }

    private void initUI() {
        positionDialogRelativeToParent(this, 0.16D, 0.25D);
        setMinimumSize(new Dimension(740, 240));
        setTitle(controller.getLocaliser().getString("sendBitcoinDialog.title"));
        setLayout(new BorderLayout());
        add(createSendBitcoinsPanel(), BorderLayout.CENTER);
        add(createButtonsPanel(), BorderLayout.SOUTH);
    }

    private JPanel createSendBitcoinsPanel() {
        JPanel sendBitcoinsPanel = new JPanel();
        sendBitcoinsPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler0 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(filler0, constraints);

        ImageIcon sendBigIcon = createImageIcon(SEND_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.10;
        constraints.gridwidth = 1;
        constraints.gridheight = 2;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(new JLabel(sendBigIcon), constraints);

        JLabel helpLabel1 = new JLabel(
                controller.getLocaliser().getString("sendBitcoinDialog.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.04;
        constraints.gridwidth = 2;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(helpLabel1, constraints);
      
        JLabel helpLabel2 = new JLabel(
                controller.getLocaliser().getString("sendBitcoinDialog.helpLabel2.message"));
        helpLabel2.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.04;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(helpLabel2, constraints);
      
        JPanel filler1 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(filler1, constraints);
      
        JLabel addressLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinDialog.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinDialog.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(addressLabel, constraints);

        addressTextField = new JTextField();
        addressTextField.setHorizontalAlignment(JTextField.LEFT);

        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.8;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(addressTextField, constraints);

        PasteAddressAction pasteAddressAction = new PasteAddressAction(controller);
        JButton pasteAddressButton = new JButton(pasteAddressAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(pasteAddressButton, constraints);

        OpenAddressBookAction openAddressBookSendingAction = new OpenAddressBookAction(controller, true, false);
        JButton addressBookButton = new JButton(openAddressBookSendingAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(addressBookButton, constraints);

        JLabel labelLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinDialog.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinDialog.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(labelLabel, constraints);

        labelTextField = new JTextField();
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.8;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(labelTextField, constraints);

        JPanel filler2 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(filler2, constraints);
      
        JLabel amountLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinDialog.amountLabel"));
        amountLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinDialog.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.gridx = 0;
        constraints.gridy = 7;
        constraints.gridwidth = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(amountLabel, constraints);

        amountTextField = new JTextField();
        amountTextField.setHorizontalAlignment(JTextField.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(amountTextField, constraints);

        JLabel amountUnitLabel = new JLabel(
                controller.getLocaliser().getString("sendBitcoinDialog.amountUnitLabel"));
        amountUnitLabel.setToolTipText(controller.getLocaliser()
                .getString("sendBitcoinDialog.amountUnitLabel.tooltip"));
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(amountUnitLabel, constraints);

        return sendBitcoinsPanel;
    }

    private JPanel createButtonsPanel() {
        JPanel buttonsPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonsPanel.setLayout(flowLayout);

        CancelBackToParentAction cancelBackToParentAction = new CancelBackToParentAction(controller);
        JButton cancelButton = new JButton(cancelBackToParentAction);
        buttonsPanel.add(cancelButton);

        SendBitcoinConfirmAction sendBitcoinConfirmAction = new SendBitcoinConfirmAction(controller);
        JButton sendButton = new JButton(sendBitcoinConfirmAction);
        buttonsPanel.add(sendButton);

        return buttonsPanel;
    }
}