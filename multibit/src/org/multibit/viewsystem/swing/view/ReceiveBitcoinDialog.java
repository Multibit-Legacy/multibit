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
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.viewsystem.swing.action.CopyAddressAction;
import org.multibit.viewsystem.swing.action.CreateOrEditAddressAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.OpenAddressBookAction;

public class ReceiveBitcoinDialog extends MultiBitDialog implements DataProvider{

    private static final long serialVersionUID = -2065108865497842662L;

    private static final String RECEIVE_BITCOIN_BIG_ICON_FILE = "/images/receive-big.jpg";

    private MultiBitController controller;

    private JTextField addressTextField;

    private JTextField labelTextField;

    private JButton copyAddressButton;

    public ReceiveBitcoinDialog(JFrame mainFrame, MultiBitController controller) {
        super(mainFrame);
        this.controller = controller;

        initUI();

        pack();
        copyAddressButton.requestFocusInWindow();

    }

    private void initUI() {
        positionDialogRelativeToParent(this, 0.25D, 0.25D);
        setMinimumSize(new Dimension(550, 220));
        setTitle(controller.getLocaliser().getString("receiveBitcoinDialog.title"));
        setLayout(new BorderLayout());
        add(createReceiveBitcoinsPanel(), BorderLayout.CENTER);
        add(createButtonPanel(), BorderLayout.SOUTH);
    }

    private JPanel createReceiveBitcoinsPanel() {       
        JPanel receiveBitcoinsPanel = new JPanel();
        
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(flowLayout);
        
        receiveBitcoinsPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        JPanel filler1 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler1, constraints);

        ImageIcon receiveBigIcon = createImageIcon(RECEIVE_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(new JLabel(receiveBigIcon), constraints);
        
        JLabel helpLabel1 = new JLabel(
                controller.getLocaliser().getString("receiveBitcoinDialog.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel1, constraints);

        JPanel filler2 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler2, constraints);
      
        JPanel filler3 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler3, constraints);

        JLabel addressLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinDialog.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser()
                .getString("receiveBitcoinDialog.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(addressLabel, constraints);

        addressTextField = new JTextField();
        addressTextField.setHorizontalAlignment(JTextField.LEFT);

        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(addressTextField, constraints);

        OpenAddressBookAction openAddressBookReceivingAction = new OpenAddressBookAction(controller, true, true);
        JButton addressBookButton = new JButton(openAddressBookReceivingAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(addressBookButton, constraints);

        JLabel labelLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinDialog.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinDialog.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(labelLabel, constraints);

        labelTextField = new JTextField();
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(labelTextField, constraints);

        JLabel filler4 = new JLabel("");
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler4, constraints);
      
        return receiveBitcoinsPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CopyAddressAction copyAddressAction = new CopyAddressAction(controller, this);
        copyAddressButton = new JButton(copyAddressAction);
        buttonPanel.add(copyAddressButton);

        CreateOrEditAddressAction createNewReceivingAddressAction = new CreateOrEditAddressAction(controller, true, true, this);
        JButton createNewButton = new JButton(createNewReceivingAddressAction);
        buttonPanel.add(createNewButton);

        OkBackToParentAction okBackToPreviousAction = new OkBackToParentAction(controller);
        JButton okButton = new JButton(okBackToPreviousAction);
        buttonPanel.add(okButton);

        return buttonPanel;
    }

    public Data getData() {
        // TODO ensure copy address has the current form data so that it can process it
        return null;
    }
}