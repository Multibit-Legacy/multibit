package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;

/**
 * Dialog for creating and editing address + label combinations
 * 
 * @author jim
 * 
 */
public class CreateOrEditAddressDialog extends MultiBitDialog {

    private static final long serialVersionUID = -209834865497842662L;

    private MultiBitController controller;

    private JTextField addressTextField;

    private JTextField labelTextField;

    /**
     * 
     * @param mainFrame
     * @param localiser
     * @param isCreate
     *            true= vreate, false = edit
     * @param isReceiving
     *            true = receiving address, false = sending
     */
    public CreateOrEditAddressDialog(JFrame mainFrame, MultiBitController controller,
            boolean isCreate, boolean isReceiving) {
        super(mainFrame);
        this.controller = controller;

        initUI(isCreate, isReceiving);

        pack();
    }

    private void initUI(boolean isCreate, boolean isReceiving) {
        String titleLocaliserKey = null;
        ;
        String helpTextKey1 = null;
        ;
        String helpTextKey2 = null;
        if (isCreate) {
            if (isReceiving) {
                // create receiving
                titleLocaliserKey = "createOrEditAddressDialog.createReceiving.title";
                helpTextKey1 = "createOrEditAddressDialog.createReceiving.helpTextKey1";
                helpTextKey2 = "createOrEditAddressDialog.createReceiving.helpTextKey2";
            } else {
                // create sending
                titleLocaliserKey = "createOrEditAddressDialog.createSending.title";
                helpTextKey1 = "createOrEditAddressDialog.createSending.helpTextKey1";
                helpTextKey2 = "createOrEditAddressDialog.createSending.helpTextKey2";
            }
        } else {
            if (isReceiving) {
                // edit receiving
                titleLocaliserKey = "createOrEditAddressDialog.editReceiving.title";
                helpTextKey1 = "createOrEditAddressDialog.editReceiving.helpTextKey1";
                helpTextKey2 = "createOrEditAddressDialog.editReceiving.helpTextKey2";
            } else {
                // edit sending
                titleLocaliserKey = "createOrEditAddressDialog.editSending.title";
                helpTextKey1 = "createOrEditAddressDialog.editSending.helpTextKey1";
                helpTextKey2 = "createOrEditAddressDialog.editSending.helpTextKey2";
            }
        }

        positionDialogRelativeToParent(this, 0.25D, 0.3D);
        setMinimumSize(new Dimension(550, 200));
        setTitle(controller.getLocaliser().getString(titleLocaliserKey));
        setLayout(new BorderLayout());
        add(createAddressLabelPanel(helpTextKey1, helpTextKey2, isCreate, isReceiving),
                BorderLayout.CENTER);
        add(createButtonPanel(), BorderLayout.SOUTH);
    }

    private JPanel createAddressLabelPanel(String helpTextKey1, String helpTextKey2,
            boolean isCreate, boolean isReceiving) {
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
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler1, constraints);

        JLabel helpLabel1 = new JLabel(controller.getLocaliser().getString(helpTextKey1));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel1, constraints);

        JLabel helpLabel2 = new JLabel(controller.getLocaliser().getString(helpTextKey2));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel2, constraints);

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
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler3, constraints);

        JLabel addressLabel = new JLabel(controller.getLocaliser().getString(
                "createOrEditAddressDialog.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString(
                "createOrEditAddressDialog.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(addressLabel, constraints);

        addressTextField = new JTextField();
        addressTextField.setHorizontalAlignment(JTextField.LEFT);
        if (isReceiving) {
            // receiving address is not editable by user
            addressTextField.setEditable(false);
        }
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(addressTextField, constraints);

        JLabel labelLabel = new JLabel(controller.getLocaliser().getString(
                "createOrEditAddressDialog.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString(
                "createOrEditAddressDialog.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(labelLabel, constraints);

        labelTextField = new JTextField();
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(labelTextField, constraints);

        JLabel filler4 = new JLabel("");
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler4, constraints);

        return receiveBitcoinsPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CancelBackToParentAction cancelBackToParentAction = new CancelBackToParentAction(controller);
        JButton cancelButton = new JButton(cancelBackToParentAction);
        buttonPanel.add(cancelButton);

        OkBackToParentAction okBackToParentAction = new OkBackToParentAction(controller);
        JButton okButton = new JButton(okBackToParentAction);

        buttonPanel.add(okButton);

        return buttonPanel;
    }
}