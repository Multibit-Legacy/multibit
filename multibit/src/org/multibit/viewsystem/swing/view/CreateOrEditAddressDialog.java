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
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.CreateOrEditAddressSubmitAction;

import com.google.bitcoin.core.ECKey;

/**
 * Dialog for creating and editing address + label combinations
 * 
 * @author jim
 * 
 */
public class CreateOrEditAddressDialog extends MultiBitDialog implements DataProvider {

    private static final long serialVersionUID = -209834865497842662L;

    private MultiBitController controller;

    private JTextField addressTextField;

    private JTextField labelTextField;

    private ECKey candidateNewKey;

    private boolean isCreate;
    private boolean isReceiving;

    /**
     * 
     * @param mainFrame
     * @param localiser
     * @param isCreate
     *            true= vreate, false = edit
     * @param isReceiving
     *            true = receiving address, false = sending
     */
    public CreateOrEditAddressDialog(JFrame mainFrame, MultiBitController controller, boolean isCreate, boolean isReceiving) {
        super(mainFrame);
        this.controller = controller;
        this.isCreate = isCreate;
        this.isReceiving = isReceiving;

        initUI(isCreate, isReceiving);

        loadForm();

        pack();
        
        if (isReceiving) {
            labelTextField.requestFocusInWindow();
        } else {
            addressTextField.requestFocusInWindow();
        }
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
        add(createAddressLabelPanel(helpTextKey1, helpTextKey2, isCreate, isReceiving), BorderLayout.CENTER);
        add(createButtonPanel(), BorderLayout.SOUTH);
    }

    private JPanel createAddressLabelPanel(String helpTextKey1, String helpTextKey2, boolean isCreate, boolean isReceiving) {
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

        JLabel addressLabel = new JLabel(controller.getLocaliser().getString("createOrEditAddressDialog.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("createOrEditAddressDialog.addressLabel.tooltip"));
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

        JLabel labelLabel = new JLabel(controller.getLocaliser().getString("createOrEditAddressDialog.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("createOrEditAddressDialog.labelLabel.tooltip"));
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

        CreateOrEditAddressSubmitAction createOrEditSubmitAction = new CreateOrEditAddressSubmitAction(controller, this, isCreate, isReceiving);
        JButton okButton = new JButton(createOrEditSubmitAction);

        buttonPanel.add(okButton);

        return buttonPanel;
    }

    /**
     * load the current address into the form
     */
    public void loadForm() {
        if (isCreate) {
            if (isReceiving) {
                // create receiving
                candidateNewKey = new ECKey();
                addressTextField.setText(candidateNewKey.toAddress(controller.getMultiBitService().getNetworkParameters())
                        .toString());
                labelTextField.setText(controller.getLocaliser().getString("createOrEditAddressAction.defaultNewLabelText"));
            } else {
                // create sending
                addressTextField.setText("");
                labelTextField.setText("");
            }
        } else {
            if (isReceiving) {
                // edit receiving
                // get the current receive address and label from the model
                String receiveAddress = controller.getModel().getUserPreference(MultiBitModel.RECEIVE_ADDRESS);
                String receiveLabel = controller.getModel().getUserPreference(MultiBitModel.RECEIVE_LABEL);
                if (receiveAddress != null) {
                    addressTextField.setText(receiveAddress);
                }
                if (receiveLabel != null) {
                    labelTextField.setText(receiveLabel);
                }
            } else {
                // edit sending
                // get the current send address and label from the model
                String sendAddress = controller.getModel().getUserPreference(MultiBitModel.SEND_ADDRESS);
                String sendLabel = controller.getModel().getUserPreference(MultiBitModel.SEND_LABEL);
                if (sendAddress != null) {
                    addressTextField.setText(sendAddress);
                }
                if (sendLabel != null) {
                    labelTextField.setText(sendLabel);
                }
            }
        }
    }

    public Data getData() {
        Data data = new Data();
        if (isReceiving) {
            Item receiveAddressItem = new Item(MultiBitModel.RECEIVE_ADDRESS);
            receiveAddressItem.setNewValue(addressTextField.getText());
            data.addItem(MultiBitModel.RECEIVE_ADDRESS, receiveAddressItem);

            Item receiveLabelItem = new Item(MultiBitModel.RECEIVE_LABEL);
            receiveLabelItem.setNewValue(labelTextField.getText());
            data.addItem(MultiBitModel.RECEIVE_LABEL, receiveLabelItem);
            
            Item receiveNewKeyItem = new Item(MultiBitModel.RECEIVE_NEW_KEY);
            receiveNewKeyItem.setNewValue(candidateNewKey);
            data.addItem(MultiBitModel.RECEIVE_NEW_KEY, receiveNewKeyItem);
        } else {
            Item sendAddressItem = new Item(MultiBitModel.SEND_ADDRESS);
            sendAddressItem.setNewValue(addressTextField.getText());
            data.addItem(MultiBitModel.SEND_ADDRESS, sendAddressItem);

            Item sendLabelItem = new Item(MultiBitModel.SEND_LABEL);
            sendLabelItem.setNewValue(labelTextField.getText());
            data.addItem(MultiBitModel.SEND_LABEL, sendLabelItem);
        }
        return data;
    }
}