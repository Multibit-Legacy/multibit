package org.multibit.viewsystem.swing.view;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Collection;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.SendBitcoinNowAction;

/**
 * The send bitcoin confirm view
 */
public class SendBitcoinConfirmView extends MultiBitDialog implements View, DataProvider {

    private static final long serialVersionUID = 191435612345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private JLabel sendAddressText;
    private JLabel sendLabelText;
    private JLabel sendAmountText;

    private String sendAddress;
    private String sendLabel;
    private String sendAmount;

    private JLabel confirmText1, confirmText2;
    
    private JButton sendButton;
    private JButton cancelButton;

    /**
     * Creates a new {@link SendBitcoinConfirmView}.
     */
    public SendBitcoinConfirmView(MultiBitController controller, Localiser localiser, MultiBitFrame mainFrame) {
        super(mainFrame);
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;

        initUI();
    }

    public String getDescription() {
        return localiser.getString("sendBitcoinConfirmView.title");
    }

    /**
     * show send bitcoin confirm view
     */
    public void initUI() {
        setMinimumSize(new Dimension(620, 300));
        positionDialogRelativeToParent(this, 0.5D, 0.5D);
        setTitle(localiser.getString("sendBitcoinConfirmView.title"));

        setLayout(new GridBagLayout());
        // get the data out of the wallet preferences
        sendAddress = controller.getModel().getWalletPreference(MultiBitModel.SEND_ADDRESS);
        sendLabel = controller.getModel().getWalletPreference(MultiBitModel.SEND_LABEL);
        sendAmount = controller.getModel().getWalletPreference(MultiBitModel.SEND_AMOUNT) + " BTC";

        GridBagConstraints constraints = new GridBagConstraints();

        ImageIcon bigIcon = createImageIcon(MultiBitFrame.MULTIBIT_ICON_FILE);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.4;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.SOUTH;
        add(new JLabel(bigIcon), constraints);

        JLabel explainLabel = new JLabel();
        explainLabel.setText(localiser.getString("sendBitcoinConfirmView.message"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        add(explainLabel, constraints);

        JLabel sendAddressLabel = new JLabel();
        sendAddressLabel.setText(localiser.getString("sendBitcoinPanel.addressLabel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(sendAddressLabel, constraints);

        JLabel filler1 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler1, constraints);

        sendAddressText = new JLabel();
        sendAddressText.setText(sendAddress);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(sendAddressText, constraints);

        JLabel sendLabelLabel = new JLabel();
        sendLabelLabel.setText(localiser.getString("sendBitcoinPanel.labelLabel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(sendLabelLabel, constraints);

        sendLabelText = new JLabel();
        sendLabelText.setText(sendLabel);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(sendLabelText, constraints);

        JLabel sendAmountLabel = new JLabel();
        sendAmountLabel.setText(localiser.getString("sendBitcoinPanel.amountLabel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(sendAmountLabel, constraints);

        sendAmountText = new JLabel();
        sendAmountText.setText(sendAmount);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(sendAmountText, constraints);

        JPanel buttonPanel = new JPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(buttonPanel, constraints);

        CancelBackToParentAction cancelAction = new CancelBackToParentAction(controller);
        cancelButton = new JButton(cancelAction);
        buttonPanel.add(cancelButton);

        SendBitcoinNowAction sendBitcoinNowAction = new SendBitcoinNowAction(mainFrame, controller, this);
        sendButton = new JButton(sendBitcoinNowAction);
        buttonPanel.add(sendButton);

        confirmText1 = new JLabel();
        confirmText1.setText(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(confirmText1, constraints);
        
        JLabel filler2 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 5;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler2, constraints);

        confirmText2 = new JLabel();
        confirmText2.setText(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(confirmText2, constraints);
        
        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 6;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler3, constraints);



        setVisible(true);
    }

    public void setSendConfirmText(String confirm1, String confirm2) {
        confirmText1.setText(confirm1);
        confirmText2.setText(" " + confirm2);
        
        OkBackToParentAction okAction = new OkBackToParentAction(controller);
        sendButton.setAction(okAction);
        
        cancelButton.setVisible(false);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        setVisible(false);
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }

    public Data getData() {
        Data data = new Data();
        Item addressItem = new Item(MultiBitModel.SEND_ADDRESS);
        addressItem.setNewValue(sendAddress);
        data.addItem(MultiBitModel.SEND_ADDRESS, addressItem);

        Item labelItem = new Item(MultiBitModel.SEND_LABEL);
        labelItem.setNewValue(sendLabel);
        data.addItem(MultiBitModel.SEND_LABEL, labelItem);

        Item amountItem = new Item(MultiBitModel.SEND_AMOUNT);
        amountItem.setNewValue(sendAmount);
        data.addItem(MultiBitModel.SEND_AMOUNT, amountItem);

        return data;
    }

    public void displayView() {
        // get the data out of the wallet preferences
        sendAddress = controller.getModel().getWalletPreference(MultiBitModel.SEND_ADDRESS);
        sendLabel = controller.getModel().getWalletPreference(MultiBitModel.SEND_LABEL);
        sendAmount = controller.getModel().getWalletPreference(MultiBitModel.SEND_AMOUNT) + " BTC";

        sendAddressText.setText(sendAddress);
        sendLabelText.setText(sendLabel);
        sendAmountText.setText(sendAmount);

        SendBitcoinNowAction sendBitcoinNowAction = new SendBitcoinNowAction(mainFrame, controller, this);
        sendButton.setAction(sendBitcoinNowAction);
        confirmText1.setText(" ");
        confirmText2.setText(" ");
        cancelButton.setVisible(true);
        
        setVisible(true);
    }
}