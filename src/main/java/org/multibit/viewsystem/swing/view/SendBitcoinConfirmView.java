package org.multibit.viewsystem.swing.view;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.Localiser;
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
    private JLabel sendFeeText;

    private String sendAddress;
    private String sendLabel;
    private String sendAmount;
    private String sendFee;

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

    /**
     * show send bitcoin confirm view
     */
    public void initUI() {
        setMinimumSize(new Dimension(680, 320));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);
        setTitle(localiser.getString("sendBitcoinConfirmView.title"));

        setLayout(new GridBagLayout());
        // get the data out of the wallet preferences
        sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        sendLabel = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
        sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT) + " BTC";
        String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
        if (fee == null || fee == "") {
            fee = Localiser.bitcoinValueToString4(MultiBitModel.SEND_MINIMUM_FEE, false, false);
        }
        sendFee = fee + " BTC";
    
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler00 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler00, constraints);

        JLabel filler01 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 5;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler01, constraints);
       
        ImageIcon bigIcon = createImageIcon(MultiBitFrame.MULTIBIT_128_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel bigIconLabel = new JLabel(bigIcon);
        add(bigIconLabel, constraints);

        JLabel explainLabel = new JLabel();
        explainLabel.setText(localiser.getString("sendBitcoinConfirmView.message"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 5;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(explainLabel, constraints);

        JPanel detailPanel = new JPanel(new GridBagLayout());
        detailPanel.setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.6;
        constraints.weighty = 0.8;
        constraints.gridwidth = 3;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;        
        add(detailPanel, constraints);
        
        GridBagConstraints constraints2 = new GridBagConstraints();
        
        JLabel sendAddressLabel = new JLabel();
        sendAddressLabel.setText(localiser.getString("sendBitcoinPanel.addressLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendAddressLabel, constraints2);

        JLabel filler1 = new JLabel();
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler1, constraints2);

        sendAddressText = new JLabel();
        sendAddressText.setText(sendAddress);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendAddressText, constraints2);

        JLabel sendLabelLabel = new JLabel();
        sendLabelLabel.setText(localiser.getString("sendBitcoinPanel.labelLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendLabelLabel, constraints2);

        sendLabelText = new JLabel();
        sendLabelText.setText(sendLabel);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendLabelText, constraints2);

        JLabel sendAmountLabel = new JLabel();
        sendAmountLabel.setText(localiser.getString("sendBitcoinPanel.amountLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendAmountLabel, constraints2);

        sendAmountText = new JLabel();
        sendAmountText.setText(sendAmount);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendAmountText, constraints2);

        JLabel filler2 = new JLabel();
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 3;
        constraints2.weightx = 0.05;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler2, constraints2);
  
        JLabel sendFeeLabel = new JLabel();
        sendFeeLabel.setText(localiser.getString("showPreferencesPanel.feeLabel.text"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 4;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendFeeLabel, constraints2);

        sendFeeText = new JLabel();
        sendFeeText.setText(sendFee);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 4;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendFeeText, constraints2);
     
        JPanel buttonPanel = new JPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
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
        constraints.gridy = 8;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(confirmText1, constraints);
        
        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 8;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler3, constraints);

        confirmText2 = new JLabel();
        confirmText2.setText(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 9;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(confirmText2, constraints);
        
        JLabel filler4 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 9;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler4, constraints);

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
        sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        sendLabel = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
        sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT) + " BTC";

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

    @Override
    public void updateView() {
        // TODO Auto-generated method stub
        
    }
}