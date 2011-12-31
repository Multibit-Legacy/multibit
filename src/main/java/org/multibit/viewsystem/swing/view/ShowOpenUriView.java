package org.multibit.viewsystem.swing.view;

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ShowOpenUriCancelAction;
import org.multibit.viewsystem.swing.action.ShowOpenUriSubmitAction;

/**
 * The show open uri view
 */
public class ShowOpenUriView extends MultiBitDialog implements View, DataProvider {

    private static final long serialVersionUID = 191411112345057705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private String sendAddress;
    private String sendLabel;
    private String sendAmount;

    private JButton submitButton;
    private JButton cancelButton;
    
    private JCheckBox rememberCheckBox;

    /**
     * Creates a new {@link ShowOpenUriView}.
     */
    public ShowOpenUriView(MultiBitController controller, MultiBitFrame mainFrame) {
        super(mainFrame);
        this.controller = controller;
        this.mainFrame = mainFrame;

        initUI();
        
        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
 
        setVisible(true);
    }

    /**
     * show open uri view
     */
    public void initUI() {
        setMinimumSize(new Dimension(480, 160));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);
        setTitle(controller.getLocaliser().getString("showOpenUriView.title"));

        setLayout(new GridBagLayout());
        // get the data out of the wallet preferences
        sendAddress = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_ADDRESS);
        sendLabel = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_LABEL);
        sendAmount = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_AMOUNT);
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
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler01, constraints);
       
        ImageIcon bigIcon = createImageIcon(MultiBitFrame.QUESTION_MARK_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel bigIconLabel = new JLabel(bigIcon);
        add(bigIconLabel, constraints);

        JLabel messageLabel1 = new JLabel();
        messageLabel1.setText(controller.getLocaliser().getString("showOpenUriView.message1"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(messageLabel1, constraints);

        JLabel messageLabel2 = new JLabel();
        messageLabel2.setText(controller.getLocaliser().getString("showOpenUriView.message2"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(messageLabel2, constraints);

        rememberCheckBox = new JCheckBox();
        rememberCheckBox.setText(controller.getLocaliser().getString("showOpenUriView.rememberMyDecision"));
        
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(rememberCheckBox, constraints);
      
        JPanel buttonPanel = new JPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(buttonPanel, constraints);

        ShowOpenUriCancelAction cancelAction = new ShowOpenUriCancelAction(controller, this);
        cancelButton = new JButton(cancelAction);
        cancelButton.setText(controller.getLocaliser().getString("showOpenUriView.noText"));
        buttonPanel.add(cancelButton);

        ShowOpenUriSubmitAction showOpenUriSubmitAction = new ShowOpenUriSubmitAction(mainFrame, controller, this);
        submitButton = new JButton(showOpenUriSubmitAction);
        submitButton.setText(controller.getLocaliser().getString("showOpenUriView.yesText"));
        buttonPanel.add(submitButton);

        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler3, constraints);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        setVisible(false);
    }

    public Data getData() {
        Data data = new Data();
        Item addressItem = new Item(MultiBitModel.OPEN_URI_ADDRESS);
        addressItem.setNewValue(sendAddress);
        data.addItem(MultiBitModel.OPEN_URI_ADDRESS, addressItem);

        Item labelItem = new Item(MultiBitModel.OPEN_URI_LABEL);
        labelItem.setNewValue(sendLabel);
        data.addItem(MultiBitModel.OPEN_URI_LABEL, labelItem);

        Item amountItem = new Item(MultiBitModel.OPEN_URI_AMOUNT);
        amountItem.setNewValue(sendAmount);
        data.addItem(MultiBitModel.OPEN_URI_AMOUNT, amountItem);

        Item showDialogItem = new Item(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        showDialogItem.setNewValue((new Boolean((!rememberCheckBox.isSelected()))).toString());
        data.addItem(MultiBitModel.OPEN_URI_SHOW_DIALOG, showDialogItem );

        return data;
    }

    public void displayView() { 
        // get the data out of the wallet preferences
        sendAddress = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_ADDRESS);
        sendLabel = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_LABEL);
        sendAmount = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_AMOUNT);

        String showDialogString = controller.getModel().getUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG);
       
        if (!(Boolean.FALSE.toString().equalsIgnoreCase(showDialogString))) {
            // missing showDialog or it is set to true
            rememberCheckBox.setSelected(false);
        } else {
            rememberCheckBox.setSelected(true);
        }
        setVisible(true);
    }

    @Override
    public void updateView() {
        // TODO Auto-generated method stub
        
    }
}