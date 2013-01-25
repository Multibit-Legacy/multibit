/**
 * Copyright 2012 multibit.org
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
package org.multibit.viewsystem.swing.view.dialogs;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.SendBitcoinNowAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionConfidence;

/**
 * The send bitcoin confirm dialog
 */
public class SendBitcoinConfirmDialog extends MultiBitDialog {

    private static final long serialVersionUID = 191435612345057705L;

    private static final int HEIGHT_DELTA = 150;
    private static final int WIDTH_DELTA = 150;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private MultiBitLabel sendAddressText;
    private MultiBitLabel sendLabelText;
    private MultiBitLabel sendAmountText;
    private MultiBitLabel sendFeeText;

    private String sendAddress;
    private String sendLabel;

    private MultiBitLabel confirmText1;
    private MultiBitLabel confirmText2;

    private MultiBitButton sendButton;
    private MultiBitButton cancelButton;
    private SendBitcoinNowAction sendBitcoinNowAction;

    private static SendBitcoinConfirmDialog thisDialog = null;

    private static ImageIcon shapeTriangleIcon;
    private static ImageIcon shapeSquareIcon;
    private static ImageIcon shapeHeptagonIcon;
    private static ImageIcon shapeHexagonIcon;
    private static ImageIcon progress0Icon;

    static {
        shapeTriangleIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_TRIANGLE_ICON_FILE);
        shapeSquareIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_SQUARE_ICON_FILE);
        shapeHeptagonIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_PENTAGON_ICON_FILE);
        shapeHexagonIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_HEXAGON_ICON_FILE);
        progress0Icon = ImageLoader.createImageIcon(ShowTransactionsPanel.PROGRESS_0_ICON_FILE);
    }

    /**
     * Creates a new {@link SendBitcoinConfirmDialog}.
     */
    public SendBitcoinConfirmDialog(MultiBitController controller, MultiBitFrame mainFrame) {
        super(mainFrame, controller.getLocaliser().getString("sendBitcoinConfirmView.title"));
        this.controller = controller;
        this.mainFrame = mainFrame;

        thisDialog = this;

        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }

        initUI();

        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * initialise bitcoin confirm dialog
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());

        int minimumHeight = fontMetrics.getHeight() * 10 + HEIGHT_DELTA;
        int minimumWidth = Math.max(Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT),
                fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinConfirmView.message"))),
                fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinConfirmView.multibitMustBeOnline")))
                + WIDTH_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);

        setLayout(new BorderLayout());
        add(mainPanel, BorderLayout.CENTER);

        mainPanel.setLayout(new GridBagLayout());

        // Get the data out of the wallet preferences.
        sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        sendLabel = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
        String sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);

        String sendAmountLocalised = CurrencyConverter.INSTANCE.prettyPrint(sendAmount);

        String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
        if (fee == null || fee == "") {
            fee = controller.getLocaliser().bitcoinValueToString(MultiBitModel.SEND_FEE_DEFAULT, false, false);
        }

        String sendFeeLocalised = CurrencyConverter.INSTANCE.prettyPrint(fee);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler00 = new JLabel();
        //filler00.setBorder(BorderFactory.createLineBorder(Color.RED));
        filler00.setMinimumSize(new Dimension(8,8));
        filler00.setPreferredSize(new Dimension(8,8));
        filler00.setMaximumSize(new Dimension(8,8));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler00, constraints);

        JLabel filler01 = new JLabel();
        //filler01.setBorder(BorderFactory.createLineBorder(Color.RED));
        filler01.setMinimumSize(new Dimension(8,8));
        filler01.setPreferredSize(new Dimension(8,8));
        filler01.setMaximumSize(new Dimension(8,8));
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 6;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler01, constraints);

        ImageIcon bigIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_128_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 2.0;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel bigIconLabel = new JLabel(bigIcon);
        mainPanel.add(bigIconLabel, constraints);

        MultiBitLabel explainLabel = new MultiBitLabel("");
        explainLabel.setText(controller.getLocaliser().getString("sendBitcoinConfirmView.message"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 5;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(explainLabel, constraints);

        JPanel detailPanel = new JPanel(new GridBagLayout());
        detailPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 20;
        constraints.weighty = 0.8;
        constraints.gridwidth = 3;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(detailPanel, constraints);

        GridBagConstraints constraints2 = new GridBagConstraints();

        MultiBitLabel sendAddressLabel = new MultiBitLabel("");
        sendAddressLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.addressLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
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

        sendAddressText = new MultiBitLabel("");
        sendAddressText.setText(sendAddress);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendAddressText, constraints2);

        MultiBitLabel sendLabelLabel = new MultiBitLabel("");
        sendLabelLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.labelLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendLabelLabel, constraints2);

        sendLabelText = new MultiBitLabel("");
        sendLabelText.setText(sendLabel);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendLabelText, constraints2);

        MultiBitLabel sendAmountLabel = new MultiBitLabel("");
        sendAmountLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendAmountLabel, constraints2);

        sendAmountText = new MultiBitLabel("");
        sendAmountText.setText(sendAmountLocalised);
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

        MultiBitLabel sendFeeLabel = new MultiBitLabel("");
        sendFeeLabel.setText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.text"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 4;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendFeeLabel, constraints2);

        sendFeeText = new MultiBitLabel("");
        sendFeeText.setText(sendFeeLocalised);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 4;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendFeeText, constraints2);

        JLabel filler3 = new JLabel();
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 5;
        constraints2.weightx = 0.05;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler3, constraints2);

        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(buttonPanel, constraints);

        CancelBackToParentAction cancelAction = new CancelBackToParentAction(controller,
                ImageLoader.createImageIcon(ImageLoader.CROSS_ICON_FILE), this);
        cancelButton = new MultiBitButton(cancelAction, controller);
        buttonPanel.add(cancelButton);

        sendBitcoinNowAction = new SendBitcoinNowAction(mainFrame, controller, this,
                ImageLoader.createImageIcon(ImageLoader.SEND_BITCOIN_ICON_FILE));
        sendButton = new MultiBitButton(sendBitcoinNowAction, controller);
        buttonPanel.add(sendButton);

        confirmText1 = new MultiBitLabel("");
        confirmText1.setHorizontalAlignment(SwingConstants.TRAILING);
        confirmText1.setText(" ");
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 8;
        constraints.weightx = 0.001;
        constraints.weighty = 0.15;
        constraints.gridwidth = 6;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(confirmText1, constraints);

        JLabel filler4 = new JLabel();
        //filler4.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        filler4.setMinimumSize(new Dimension(8,8));
        filler4.setPreferredSize(new Dimension(8,8));
        filler4.setMaximumSize(new Dimension(8,8));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 6;
        constraints.gridy = 8;
        constraints.weightx = 0.001;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler4, constraints);

        confirmText2 = new MultiBitLabel(" ");
        confirmText2.setHorizontalAlignment(SwingConstants.TRAILING);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 9;
        constraints.weightx = 0.001;
        constraints.weighty = 0.15;
        constraints.gridwidth = 6;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(confirmText2, constraints);

        JLabel filler5 = new JLabel();
        //filler5.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        filler5.setMinimumSize(new Dimension(8,8));
        filler5.setPreferredSize(new Dimension(8,8));
        filler5.setMaximumSize(new Dimension(8,8));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 6;
        constraints.gridy = 9;
        constraints.weightx = 0.001;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        mainPanel.add(filler5, constraints);

        enableSendAccordingToNumberOfConnectedPeers();
    }

    private void enableSendAccordingToNumberOfConnectedPeers() {
        boolean enableSend = false;

        MultiBitModel model = controller.getModel();
        if (model != null) {
            String singleNodeConnection = model.getUserPreference(MultiBitModel.SINGLE_NODE_CONNECTION);
            boolean singleNodeConnectionOverride = singleNodeConnection != null && singleNodeConnection.trim().length() > 0;
            
            if (thisDialog.sendBitcoinNowAction != null) {
                if (!singleNodeConnectionOverride && model.getNumberOfConnectedPeers() < MultiBitModel.MINIMUM_NUMBER_OF_CONNECTED_PEERS_BEFORE_SEND_IS_ENABLED) {
                    // Disable send button
                    enableSend = false;
                } else {
                    // Enable send button
                    enableSend = true;
                }
                thisDialog.sendBitcoinNowAction.setEnabled(enableSend);
            }
        }
        
        if (sendBitcoinNowAction != null) {
            sendBitcoinNowAction.setEnabled(enableSend);
            if (enableSend) {
                if (confirmText1 != null) {
                    if (controller.getLocaliser().getString("sendBitcoinConfirmView.multibitMustBeOnline").equals(confirmText1)){
                        confirmText1.setText(" ");
                    }
                }
            } else {
                if (confirmText1 != null) {
                    confirmText1.setText(controller.getLocaliser().getString("sendBitcoinConfirmView.multibitMustBeOnline"));
                }
            }
        }
    }


    public void setSendConfirmText(String confirm1, String confirm2) {
        confirmText1.setText(confirm1);
        confirmText2.setText(" " + confirm2);

        // REmove the Send button and replace it with an ok button.
        if (sendButton.getAction() instanceof SendBitcoinNowAction) {
            OkBackToParentAction okAction = new OkBackToParentAction(controller, this);
            sendButton.setAction(okAction);

            cancelButton.setVisible(false);
        }
    }

    public static void updateDialog(final Transaction transactionWithChangedConfidence) {
        SwingUtilities.invokeLater(new Runnable() {

            @Override
            public void run() {
                if (thisDialog != null && thisDialog.isVisible()) {
                    MultiBitModel model = MultiBit.getController().getModel();
                    if (model != null) {
                        boolean enableSend = false;
                        if (thisDialog.sendBitcoinNowAction != null) {
                            if (model.getNumberOfConnectedPeers() < MultiBitModel.MINIMUM_NUMBER_OF_CONNECTED_PEERS_BEFORE_SEND_IS_ENABLED) {
                                // Disable send button
                                enableSend = false;
                            } else if (model.getNumberOfConnectedPeers() >= MultiBitModel.MINIMUM_NUMBER_OF_CONNECTED_PEERS_BEFORE_SEND_IS_ENABLED) {
                                // Enable send button
                                enableSend = true;
                            }
                            thisDialog.sendBitcoinNowAction.setEnabled(enableSend);
                        }

                        MultiBitLabel confirmText1 = thisDialog.confirmText1;
                        if (enableSend) {
                            if (confirmText1 != null) {
                                if (MultiBit.getController().getLocaliser()
                                        .getString("sendBitcoinConfirmView.multibitMustBeOnline").equals(confirmText1.getText())) {
                                    confirmText1.setText(" ");
                                }
                            }
                        } else {
                            if (confirmText1 != null) {
                                confirmText1.setText(MultiBit.getController().getLocaliser()
                                        .getString("sendBitcoinConfirmView.multibitMustBeOnline"));
                            }
                        }
                    }

                    if (transactionWithChangedConfidence == null) {
                        return;
                    }

                    MultiBitLabel confirmText2 = thisDialog.getConfirmText2();
                    if (confirmText2 != null) {
                        if (thisDialog.getSendBitcoinNowAction() != null) {
                            Transaction sentTransaction = thisDialog.getSendBitcoinNowAction().getTransaction();
                            if (sentTransaction != null
                                    && sentTransaction.getHash().equals(transactionWithChangedConfidence.getHash())) {
                                confirmText2.setText(thisDialog.getConfidenceToolTip(sentTransaction.getConfidence()));
                                confirmText2.setIcon(thisDialog.getConfidenceIcon(sentTransaction.getConfidence()));
                            }
                        }
                    }
                }
            }
        });
    }
    
    public static void updatePanel(final Transaction transactionWithChangedConfidence) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (thisDialog != null && thisDialog.isVisible()) {
                    MultiBitModel model = MultiBit.getController().getModel();
                    if (model != null) {
                        String singleNodeConnection = model.getUserPreference(MultiBitModel.SINGLE_NODE_CONNECTION);
                        boolean singleNodeConnectionOverride = singleNodeConnection != null && singleNodeConnection.trim().length() > 0;
                        
                        boolean enableSend = false;
                        if (thisDialog.sendBitcoinNowAction != null) {
                            if (!singleNodeConnectionOverride && model.getNumberOfConnectedPeers() < MultiBitModel.MINIMUM_NUMBER_OF_CONNECTED_PEERS_BEFORE_SEND_IS_ENABLED) {
                                // Disable send button
                                enableSend = false;
                            } else {
                                // Enable send button
                                enableSend = true;
                            }
                            thisDialog.sendBitcoinNowAction.setEnabled(enableSend);
                        }

                        MultiBitLabel confirmText1 = thisDialog.confirmText1;
                        if (enableSend) {
                            if (confirmText1 != null) {
                                if (MultiBit.getController().getLocaliser()
                                        .getString("sendBitcoinConfirmView.multibitMustBeOnline").equals(confirmText1.getText())) {
                                    confirmText1.setText(" ");
                                }
                            }
                        } else {
                            if (confirmText1 != null) {
                                confirmText1.setText(MultiBit.getController().getLocaliser()
                                        .getString("sendBitcoinConfirmView.multibitMustBeOnline"));
                            }
                        }
                    }

                    if (transactionWithChangedConfidence == null) {
                        return;
                    }

                    MultiBitLabel confirmText2 = thisDialog.getConfirmText2();
                    if (confirmText2 != null) {
                        if (thisDialog.getSendBitcoinNowAction() != null) {
                            Transaction sentTransaction = thisDialog.getSendBitcoinNowAction().getTransaction();
                            if (sentTransaction != null
                                    && sentTransaction.getHash().equals(transactionWithChangedConfidence.getHash())) {
                                confirmText2.setText(thisDialog.getConfidenceToolTip(transactionWithChangedConfidence
                                        .getConfidence()));
                                confirmText2.setIcon(thisDialog.getConfidenceIcon(transactionWithChangedConfidence.getConfidence()));
                            }
                        }
                    }
                }
            }
        });
    }

    private String getConfidenceToolTip(TransactionConfidence confidence) {
        int peers = 0;
        if (confidence != null) {
            peers = confidence.getBroadcastByCount();
        }
        StringBuilder builder = new StringBuilder();
        if (peers == 0) {
            builder.append(MultiBit.getController().getLocaliser().getString("transactionConfidence.seenByUnknownNumberOfPeers"));
        } else {
            builder.append(MultiBit.getController().getLocaliser().getString("transactionConfidence.seenBy") + " ");
            builder.append(peers);
            if (peers > 1)
                builder.append(" " + MultiBit.getController().getLocaliser().getString("transactionConfidence.peers") + ".");
            else
                builder.append(" " + MultiBit.getController().getLocaliser().getString("transactionConfidence.peer") + ".");
        }

        return builder.toString();

    }

    private ImageIcon getConfidenceIcon(TransactionConfidence confidence) {
        // By default return a triangle which indicates the least known.
        ImageIcon iconToReturn = shapeTriangleIcon;

        if (confidence != null) {
            int numberOfPeers = confidence.getBroadcastByCount();
            if (numberOfPeers >= 4) {
                return progress0Icon;
            } else {
                switch (numberOfPeers) {
                case 0:
                    iconToReturn = shapeTriangleIcon;
                    break;
                case 1:
                    iconToReturn = shapeSquareIcon;
                    break;
                case 2:
                    iconToReturn = shapeHeptagonIcon;
                    break;
                case 3:
                    iconToReturn = shapeHexagonIcon;
                    break;
                default:
                    iconToReturn = shapeTriangleIcon;
                }
            }
        }
        return iconToReturn;
    }

    public SendBitcoinNowAction getSendBitcoinNowAction() {
        return sendBitcoinNowAction;
    }

    public MultiBitLabel getConfirmText2() {
        return confirmText2;
    }
}