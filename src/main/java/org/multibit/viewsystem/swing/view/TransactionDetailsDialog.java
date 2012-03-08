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
package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletTableData;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;

/**
 * The transaction details dialog
 */
public class TransactionDetailsDialog extends MultiBitDialog {

    private static final long serialVersionUID = 191435612345057705L;

    private static final Logger log = LoggerFactory.getLogger(TransactionDetailsDialog.class);


    private static final int HEIGHT_DELTA = 125;
    private static final int WIDTH_DELTA = 300;
    
    private MultiBitController controller;
    private WalletTableData rowTableData;

    private MultiBitLabel confidenceText;
    private MultiBitLabel dateText;
    private MultiBitLabel amountText;
    private MultiBitLabel feeText;
    
    private MultiBitButton okButton;
    
    private SimpleDateFormat dateFormatter;

    /**
     * Creates a new {@link TransactionDetailsDialog}.
     */
    public TransactionDetailsDialog(MultiBitController controller, MultiBitFrame mainFrame, WalletTableData rowTableData) {
        super(mainFrame, controller.getLocaliser().getString("transactionDetailsDialog.title"));
        this.controller = controller;
        this.rowTableData = rowTableData;
        
        dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }
        
        initUI();
        
        okButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * initialise transaction details dialog
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        int minimumHeight = fontMetrics.getHeight() * 12 + HEIGHT_DELTA;
        int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), fontMetrics.stringWidth("0123456789") * 5) + WIDTH_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        positionDialogRelativeToParent(this, 0.5D, 0.40D);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);
        
        setLayout(new BorderLayout());
        add(mainPanel, BorderLayout.CENTER);
        
        mainPanel.setLayout(new BorderLayout());
        
        // get the transaction value out of the wallet data
        BigInteger value = null;
        try {
            value = rowTableData.getTransaction().getValue(controller.getModel().getActiveWallet());
        } catch (ScriptException e) {
            log.error(e.getMessage(), e);

        }

        JPanel detailPanel = new JPanel(new GridBagLayout());
        detailPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);       
        mainPanel.add(detailPanel, BorderLayout.CENTER);
        
        GridBagConstraints constraints = new GridBagConstraints();
        
        MultiBitLabel confidenceLabel = new MultiBitLabel("", controller);
        confidenceLabel.setText(controller.getLocaliser().getString("walletData.statusText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(confidenceLabel, constraints);

        JLabel filler1 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler1, constraints);

        confidenceText = new MultiBitLabel("", controller);
        confidenceText.setText(createStatusText(rowTableData.getTransaction()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(confidenceText, constraints);

        MultiBitLabel dateLabel = new MultiBitLabel("", controller);
        dateLabel.setText(controller.getLocaliser().getString("walletData.dateText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(dateLabel, constraints);

        dateText = new MultiBitLabel("", controller);
        dateText.setText(dateFormatter.format(rowTableData.getDate()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(dateText, constraints);

        MultiBitLabel descriptionLabel = new MultiBitLabel("", controller);
        descriptionLabel.setText(controller.getLocaliser().getString("walletData.descriptionText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(descriptionLabel, constraints);

        MultiBitLabel descriptionText = new MultiBitLabel("", controller);
        descriptionText.setText(createTransactionDescription(rowTableData.getTransaction()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(descriptionText, constraints);

        MultiBitLabel amountLabel = new MultiBitLabel("", controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(amountLabel, constraints);

        amountText = new MultiBitLabel("", controller);
        String btcSuffix =  controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel");
         constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(amountText, constraints);

        MultiBitLabel feeLabel = new MultiBitLabel("", controller);
        feeLabel.setText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.text"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(feeLabel, constraints);

        feeText = new MultiBitLabel("", controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(feeText, constraints);

        MultiBitLabel totalDebitLabel = new MultiBitLabel("", controller);
        totalDebitLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.totalDebit"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(totalDebitLabel, constraints);

        MultiBitLabel totalDebitText = new MultiBitLabel("", controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(totalDebitText, constraints);

        BigInteger fee = rowTableData.getTransaction().calculateFee(controller.getModel().getActiveWallet());
        feeText.setText(Utils.bitcoinValueToPlainString(fee) + " " + btcSuffix);
        if (BigInteger.ZERO.compareTo(value) > 0) {
            // debit
            amountLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.amountSent"));
            try {
                BigInteger totalDebit = rowTableData.getTransaction().getValue(controller.getModel().getActiveWallet()).negate();
                BigInteger amountSent = totalDebit.subtract(fee);
                totalDebitText.setText(Utils.bitcoinValueToPlainString(totalDebit) + " " + btcSuffix);
                amountText.setText(Utils.bitcoinValueToPlainString(amountSent) + " " + btcSuffix);
            } catch (ScriptException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            totalDebitLabel.setVisible(true);
            totalDebitText.setVisible(true);
            feeLabel.setVisible(true);
            feeText.setVisible(true);
         } else {
            // credit - cannot calculate fee so do not show
             try {
                 amountText.setText(Utils.bitcoinValueToPlainString(rowTableData.getTransaction().getValue(controller.getModel().getActiveWallet())) + " " + btcSuffix);
             } catch (ScriptException e) {
                 // TODO Auto-generated catch block
                 e.printStackTrace();
             }
            amountLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.amountReceived"));
            totalDebitLabel.setVisible(false);
            totalDebitText.setVisible(false);
            feeLabel.setVisible(false);
            feeText.setVisible(false);
        }
     
        MultiBitLabel transactionDetailLabel = new MultiBitLabel("", controller);
        transactionDetailLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.transactionDetailText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_TRAILING;
        detailPanel.add(transactionDetailLabel, constraints);

        MultiBitTextArea transactionDetailText = new MultiBitTextArea("", 5, 40, controller);
        // TODO localise
        transactionDetailText.setText(rowTableData.getTransaction().toString());
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;

        JScrollPane scrollPane = new JScrollPane(transactionDetailText, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
 
        detailPanel.add(scrollPane, constraints);

        
        JLabel filler2 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 6;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler2, constraints);
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(buttonPanel, constraints);

        OkBackToParentAction okAction = new OkBackToParentAction(controller, this);
        okButton = new MultiBitButton(okAction, controller);
        buttonPanel.add(okButton);
    }
    
    // TODO localise
    private String createStatusText(Transaction transaction) {
        return transaction.getConfidence().toString();
    }
    
    /**
     * create a description for a transaction
     * 
     * @param transactionInputs
     * @param transactionOutputs
     * @param credit
     * @param debit
     * @return A description of the transaction
     */
    private String createTransactionDescription(Transaction transaction) {
        String toReturn = "";

        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();

        if (perWalletModelData == null) {
            return toReturn;
        }
        
        Wallet wallet = controller.getModel().getActiveWallet();
        List<TransactionOutput> transactionOutputs = transaction.getOutputs();
        
        BigInteger credit = transaction.getValueSentToMe(wallet);
        BigInteger debit = null;
        try {
            debit = transaction.getValueSentFromMe(wallet);
        } catch (ScriptException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
               
        TransactionOutput myOutput = null;
        TransactionOutput theirOutput = null;
        if (transactionOutputs != null) {
            for (TransactionOutput transactionOutput : transactionOutputs) {
                if (transactionOutput != null && transactionOutput.isMine(perWalletModelData.getWallet())) {
                    myOutput = transactionOutput;
                }
                if (transactionOutput != null && !transactionOutput.isMine(perWalletModelData.getWallet())) {
                    theirOutput = transactionOutput;
                }
            }
        }

        if (credit != null && credit.compareTo(BigInteger.ZERO) > 0) {
            // credit
            try {
                String addressString = "";

                if (controller.getMultiBitService() != null && myOutput != null) {
                    Address toAddress = new Address(controller.getMultiBitService().getNetworkParameters(), myOutput
                            .getScriptPubKey().getPubKeyHash());
                    addressString = toAddress.toString();
                }

                String label = null;
                if (perWalletModelData.getWalletInfo() != null) {
                    label = perWalletModelData.getWalletInfo().lookupLabelForReceivingAddress(addressString);
                }
                if (label != null && label != "") {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescriptionWithLabel",
                            new Object[] { addressString, label });
                } else {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescription",
                            new Object[] { addressString });
                }
            } catch (ScriptException e) {
                log.error(e.getMessage(), e);

            }
        }

        if (debit != null && debit.compareTo(BigInteger.ZERO) > 0) {
            // debit
            try {
                // see if the address is a known sending address
                if (theirOutput != null) {
                    String addressString = theirOutput.getScriptPubKey().getToAddress().toString();
                    String label = null;
                    if (perWalletModelData.getWalletInfo() != null) {
                        label = perWalletModelData.getWalletInfo().lookupLabelForSendingAddress(addressString);
                    }
                    if (label != null && label != "") {
                        toReturn = controller.getLocaliser().getString("multiBitModel.debitDescriptionWithLabel",
                                new Object[] { addressString, label });
                    } else {
                        toReturn = controller.getLocaliser().getString("multiBitModel.debitDescription",
                                new Object[] { addressString });
                    }
                }
            } catch (ScriptException e) {
                log.error(e.getMessage(), e);
            }
        }
        return toReturn;
    }
}