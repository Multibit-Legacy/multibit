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

import com.google.dogecoin.core.*;
import org.multibit.MultiBit;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletTableData;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.view.components.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.SimpleDateFormat;
import java.util.List;

/**
 * The transaction details dialog.
 */
public class TransactionDetailsDialog extends MultiBitDialog {

    private static final String BLOCKCHAIN_INFO_PREFIX = "http://dogechain.info/tx/";

    private static final String BLOCKEXPLORER_TRANSACTION_PREFIX = "http://dogechain.info/tx/";

    private static final long serialVersionUID = 191435612345057705L;

    private static final Logger log = LoggerFactory.getLogger(TransactionDetailsDialog.class);

    private static final int HEIGHT_DELTA = 150;
    private static final int WIDTH_DELTA = 440;
    private static final int FIELD_SEPARATION = 12;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private WalletTableData rowTableData;

    private MultiBitLabel confidenceText;
    private MultiBitLabel dateText;
    private MultiBitLabel amountText;
    private MultiBitLabel feeText;
    private MultiBitLabel sizeText;

    private JPanel mainPanel;
    private JPanel detailPanel;

    private MultiBitButton okButton;

    public MultiBitButton getOkButton() {
        return okButton;
    }

    private JScrollPane labelScrollPane;
    private JScrollPane detailScrollPane;

    private SimpleDateFormat dateFormatter;
    
    private boolean initialisedOk = false;

    /**
     * Creates a new {@link TransactionDetailsDialog}.
     */
    public TransactionDetailsDialog(BitcoinController bitcoinController, MultiBitFrame mainFrame, WalletTableData rowTableData) {
        super(mainFrame, bitcoinController.getLocaliser().getString("transactionDetailsDialog.title"));
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        this.rowTableData = rowTableData;

        try {
            dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

            ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
            if (imageIcon != null) {
                setIconImage(imageIcon.getImage());
            }

            initUI();

            applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            // Put focus on okButton.
            (new Thread() {
                public void run() {
                    // Sleep long enough for the window to pop up.....
                    try {
                        Thread.sleep(200);
                        // request focus.
                        okButton.requestFocus();

                        Thread.sleep(400);
                    } catch (Exception e) {
                        // Not required.
                    }
                    // Request focus.
                    okButton.requestFocus();
                }
            }).start();

            initialisedOk = true;
        } catch (Exception e) {
            String errorMessage = e.getClass().getName() + " " + e.getMessage();
            log.error(errorMessage);
            MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString(
                    "privateKeysHandler.thereWasAnException", new String[] { errorMessage })));
        }
    }
    
    /**
     * Initialise transaction details dialog.
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());

        int minimumHeight = fontMetrics.getHeight() * 14 + HEIGHT_DELTA;
        int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT),
                fontMetrics.stringWidth("0123456789") * 5)
                + WIDTH_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        positionDialogRelativeToParent(this, 0.5D, 0.45D);

        mainPanel = new JPanel();
        mainPanel.setOpaque(false);

        setLayout(new BorderLayout());
        add(mainPanel, BorderLayout.CENTER);

        mainPanel.setLayout(new BorderLayout());

        // get the transaction value out of the wallet data
        BigInteger value = null;
        try {
            value = rowTableData.getTransaction().getValue(this.bitcoinController.getModel().getActiveWallet());
        } catch (ScriptException e) {
            log.error(e.getMessage(), e);

        }

        detailPanel = new JPanel(new GridBagLayout());
        detailPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        mainPanel.add(detailPanel, BorderLayout.CENTER);

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitLabel confidenceLabel = new MultiBitLabel("");
        confidenceLabel.setText(controller.getLocaliser().getString("walletData.statusText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(confidenceLabel, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(MultiBitTitledPanel.createStent(FIELD_SEPARATION), constraints);

        confidenceText = new MultiBitLabel("");
        confidenceText.setText(createStatusText(rowTableData.getTransaction()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(confidenceText, constraints);

        MultiBitLabel dateLabel = new MultiBitLabel("");
        dateLabel.setText(controller.getLocaliser().getString("walletData.dateText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(dateLabel, constraints);

        dateText = new MultiBitLabel("");
        if (rowTableData.getDate().getTime() > 0) {
            dateText.setText(dateFormatter.format(rowTableData.getDate()));
        }
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(dateText, constraints);

        MultiBitLabel amountLabel = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(amountLabel, constraints);

        amountText = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(amountText, constraints);

        MultiBitLabel feeLabel = new MultiBitLabel("");
        feeLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.feeLabel.text"));
        feeLabel.setToolTipText(controller.getLocaliser().getString("transactionDetailsDialog.feeLabel.tooltip"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(feeLabel, constraints);

        feeText = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(feeText, constraints);

        MultiBitLabel totalDebitLabel = new MultiBitLabel("");
        totalDebitLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.totalDebit"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(totalDebitLabel, constraints);

        MultiBitLabel totalDebitText = new MultiBitLabel("");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(totalDebitText, constraints);

        BigInteger fee = rowTableData.getTransaction().calculateFee(this.bitcoinController.getModel().getActiveWallet());
        feeText.setText(CurrencyConverter.INSTANCE.prettyPrint(Utils.bitcoinValueToPlainString(fee)));
        if (BigInteger.ZERO.compareTo(value) > 0) {
            // debit
            amountLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.amountSent"));
            try {
                BigInteger totalDebit = rowTableData.getTransaction().getValue(this.bitcoinController.getModel().getActiveWallet()).negate();
                BigInteger amountSent = totalDebit.subtract(fee);
                totalDebitText.setText(CurrencyConverter.INSTANCE.prettyPrint(Utils.bitcoinValueToPlainString(totalDebit)));
                amountText.setText(CurrencyConverter.INSTANCE.prettyPrint(Utils.bitcoinValueToPlainString(amountSent)));
            } catch (ScriptException e) {
                e.printStackTrace();
            }

            totalDebitLabel.setVisible(true);
            totalDebitText.setVisible(true);
            feeLabel.setVisible(true);
            feeText.setVisible(true);
        } else {
            // Credit - cannot calculate fee so do not show.
            try {
                amountText.setText(CurrencyConverter.INSTANCE.prettyPrint(Utils.bitcoinValueToPlainString(rowTableData.getTransaction().getValue(
                        this.bitcoinController.getModel().getActiveWallet()))));
            } catch (ScriptException e) {
                e.printStackTrace();
            }
            amountLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.amountReceived"));
            totalDebitLabel.setVisible(false);
            totalDebitText.setVisible(false);
            feeLabel.setVisible(false);
            feeText.setVisible(false);
        }

        MultiBitLabel descriptionLabel = new MultiBitLabel("");
        descriptionLabel.setText(controller.getLocaliser().getString("walletData.descriptionText"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(descriptionLabel, constraints);

        MultiBitTextArea descriptionText = new MultiBitTextArea("", 2, 20, controller);
        descriptionText.setText(createTransactionDescription(rowTableData.getTransaction()));
        descriptionText.setEditable(false);
        descriptionText.setFocusable(true);
        labelScrollPane = new JScrollPane(descriptionText, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        labelScrollPane.setOpaque(true);
        labelScrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        labelScrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        labelScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        labelScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(labelScrollPane, constraints);

        MultiBitLabel transactionDetailLabel = new MultiBitLabel("");
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
        transactionDetailText.setEditable(false);

        transactionDetailText.setText(rowTableData.getTransaction().toString());
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;

        detailScrollPane = new JScrollPane(transactionDetailText, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        detailScrollPane.setOpaque(true);
        detailScrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        detailScrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        detailScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        detailScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

        detailPanel.add(detailScrollPane, constraints);

        JLabel filler2 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 6;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler2, constraints);
  
        MultiBitLabel sizeLabel = new MultiBitLabel("");
        feeLabel.setText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.text"));
        feeLabel.setToolTipText(controller.getLocaliser().getString("transactionDetailsDialog.feeLabel.tooltip"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sizeLabel, constraints);

        sizeText = new MultiBitLabel("");
        sizeLabel.setText(controller.getLocaliser().getString("transactionDetailsDialog.sizeLabel.text"));
        sizeLabel.setToolTipText(controller.getLocaliser().getString("transactionDetailsDialog.sizeLabel.tooltip"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sizeText, constraints);
        
        ByteArrayOutputStream byteOutputStream = new ByteArrayOutputStream();
        try {
            rowTableData.getTransaction().bitcoinSerialize(byteOutputStream);
            sizeText.setText(controller.getLocaliser().getString("showPreferencesPanel.size.text", new Object[] {byteOutputStream.size()}));
        } catch (IOException e1) {
            e1.printStackTrace();
        }


        if (isBrowserSupported()) {
/*            MultiBitButton openInBlockExplorerButton = new MultiBitButton(controller.getLocaliser().getString("transactionDetailsDialog.viewAtBlockExplorer"));
            openInBlockExplorerButton.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    try {
                        String blockExplorerTransactionURL = BLOCKEXPLORER_TRANSACTION_PREFIX + rowTableData.getTransaction().getHashAsString();
                        openURI(new URI(blockExplorerTransactionURL));
                    } catch (URISyntaxException e) {
                        log.debug(e.getMessage());
                    }
                    
                }});*/
            
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 2;
            constraints.gridy = 8;
            constraints.weightx = 0.4;
            constraints.weighty = 0.1;
            constraints.gridwidth = 1;
            constraints.gridheight = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            //detailPanel.add(openInBlockExplorerButton, constraints);

            MultiBitButton openInBlockChainInfoButton = new MultiBitButton(controller.getLocaliser().getString("transactionDetailsDialog.viewAtBlockChainInfo"));
            openInBlockChainInfoButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    try {
                        String blockChainInfoTransactionInfo = BLOCKCHAIN_INFO_PREFIX + rowTableData.getTransaction().getHashAsString();
                        openURI(new URI(blockChainInfoTransactionInfo));
                    } catch (URISyntaxException e) {
                        log.debug(e.getMessage());
                    } 
                }});
            
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 3;
            constraints.gridy = 8;
            constraints.weightx = 0.4;
            constraints.weighty = 0.1;
            constraints.gridwidth = 1;
            constraints.gridheight = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            detailPanel.add(openInBlockChainInfoButton, constraints);
        }

        OkBackToParentAction okAction = new OkBackToParentAction(controller, this);
        okButton = new MultiBitButton(okAction, controller);
        okButton.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent key) {
                if (key.getKeyChar() == KeyEvent.VK_ENTER)
                    okButton.doClick();
            }
        });
   
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 8;
        constraints.weightx = 0.4;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(okButton, constraints);
    }

    private String createStatusText(Transaction transaction) {
        if (transaction.getLockTime() > 0) {
            // Non standard transaction.
            String transactionTrustfulness = MultiBit.getController().getLocaliser().getString("multiBitFrame.status.notConfirmedAndNotStandard") + ". ";
            return transactionTrustfulness + transactionConfidenceToStringLocalised(transaction.getConfidence()); 
        } else {
            return transactionConfidenceToStringLocalised(transaction.getConfidence());   
        }
    }

    /**
     * Create a description for a transaction.
     *
     * @return A description of the transaction
     */
    private String createTransactionDescription(Transaction transaction) {
        String toReturn = "";

        WalletData perWalletModelData = this.bitcoinController.getModel().getActivePerWalletModelData();

        if (perWalletModelData == null) {
            return toReturn;
        }

        Wallet wallet = this.bitcoinController.getModel().getActiveWallet();
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

                if (this.bitcoinController.getMultiBitService() != null && myOutput != null) {
                    Address toAddress = new Address(this.bitcoinController.getModel().getNetworkParameters(), myOutput
                            .getScriptPubKey().getPubKeyHash());
                    addressString = toAddress.toString();
                }

                String label = null;
                if (perWalletModelData.getWalletInfo() != null) {
                    label = perWalletModelData.getWalletInfo().lookupLabelForReceivingAddress(addressString);
                }
                if (label != null && !label.equals("")) {
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
                    String addressString = theirOutput.getScriptPubKey().getToAddress(wallet.getNetworkParameters()).toString();
                    String label = null;
                    if (perWalletModelData.getWalletInfo() != null) {
                        label = perWalletModelData.getWalletInfo().lookupLabelForSendingAddress(addressString);
                    }
                    if (label != null && !label.equals("")) {
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

    private boolean isBrowserSupported() {
        if (!java.awt.Desktop.isDesktopSupported()) {
            return false;
        }

        java.awt.Desktop desktop = java.awt.Desktop.getDesktop();

        if (!desktop.isSupported(java.awt.Desktop.Action.BROWSE)) {
            return false;
        }

        return true;
    }

    private void openURI(URI uri) {
        try {
            java.awt.Desktop desktop = java.awt.Desktop.getDesktop();
            desktop.browse(uri);
        } catch (IOException ioe) {
            log.debug(ioe.getMessage());
            Message message = new Message(controller.getLocaliser().getString("browser.unableToLoad", new String[]{ uri.toString(), ioe.getMessage()}));
            MessageManager.INSTANCE.addMessage(message);
        }
    }

    public boolean isInitialisedOk() {
        return initialisedOk;
    }
    
    private String transactionConfidenceToStringLocalised(TransactionConfidence transactionConfidence) {
        StringBuilder builder = new StringBuilder();

        if (MultiBit.getController() != null && MultiBit.getController().getLocaliser() != null) {
            int peers = transactionConfidence.getBroadcastByCount();
            if (peers > 0) {
                builder
                    .append(MultiBit.getController().getLocaliser().getString("transactionConfidence.seenBy"))
                    .append(" ");
                builder.append(peers);
                if (peers > 1)
                    builder
                        .append(" ")
                        .append(MultiBit.getController().getLocaliser().getString("transactionConfidence.peers"))
                        .append(". ");
                else
                    builder
                        .append(" ")
                        .append(MultiBit.getController().getLocaliser().getString("transactionConfidence.peer"))
                        .append(". ");
            }
            switch (transactionConfidence.getConfidenceType()) {
            case UNKNOWN:
                builder.append(MultiBit.getController().getLocaliser().getString("transactionConfidence.unknownConfidenceLevel"));
                break;
            case DEAD:
                builder.append(MultiBit.getController().getLocaliser().getString("transactionConfidence.dead"));
                break;
            case PENDING:
                builder.append(MultiBit.getController().getLocaliser().getString("transactionConfidence.pending"));
                break;
            case BUILDING:
                builder.append(MultiBit.getController().getLocaliser()
                        .getString("transactionConfidence.appearedInBestChain", new Object[] { transactionConfidence.getAppearedAtChainHeight() }));
                break;
            }
        }
        return builder.toString();
    }

}