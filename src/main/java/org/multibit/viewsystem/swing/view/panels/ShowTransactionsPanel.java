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
package org.multibit.viewsystem.swing.view.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Timer;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextPane;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;

import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterListener;
import org.multibit.exchange.ExchangeRate;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletTableData;
import org.multibit.utils.DateUtils;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.UpdateTransactionsTimerTask;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.action.ShowTransactionDetailsAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionConfidence;
import com.google.bitcoin.core.TransactionConfidence.ConfidenceType;

public class ShowTransactionsPanel extends JPanel implements View, CurrencyConverterListener {
    private static final long serialVersionUID = 1235108897887842662L;

    private static final Logger log = LoggerFactory.getLogger(ShowTransactionsPanel.class);

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private JTable table;
    private WalletTableModel walletTableModel;

    private TableRowSorter<TableModel> rowSorter;

    private static final String SPACER = "   "; // 3 spaces
    
    private static final int STATUS_WIDTH_DELTA = 6;

    private static final int TABLE_BORDER = 3;

    private static final int MINIMUM_ICON_HEIGHT = 18;
    
    private static final int HEIGHT_DELTA = 3;

    public static final String PROGRESS_0_ICON_FILE = "/images/circleProgress0.png";
    private static final String PROGRESS_1_ICON_FILE = "/images/circleProgress1.png";
    private static final String PROGRESS_2_ICON_FILE = "/images/circleProgress2.png";
    private static final String PROGRESS_3_ICON_FILE = "/images/circleProgress3.png";
    private static final String PROGRESS_4_ICON_FILE = "/images/circleProgress4.png";
    private static final String PROGRESS_5_ICON_FILE = "/images/circleProgress5.png";
    private static final String RTL_PROGRESS_1_ICON_FILE = "/images/circleProgress1.png";
    private static final String RTL_PROGRESS_2_ICON_FILE = "/images/circleProgress2.png";
    private static final String RTL_PROGRESS_3_ICON_FILE = "/images/circleProgress3.png";
    private static final String RTL_PROGRESS_4_ICON_FILE = "/images/circleProgress4.png";
    private static final String RTL_PROGRESS_5_ICON_FILE = "/images/circleProgress5.png";
    private static final String TICK_ICON_FILE = "/images/tick.png";
    private static final String PICKAXE_ICON_FILE = "/images/pickaxe.png";
    private static final String SMALL_EXCLAMATION_MARK_ICON_FILE = "/images/smallExclamationMark.png";

    private int selectedRow = -1;
    
    public static final int UPDATE_TRANSACTIONS_DELAY_TIME = 333; // milliseconds
    
    public static final int DISPLAY_COUNT_LIMIT = 6;
    private int displayCount;
    
    private JScrollPane scrollPane;
    
    /**
     * Timer used to condense multiple updates
     */
    private static Timer updateTransactionsTimer;

    private static UpdateTransactionsTimerTask updateTransactionsTimerTask;
    
    public ShowTransactionsPanel(MultiBitFrame mainFrame, MultiBitController controller) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        updateTransactionsTimerTask = new UpdateTransactionsTimerTask(controller, this, mainFrame);
        updateTransactionsTimer = new Timer();
        updateTransactionsTimer.scheduleAtFixedRate(updateTransactionsTimerTask, UPDATE_TRANSACTIONS_DELAY_TIME, UPDATE_TRANSACTIONS_DELAY_TIME);
               
        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        
        CurrencyConverter.INSTANCE.addCurrencyConverterListener(this);
        
        displayCount = 0;;
    }

    private void initUI() {
        createWalletPanel();
    }
    
    public static void updateTransactions() {
        if (updateTransactionsTimerTask != null) {
                updateTransactionsTimerTask.setUpdateTransactions(true);                
        }
    }

    private void createWalletPanel() {
        setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        setLayout(new GridBagLayout());
        setOpaque(true);
        GridBagConstraints constraints = new GridBagConstraints();

        walletTableModel = new WalletTableModel(controller);
        table = new JTable(walletTableModel);
        table.setOpaque(false);
        table.setBorder(BorderFactory.createEmptyBorder());
        table.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        table.setRowHeight(Math.max(MINIMUM_ICON_HEIGHT, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()) + HEIGHT_DELTA);

        // Use status icons.
        table.getColumnModel().getColumn(0).setCellRenderer(new ImageRenderer());

        // Set popup for displaying transaction contents.
        table.addMouseListener(new PopClickListener());

        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);
        table.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        // No row is currently selected.
        selectedRow = -1;

        // Date right justified.
        table.getColumnModel().getColumn(1).setCellRenderer(new TrailingJustifiedDateRenderer());

        // Justify column headers.
        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);
        table.getTableHeader().setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        // Description leading justified (set explicitly as it does not seem to work otherwise).
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            table.getColumnModel().getColumn(2).setCellRenderer(new LeadingJustifiedRenderer());
        } else {
            table.getColumnModel().getColumn(2).setCellRenderer(new TrailingJustifiedStringRenderer());
        }

        // Amount decimal aligned
        DecimalAlignRenderer decimalAlignRenderer = new DecimalAlignRenderer();
        table.getColumnModel().getColumn(3).setCellRenderer(decimalAlignRenderer);
 

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        TableColumn tableColumn = table.getColumnModel().getColumn(0); // status
        int statusWidth = fontMetrics.stringWidth(controller.getLocaliser().getString("walletData.statusText"));
        tableColumn.setPreferredWidth(statusWidth + STATUS_WIDTH_DELTA);

        tableColumn = table.getColumnModel().getColumn(1); // Date.
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        int dateWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("walletData.dateText")),
                fontMetrics.stringWidth(dateFormatter.format(new Date(DateUtils.nowUtc().getMillis()))));
        tableColumn.setPreferredWidth(dateWidth);

        tableColumn = table.getColumnModel().getColumn(2); // Description.
        tableColumn.setPreferredWidth(250);

        tableColumn = table.getColumnModel().getColumn(3); // Amount (BTC).
        int amountBTCWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel") + " (BTC)"),
                fontMetrics.stringWidth("00000.000000000"));
        tableColumn.setPreferredWidth(amountBTCWidth);
        tableColumn.setMinWidth(amountBTCWidth);

        if (CurrencyConverter.INSTANCE.isShowingFiat()) {
            tableColumn = table.getColumnModel().getColumn(4); // Amount (fiat).
            int amountFiatWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel") + " (USD)"),
                    fontMetrics.stringWidth("000.0000"));
            tableColumn.setPreferredWidth(amountFiatWidth);
           
            table.getColumnModel().getColumn(4).setCellRenderer(new TrailingJustifiedNumericRenderer());
        }

        // Row sorter.
        rowSorter = new TableRowSorter<TableModel>(table.getModel());
        table.setRowSorter(rowSorter);

        // Sort by date descending.
        List<TableRowSorter.SortKey> sortKeys = new ArrayList<TableRowSorter.SortKey>();
        sortKeys.add(new TableRowSorter.SortKey(1, SortOrder.DESCENDING));
        rowSorter.setSortKeys(sortKeys);
        Comparator<Date> comparator = new Comparator<Date>() {
            public int compare(Date o1, Date o2) {
                if (o1 == null) {
                    if (o2 == null) {
                        return 0;
                    } else {
                        return 1;
                    }
                } else {
                    if (o2 == null) {
                        return -1;
                    }
                }
                long n1 = o1.getTime();
                long n2 = o2.getTime();
                if (n1 == 0) {
                    // Object 1 has missing date.
                    return 1;
                }
                if (n2 == 0) {
                    // Object 2 has missing date.
                    return -1;
                }
                if (n1 < n2) {
                    return -1;
                } else if (n1 > n2) {
                    return 1;
                } else {
                    return 0;
                }
            }
        };
        rowSorter.setComparator(1, comparator);

        Comparator<String> comparatorNumber = new Comparator<String>() {
            public int compare(String o1, String o2) {
                try {
                    if (o1 == null) {
                        if (o2 == null) {
                            return 0;
                        } else {
                            return 1;
                        }
                    } else {
                        if (o2 == null) {
                            return -1;
                        }
                    }
                    DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
                    formatter.setParseBigDecimal(true);

                    // Convert spaces to non breakable space.
                    o1 = o1.replaceAll(" ", "\u00A0");
                    o2 = o2.replaceAll(" ", "\u00A0");

                    BigDecimal parsedO1 = (BigDecimal) formatter.parse(o1);
                    BigDecimal parsedO2 = (BigDecimal) formatter.parse(o2);
                    return parsedO1.compareTo(parsedO2);
                } catch (NumberFormatException nfe) {
                    return o1.compareTo(o2);
                } catch (ParseException e) {
                    return o1.compareTo(o2);
                }
            }
        };
        rowSorter.setComparator(3, comparatorNumber);
        if (CurrencyConverter.INSTANCE.isShowingFiat()) {
            rowSorter.setComparator(4, comparatorNumber);
        }
        
        scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        scrollPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        scrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        scrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        scrollPane.setOpaque(true);
        scrollPane.getViewport().setOpaque(true);
        

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        add(scrollPane, constraints);
    }

    @Override
    public void displayView() {
        //log.debug("ShowTransactionsPanel#displayView called on panel " + System.identityHashCode(this) + " for wallet " + controller.getModel().getActiveWalletFilename());
        
        if (controller.getModel().getActiveWallet() == null) {
            return;
        }
        walletTableModel.recreateWalletData();

        if (selectedRow > -1 && selectedRow < table.getRowCount()) {
            table.setRowSelectionInterval(selectedRow, selectedRow);
        }

        // If it is the first showing - schedule to redisplay.
        // This is to get rid of the bug on the first row amount (BTC) display.
        if (displayCount < DISPLAY_COUNT_LIMIT) {
            displayCount++;
            ShowTransactionsPanel.updateTransactions();
        }
        //log.debug("Table has " + table.getRowCount() + " rows");
    }

    @Override
    public void navigateAwayFromView() {
    }

    public WalletTableModel getWalletTableModel() {
        return walletTableModel;
    }

    class ImageRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 154545L;

        JLabel primaryLabel = new JLabel();

        // If the component is a doubleIcon the next fields are used.
        JLabel extraLabel = new JLabel();
        boolean doubleIcon = false;
        JPanel combinationPanel = new JPanel();
        
        ImageIcon shapeTriangleIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_TRIANGLE_ICON_FILE);
        ImageIcon shapeSquareIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_SQUARE_ICON_FILE);
        ImageIcon shapeHeptagonIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_PENTAGON_ICON_FILE);
        ImageIcon shapeHexagonIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_HEXAGON_ICON_FILE);
        ImageIcon tickIcon = ImageLoader.createImageIcon(TICK_ICON_FILE);
        ImageIcon pickaxeIcon = ImageLoader.createImageIcon(PICKAXE_ICON_FILE);
        ImageIcon smallExclamationMarkIcon = ImageLoader.createImageIcon(SMALL_EXCLAMATION_MARK_ICON_FILE);
        ImageIcon progress0Icon = ImageLoader.createImageIcon(PROGRESS_0_ICON_FILE);
        ImageIcon progress1Icon = ImageLoader.createImageIcon(PROGRESS_1_ICON_FILE);
        ImageIcon progress2Icon = ImageLoader.createImageIcon(PROGRESS_2_ICON_FILE);
        ImageIcon progress3Icon = ImageLoader.createImageIcon(PROGRESS_3_ICON_FILE);
        ImageIcon progress4Icon = ImageLoader.createImageIcon(PROGRESS_4_ICON_FILE);
        ImageIcon progress5Icon = ImageLoader.createImageIcon(PROGRESS_5_ICON_FILE);
        ImageIcon rtlProgress1Icon = ImageLoader.createImageIcon(RTL_PROGRESS_1_ICON_FILE);
        ImageIcon rtlProgress2Icon = ImageLoader.createImageIcon(RTL_PROGRESS_2_ICON_FILE);
        ImageIcon rtlProgress3Icon = ImageLoader.createImageIcon(RTL_PROGRESS_3_ICON_FILE);
        ImageIcon rtlProgress4Icon = ImageLoader.createImageIcon(RTL_PROGRESS_4_ICON_FILE);
        ImageIcon rtlProgress5Icon = ImageLoader.createImageIcon(RTL_PROGRESS_5_ICON_FILE);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            
            // Prepare the primary icon (used always), and an extra icon and containing panel for use as required.
            primaryLabel.setHorizontalAlignment(SwingConstants.CENTER);
            primaryLabel.setVerticalAlignment(SwingConstants.CENTER);
            primaryLabel.setOpaque(true);
            extraLabel.setHorizontalAlignment(SwingConstants.CENTER);
            extraLabel.setVerticalAlignment(SwingConstants.CENTER);
            extraLabel.setOpaque(true);
            combinationPanel.setOpaque(true);
            combinationPanel.setLayout(new GridBagLayout());

            GridBagConstraints constraints = new GridBagConstraints();

            // Prepare a double icon panel for use as required.
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 0;
            constraints.gridy = 0;
            constraints.weightx = 1;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.LINE_END;
            combinationPanel.add(primaryLabel, constraints);

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            constraints.gridy = 0;
            constraints.weightx = 1;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.LINE_START;

            combinationPanel.add(extraLabel, constraints); 
            
            
            // Get the transaction and transaction confidence
            Transaction transaction = (Transaction)value;
     
            TransactionConfidence confidence = null;
            if (transaction != null) {
                confidence = transaction.getConfidence();
            }            
            ConfidenceType confidenceType = null;
            if (confidence != null) {
                confidenceType = confidence.getConfidenceType();
            }
            if (confidenceType == null) {
                confidenceType = ConfidenceType.UNKNOWN;
            }
 
   
            // Coinbase transactions have an extra pickaxe icon.
            if (transaction != null && transaction.isCoinBase()) {
                extraLabel.setIcon(pickaxeIcon);
                doubleIcon = true;
            } else {
                doubleIcon = false;
            }

            // Work out the primary icon.
            switch (confidenceType) {
            case UNKNOWN: {
                primaryLabel.setText("?");
                primaryLabel.setIcon(null);
                break;
            }
            case BUILDING: {
                if (controller.getMultiBitService().getChain() == null) {
                    primaryLabel.setText("?");
                    primaryLabel.setIcon(null);
                } else {
                    int numberOfBlocksEmbedded = controller.getMultiBitService().getChain().getBestChainHeight() - confidence.getAppearedAtChainHeight() + 1;
                    if (transaction != null && transaction.isCoinBase()) {
                        // Coinbase tx mature slower than regular blocks
                        numberOfBlocksEmbedded = numberOfBlocksEmbedded / 20;
                    }
                    ImageIcon buildingIcon = getBuildingIcon(numberOfBlocksEmbedded, transaction);
                    primaryLabel.setIcon(buildingIcon);
                    primaryLabel.setText("");
                    if (numberOfBlocksEmbedded >= 6) {
                        primaryLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.isConfirmed"));
                    } else {
                        if (transaction != null && transaction.isCoinBase()) {
                            primaryLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmedAndCoinbase"));                            
                        } else {
                            primaryLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                        }
                    }
                }
                break;
            }
            case NOT_SEEN_IN_CHAIN: {
                primaryLabel.setIcon(getConfidenceIcon(confidence));
                primaryLabel.setText("");
                
                primaryLabel.setToolTipText(getUnconfirmedConfidenceToolTip(transaction) );
                
                if (transaction != null) {
                    if (transaction.getLockTime() > 0) {
                        extraLabel.setIcon(smallExclamationMarkIcon);
                        doubleIcon = true;
                    } else {
                        doubleIcon = false;
                    }
                }
                break;
            }
            case NOT_IN_BEST_CHAIN: {
                primaryLabel.setIcon(getConfidenceIcon(confidence));
                primaryLabel.setText("");
                primaryLabel.setToolTipText(getUnconfirmedConfidenceToolTip(transaction) );
                
                if (transaction != null) {
                    if (transaction.getLockTime() > 0) {
                        extraLabel.setIcon(smallExclamationMarkIcon);
                        doubleIcon = true;
                    } else {
                        doubleIcon = false;
                    }
                }

                break;
            }
            case DEAD: {
                primaryLabel.setIcon(smallExclamationMarkIcon);
                primaryLabel.setText(controller.getLocaliser().getString("multiBitFrame.status.dead"));
                break;
            }
            default: {
                primaryLabel.setIcon(null);
                primaryLabel.setText("?");
                break;
            }
            }
           
            // Propagate the tooltip text.
            extraLabel.setToolTipText(primaryLabel.getToolTipText());
            combinationPanel.setToolTipText(primaryLabel.getToolTipText());

            // Set foreground and background colors.
            if (isSelected) {
                selectedRow = row;
                primaryLabel.setBackground(table.getSelectionBackground());
                primaryLabel.setForeground(table.getSelectionForeground());
                extraLabel.setBackground(table.getSelectionBackground());
                extraLabel.setForeground(table.getSelectionForeground());
                combinationPanel.setBackground(table.getSelectionBackground());
            } else {
                primaryLabel.setForeground(table.getForeground());
                extraLabel.setForeground(table.getForeground());
                combinationPanel.setForeground(table.getForeground());
                if (row % 2 == 0) {
                    primaryLabel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                    extraLabel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                    combinationPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                } else {
                    primaryLabel.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    extraLabel.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    combinationPanel.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    primaryLabel.setOpaque(true);
                    extraLabel.setOpaque(true);
                    combinationPanel.setOpaque(true);
                }
            }

            // Return either a single icon or a double icon panel.
            if (doubleIcon) {
                return combinationPanel;
            } else {
                return primaryLabel;
            }
        }

        private ImageIcon getBuildingIcon(int numberOfBlocksEmbedded, Transaction transaction) {
            TransactionConfidence confidence = null;
            if (transaction != null) {
                confidence = transaction.getConfidence();
            }
            
            if (numberOfBlocksEmbedded < 0) {
                numberOfBlocksEmbedded = 0;
            }
            if (numberOfBlocksEmbedded > 6) {
                numberOfBlocksEmbedded = 6;
            }

            boolean isLeftToRight = ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight();

            switch (numberOfBlocksEmbedded) {
            case 0: {
                return getConfidenceIcon(confidence);
            }
            case 1: {
                if (isLeftToRight) {
                    return progress1Icon;
                } else {
                    return rtlProgress1Icon;
                }
            }
            case 2: {
                if (isLeftToRight) {
                    return progress2Icon;
                } else {
                    return rtlProgress2Icon;
                }
            }
            case 3: {
                if (isLeftToRight) {
                    return progress3Icon;
                } else {
                    return rtlProgress3Icon;
                }
            }
            case 4: {
                if (isLeftToRight) {
                    return progress4Icon;
                } else {
                    return rtlProgress4Icon;
                }
            }
            case 5: {
                if (isLeftToRight) {
                    return progress5Icon;
                } else {
                    return rtlProgress5Icon;
                }
            }
            case 6: {
                return tickIcon;
            }
            default:
                return getConfidenceIcon(confidence);
            }
        }
        
        private String getUnconfirmedConfidenceToolTip(Transaction transaction) {
            TransactionConfidence confidence = null;
            if (transaction != null) {
                confidence = transaction.getConfidence();
            }

            // Work out the line describing the is the transaction is standard or not.
            String transactionTrustfulness = "";
            if (transaction != null) {
                if (transaction.getLockTime() > 0) {
                    // Non standard transaction.
                    transactionTrustfulness = MultiBit.getController().getLocaliser().getString("multiBitFrame.status.notConfirmedAndNotStandard") + ".";
                } else {
                    // Normal transaction.
                    if (transaction != null && transaction.isCoinBase()) {
                        transactionTrustfulness = MultiBit.getController().getLocaliser().getString("multiBitFrame.status.notConfirmedAndCoinbase") + ".";  
                    } else {
                        transactionTrustfulness = MultiBit.getController().getLocaliser().getString("multiBitFrame.status.notConfirmed") + ".";  
                    }
                }
            }
            
            // Work out the line describing the number of peers.
            int peers = 0;
            if (confidence != null && confidence.getBroadcastBy() != null) {
                peers = confidence.getBroadcastBy().size();
            }
            StringBuilder builder = new StringBuilder();
            if (peers == 0) {
                builder.append(MultiBit.getController().getLocaliser()
                        .getString("transactionConfidence.seenByUnknownNumberOfPeers"));
            } else {
                builder.append(MultiBit.getController().getLocaliser().getString("transactionConfidence.seenBy") + " ");
                builder.append(peers);
                if (peers > 1)
                    builder.append(" " + MultiBit.getController().getLocaliser().getString("transactionConfidence.peers") + ". ");
                else
                    builder.append(" " + MultiBit.getController().getLocaliser().getString("transactionConfidence.peer") + ". ");
            }

            return HelpContentsPanel.createMultilineTooltipText(new String[] {
                    transactionTrustfulness, builder.toString() });
        }
        
        private ImageIcon getConfidenceIcon(TransactionConfidence confidence) {
            // By default return a triangle which indicates the least known.
            ImageIcon iconToReturn = shapeTriangleIcon;
            
            if (confidence != null) {
                if (confidence.getConfidenceType() == ConfidenceType.BUILDING) {
                    return progress0Icon;
                }
            
                if (confidence.getBroadcastBy() != null) {
                    int numberOfPeers = confidence.getBroadcastBy().size();
                    if (numberOfPeers >= 4) {
                        return progress0Icon;
                    } else {
                        switch (numberOfPeers) {
                        case 0 : iconToReturn = shapeTriangleIcon; break;
                        case 1 : iconToReturn = shapeSquareIcon; break;
                        case 2 : iconToReturn = shapeHeptagonIcon; break;
                        case 3 : iconToReturn = shapeHexagonIcon; break;
                        default:
                            iconToReturn = shapeTriangleIcon; 
                        }
                    }
                }
            }
            return iconToReturn;
        }    
    }

    class TrailingJustifiedNumericRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label;

        public TrailingJustifiedNumericRenderer() {
            label = new MultiBitLabel("");
        }
        
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(0, TABLE_BORDER, 1, TABLE_BORDER)));

            label.setText(value + SPACER);

            if ((value + "").indexOf("-") > -1) {
                // debit
                if (isSelected) {
                    label.setForeground(table.getSelectionForeground());
                } else {
                    label.setForeground(ColorAndFontConstants.DEBIT_FOREGROUND_COLOR);                    
                }
            } else {
                // debit
                if (isSelected) {
                    label.setForeground(table.getSelectionForeground()); 
                } else {
                    label.setForeground(ColorAndFontConstants.CREDIT_FOREGROUND_COLOR);                     
                }
            }
            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
            } else {
                if (row % 2 == 0) {
                    label.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                } else {
                    label.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    label.setOpaque(true);
                }
            }

            return label;
        }
    }

    class TrailingJustifiedStringRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label;

        public TrailingJustifiedStringRenderer() {
            label = new MultiBitLabel("");
        }
        
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(0, TABLE_BORDER, 1, TABLE_BORDER)));

            label.setText(value + SPACER);

            if (isSelected) {
                label.setForeground(table.getSelectionForeground());
            } else {
                label.setForeground(Color.BLACK);
            }

            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
            } else {
                if (row % 2 == 0) {
                    label.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                } else {
                    label.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    label.setOpaque(true);
                }
            }

            return label;
        }
    }
    
    class TrailingJustifiedDateRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        SimpleDateFormat dateFormatter;

        MultiBitLabel label;

        public TrailingJustifiedDateRenderer() {
            label = new MultiBitLabel("");
            dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());
        }
        
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(0, TABLE_BORDER, 1, TABLE_BORDER)));

            String formattedDate = "";
            if (value != null) {
                if (value instanceof Date) {
                    if (((Date) value).getTime() != 0) {
                        try {
                            formattedDate = dateFormatter.format(value);
                        } catch (IllegalArgumentException iae) {
                            // ok
                        }
                    }
                } else {
                    formattedDate = value.toString();
                }
            }

            label.setText(formattedDate + SPACER);

            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                label.setForeground(table.getForeground());
                if (row % 2 == 0) {
                    label.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                } else {
                    label.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    label.setOpaque(true);
                }
            }

            return label;
        }
    }

    class LeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label;

        public LeadingJustifiedRenderer() {
            label = new MultiBitLabel("");
        }
        
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(0, TABLE_BORDER, 1, TABLE_BORDER)));
            label.setText((String) value);

            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                label.setForeground(table.getForeground());
                if (row % 2 == 0) {
                    label.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                } else {
                    label.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    label.setOpaque(true);
                }
            }

            return label;
        }
    }
    
    class DecimalAlignRenderer implements TableCellRenderer {
        private final TabStop tabStopRight = new TabStop(40, TabStop.ALIGN_RIGHT, TabStop.LEAD_NONE);
        private final TabStop tabStopLeft = new TabStop(41, TabStop.ALIGN_LEFT, TabStop.LEAD_NONE);

        private final TabSet tabSet = new TabSet(new TabStop[] { tabStopRight, tabStopLeft });

        private AttributeSet attributeSet;
        private JTextPane pane;
        private Style style;

        public DecimalAlignRenderer() {
            pane = new JTextPane();

            StyleContext styleContext = StyleContext.getDefaultStyleContext();
            attributeSet = styleContext.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.TabSet, tabSet);
            pane.setParagraphAttributes(attributeSet, true);
            style = pane.addStyle("number", null);

            pane.setOpaque(true);
            pane.setBorder(BorderFactory.createEmptyBorder());
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            JPanel outerPanel = new JPanel(new BorderLayout());
            outerPanel.setOpaque(true);
            outerPanel.setBorder(BorderFactory.createEmptyBorder());

            JLabel filler = new JLabel();
            filler.setOpaque(true);

            if (value == null) {
                pane.setText("\t" + controller.getLocaliser().bitcoinValueToString(BigInteger.ZERO, false, false));
            } else {
                String contents = value.toString();
                String splitChar;
                String[] split;
                if (controller.getLocaliser().getDecimalFormatSymbols().getDecimalSeparator() == ',') {
                    // , as decimal point
                    splitChar = ",";
                    split = contents.split(",");
                } else {
                    // . as decimal point
                    splitChar = ".";
                    split = contents.split("\\.");
                }
                if (split == null) {
                    pane.setText("");
                } else if (split.length == 1) {
                    // Integer amount - no decimal point. Add a space to pad it
                    // left.
                    pane.setText("\t" + split[0] + " ");
                } else {
                    pane.setText("\t" + split[0] + splitChar + "\t" + split[1] + " ");
                }
                // log.debug("pane.getText = " + pane.getText());
            }

            if ((value.toString()).indexOf("-") > -1) {
                // debit
                if (isSelected) {
                    pane.setForeground(table.getSelectionForeground());
                } else {
                    pane.setForeground(ColorAndFontConstants.DEBIT_FOREGROUND_COLOR);
                }
            } else {
                // credit
                if (isSelected) {
                    pane.setForeground(table.getSelectionForeground());
                } else {
                    pane.setForeground(ColorAndFontConstants.CREDIT_FOREGROUND_COLOR);
                }
            }

            if (isSelected) {
                selectedRow = row;
                pane.setBackground(table.getSelectionBackground());
                // outerPanel.setBackground(table.getSelectionBackground());
                filler.setBackground(table.getSelectionBackground());
                pane.setBorder(BorderFactory.createMatteBorder(1,1,1,1, table.getSelectionBackground()));
            } else {
                if (row % 2 == 0) {
                    pane.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                    pane.setBorder(BorderFactory.createMatteBorder(1,1,1,1, ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR));
                    outerPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                    filler.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
                    outerPanel.setOpaque(true);
                } else {
                    pane.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    pane.setBorder(BorderFactory.createMatteBorder(1,1,1,1, ColorAndFontConstants.ALTERNATE_TABLE_COLOR));
                    outerPanel.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    filler.setBackground(ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
                    pane.setOpaque(true);
                    outerPanel.setOpaque(true);
                    filler.setOpaque(true);
                }
            }

            StyleConstants.setForeground(style, pane.getForeground());
            if (row % 2 == 0 || isSelected) {
                StyleConstants.setBackground(style, pane.getBackground());
            } else {
                StyleConstants.setBackground(style, ColorAndFontConstants.ALTERNATE_TABLE_COLOR);
            }
            StyleConstants.setBold(style, false);
            StyleConstants.setFontSize(style, FontSizer.INSTANCE.getAdjustedDefaultFont().getSize());
            StyleConstants.setFontFamily(style, FontSizer.INSTANCE.getAdjustedDefaultFont().getFontName());

            pane.getStyledDocument().setCharacterAttributes(0, pane.getText().length(), pane.getStyle("number"), true);

            outerPanel.add(pane, BorderLayout.LINE_START);
            outerPanel.add(filler, BorderLayout.CENTER);
            
            // Avoid flicker of first row by doing layout.
            if (row ==0) {
                outerPanel.doLayout();
            }
            return outerPanel;
        }
    }

    class TransactionPopUp extends JPopupMenu {
        private static final long serialVersionUID = 1022706046979674798L;
        JMenuItem showTransactionsDetailsMenuItem;

        public TransactionPopUp(WalletTableData rowTableData) {
            Action showTransactionDetailsAction = new ShowTransactionDetailsAction(controller, mainFrame, rowTableData);
            showTransactionsDetailsMenuItem = new JMenuItem(showTransactionDetailsAction);
            showTransactionsDetailsMenuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

            add(showTransactionsDetailsMenuItem);
        }
    }

    class PopClickListener extends MouseAdapter {
        public void mousePressed(MouseEvent e) {
            if (e.isPopupTrigger())
                doPop(e);
        }

        public void mouseReleased(MouseEvent e) {
            if (e.isPopupTrigger())
                doPop(e);
        }

        private void doPop(MouseEvent e) {
            JTable table = (JTable) (e.getSource());
            Point p = e.getPoint();
            int row = table.rowAtPoint(p);
            int col = table.columnAtPoint(p);

            // The autoscroller can generate drag events outside the Table's
            // range.
            if ((col != -1) && (row != -1)) {
                selectedRow = row;
                table.setRowSelectionInterval(row, row);
            }

            // get the transaction on the row
            WalletTableData rowTableData = walletTableModel.getRow(rowSorter.convertRowIndexToModel(row));
            TransactionPopUp menu = new TransactionPopUp(rowTableData);
            menu.show(e.getComponent(), e.getX(), e.getY());
        }
    }

    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.TRANSACTIONS_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showTransactionsAction.text");
    }

    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showTransactionsAction.tooltip");
    }

    @Override
    public int getViewId() {
        return View.TRANSACTIONS_VIEW;
    }

    @Override
    public void lostExchangeRate(ExchangeRate exchangeRate) {
        // TODO Auto-generated method stub    
    }

    @Override
    public void foundExchangeRate(ExchangeRate exchangeRate) {
        initUI();
    }

    @Override
    public void updatedExchangeRate(ExchangeRate exchangeRate) {
        ShowTransactionsPanel.updateTransactions();
    }
}