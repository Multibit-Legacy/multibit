/**
 * Copyright 2011 multibit.org
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
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

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

    private static final int TABLE_BORDER = 3;

    private static final int MINIMUM_ICON_HEIGHT = 18;

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

    private int selectedRow = -1;
    
    public static final int UPDATE_TRANSACTIONS_DELAY_TIME = 333; // milliseconds

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
    }

    private void initUI() {
        createWalletPanel();
    }
    
    public static void updateTransactions() {
        if (updateTransactionsTimerTask != null) {
            synchronized(updateTransactionsTimerTask) {
                updateTransactionsTimerTask.setUpdateTransactions(true);                
            }
        }
    }

    private void createWalletPanel() {
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        setLayout(new GridBagLayout());
        setOpaque(true);
        GridBagConstraints constraints = new GridBagConstraints();

        walletTableModel = new WalletTableModel(controller);
        table = new JTable(walletTableModel);
        table.setOpaque(true);
        table.setBorder(BorderFactory.createEmptyBorder());
        table.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        table.setRowHeight(Math.max(MINIMUM_ICON_HEIGHT, getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()));

        // Use status icons.
        table.getColumnModel().getColumn(0).setCellRenderer(new ImageRenderer());

        // Set popup for displaying transaction contents.
        table.addMouseListener(new PopClickListener());

        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);

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
            table.getColumnModel().getColumn(2).setCellRenderer(new TrailingJustifiedRenderer());
        }

        // Amount trailing justified.
        table.getColumnModel().getColumn(3).setCellRenderer(new TrailingJustifiedRenderer());

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        TableColumn tableColumn = table.getColumnModel().getColumn(0); // status
        int statusWidth = fontMetrics.stringWidth(controller.getLocaliser().getString("walletData.statusText"));
        tableColumn.setPreferredWidth(statusWidth);

        tableColumn = table.getColumnModel().getColumn(1); // Date.
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        int dateWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("walletData.debitText")),
                fontMetrics.stringWidth(dateFormatter.format(new Date(DateUtils.nowUtc().getMillis()))));
        tableColumn.setPreferredWidth(dateWidth);

        tableColumn = table.getColumnModel().getColumn(2); // Description.
        tableColumn.setPreferredWidth(250);

        tableColumn = table.getColumnModel().getColumn(3); // Amount (BTC).
        int amountBTCWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel") + " (BTC)"),
                fontMetrics.stringWidth("000.00000000"));
        tableColumn.setPreferredWidth(amountBTCWidth);

        if (CurrencyConverter.INSTANCE.isShowingFiat()) {
            tableColumn = table.getColumnModel().getColumn(4); // Amount (fiat).
            int amountFiatWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel") + " (USD)"),
                    fontMetrics.stringWidth("000.00000000"));
            tableColumn.setPreferredWidth(amountFiatWidth);
           
            table.getColumnModel().getColumn(4).setCellRenderer(new TrailingJustifiedRenderer());
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

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        scrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        scrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);

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
        //log.debug("ShowTransactionsPanel#displayView called on panel " + System.identityHashCode(this));

        walletTableModel.recreateWalletData();

        if (selectedRow > -1 && selectedRow < table.getRowCount()) {
            table.setRowSelectionInterval(selectedRow, selectedRow);
        }

        table.invalidate();
        table.validate();
        table.repaint();
        
        invalidate();
        validate();
        repaint();
        
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

        JLabel label = new JLabel();

        ImageIcon shapeTriangleIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_TRIANGLE_ICON_FILE);
        ImageIcon shapeSquareIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_SQUARE_ICON_FILE);
        ImageIcon shapeHeptagonIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_PENTAGON_ICON_FILE);
        ImageIcon shapeHexagonIcon = ImageLoader.createImageIcon(ImageLoader.SHAPE_HEXAGON_ICON_FILE);
        ImageIcon tickIcon = ImageLoader.createImageIcon(TICK_ICON_FILE);
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
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setOpaque(true);

            TransactionConfidence confidence = (TransactionConfidence) value;

            ConfidenceType confidenceType = null;
            if (confidence != null) {
                confidenceType = confidence.getConfidenceType();
            }
            if (confidenceType == null) {
                confidenceType = ConfidenceType.UNKNOWN;
            }
            switch (confidenceType) {
            case UNKNOWN: {
                label.setText("?");
                label.setIcon(null);
                // label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.notConfirmed"));
                break;
            }
            case BUILDING: {
                if (controller.getMultiBitService().getChain() == null) {
                    label.setText("?");
                    label.setIcon(null);
                } else {
                    int numberOfBlocksEmbedded = controller.getMultiBitService().getChain().getBestChainHeight() - confidence.getAppearedAtChainHeight() + 1;
                    ImageIcon buildingIcon = getBuildingIcon(numberOfBlocksEmbedded, confidence);
                    label.setIcon(buildingIcon);
                    label.setText("");
                    if (numberOfBlocksEmbedded >= 6) {
                        label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.isConfirmed"));
                    } else {
                        label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                    }
                }
                break;
            }
            case NOT_SEEN_IN_CHAIN: {
                label.setIcon(getConfidenceIcon(confidence));
                label.setText("");
                
                label.setToolTipText(getConfidenceToolTip(confidence) );

                // label.setText("NSIC");
                break;
            }
            case NOT_IN_BEST_CHAIN: {
                label.setIcon(getConfidenceIcon(confidence));
                label.setText("");
                label.setToolTipText(getConfidenceToolTip(confidence) );
                // label.setText("NSIBC");
                break;
            }
            case DEAD: {
                label.setIcon(null);
                label.setText("DS");
                break;
            }
            default: {
                label.setIcon(null);
                label.setText("?");
                break;
            }
            }

            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
        }

        private ImageIcon getBuildingIcon(int numberOfBlocksEmbedded, TransactionConfidence confidence) {
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
        
        private String getConfidenceToolTip(TransactionConfidence confidence) {
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
                    controller.getLocaliser().getString("multiBitFrame.status.notConfirmed") + ".", builder.toString() });
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

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));

            label.setText(value + SPACER);

            if ((value + "").indexOf("-") > -1) {
                // debit
                if (isSelected) {
                    label.setForeground(ColorAndFontConstants.SELECTION_DEBIT_FOREGROUND_COLOR);
                } else {
                    label.setForeground(ColorAndFontConstants.DEBIT_FOREGROUND_COLOR);                    
                }
            } else {
                // debit
                if (isSelected) {
                    label.setForeground(ColorAndFontConstants.SELECTION_CREDIT_FOREGROUND_COLOR); 
                } else {
                    label.setForeground(ColorAndFontConstants.CREDIT_FOREGROUND_COLOR);                     
                }
            }
            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
            }

            return label;
        }
    }

    class TrailingJustifiedDateRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));

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
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
        }
    }

    class LeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
            label.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));
            label.setText((String) value);

            if (isSelected) {
                selectedRow = row;
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
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