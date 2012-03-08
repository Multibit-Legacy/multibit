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

import javax.swing.Action;
import javax.swing.BorderFactory;
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

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.WalletTableData;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.action.ShowTransactionDetailsAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.TransactionConfidence;
import com.google.bitcoin.core.TransactionConfidence.ConfidenceType;

public class ShowTransactionsPanel extends JPanel implements DataProvider, View {

    private static final Logger log = LoggerFactory.getLogger(ShowTransactionsPanel.class);

    private static final long serialVersionUID = 1235108897887842662L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    

    private JTable table;
    private WalletTableModel walletTableModel;
    
    private  TableRowSorter<TableModel> rowSorter;

    private Data data;

    private static final String SPACER = "   "; // 3 spaces

    private static final int TABLE_BORDER = 3;

    private static final String PROGRESS_0_ICON_FILE = "/images/progress0.png";
    private static final String PROGRESS_1_ICON_FILE = "/images/progress1.png";
    private static final String PROGRESS_2_ICON_FILE = "/images/progress2.png";
    private static final String PROGRESS_3_ICON_FILE = "/images/progress3.png";
    private static final String PROGRESS_4_ICON_FILE = "/images/progress4.png";
    private static final String PROGRESS_5_ICON_FILE = "/images/progress5.png";
    private static final String RTL_PROGRESS_1_ICON_FILE = "/images/rtl_progress1.png";
    private static final String RTL_PROGRESS_2_ICON_FILE = "/images/rtl_progress2.png";
    private static final String RTL_PROGRESS_3_ICON_FILE = "/images/rtl_progress3.png";
    private static final String RTL_PROGRESS_4_ICON_FILE = "/images/rtl_progress4.png";
    private static final String RTL_PROGRESS_5_ICON_FILE = "/images/rtl_progress5.png";
    private static final String TICK_ICON_FILE = "/images/tick.png";

    public ShowTransactionsPanel(MultiBitFrame mainFrame, MultiBitController controller) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        data = new Data();

        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    private void initUI() {
        createWalletPanel();
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
        table.setRowHeight(getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight());

        // use status icons
        table.getColumnModel().getColumn(0).setCellRenderer(new ImageRenderer());

        // set popup for displaying transaction contents
        table.addMouseListener(new PopClickListener());

        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);

        // date right justified
        table.getColumnModel().getColumn(1).setCellRenderer(new TrailingJustifiedDateRenderer());

        // justify column headers
        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);
        table.getTableHeader().setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        // description leading justified (set explicitly as it does not seem to
        // work otherwise)
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            table.getColumnModel().getColumn(2).setCellRenderer(new LeadingJustifiedRenderer());
        } else {
            table.getColumnModel().getColumn(2).setCellRenderer(new TrailingJustifiedRenderer());
        }

        // credit and debit trailing justified
        table.getColumnModel().getColumn(3).setCellRenderer(new TrailingJustifiedRenderer());
        table.getColumnModel().getColumn(4).setCellRenderer(new TrailingJustifiedRenderer());

        TableColumn tableColumn = table.getColumnModel().getColumn(0); // status
        tableColumn.setPreferredWidth(35);

        tableColumn = table.getColumnModel().getColumn(1); // date
        tableColumn.setPreferredWidth(85);

        tableColumn = table.getColumnModel().getColumn(2); // description
        tableColumn.setPreferredWidth(320);

        tableColumn = table.getColumnModel().getColumn(3); // debit
        tableColumn.setPreferredWidth(40);

        tableColumn = table.getColumnModel().getColumn(4); // credit
        tableColumn.setPreferredWidth(40);

        // row sorter
        rowSorter = new TableRowSorter<TableModel>(table.getModel());
        table.setRowSorter(rowSorter);

        // sort by date descending
        List<TableRowSorter.SortKey> sortKeys = new ArrayList<TableRowSorter.SortKey>();
        sortKeys.add(new TableRowSorter.SortKey(1, SortOrder.DESCENDING));
        rowSorter.setSortKeys(sortKeys);
        Comparator<Date> comparator = new Comparator<Date>() {
            public int compare(Date o1, Date o2) {
                long n1 = o1.getTime();
                long n2 = o2.getTime();
                if (n1 == 0) {
                    // object 1 has missing date
                    return 1;
                }
                if (n2 == 0) {
                    // object 2 has missing date
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

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        add(scrollPane, constraints);
    }

    public Data getData() {
        return data;
    }

    @Override
    public void displayView() {
        walletTableModel.recreateWalletData();
        table.invalidate();
        table.validate();
        table.repaint();
    }

    @Override
    public void updateView() {
        walletTableModel.recreateWalletData();

        table.invalidate();
        table.validate();
        table.repaint();
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
                int numberOfBlocksEmbedded = confidence.getDepthInBlocks(controller.getMultiBitService().getChain());
                ImageIcon buildingIcon = getBuildingIcon(numberOfBlocksEmbedded);
                label.setIcon(buildingIcon);
                label.setText("");
                if (numberOfBlocksEmbedded >= 6) {
                    label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.isConfirmed"));
                } else {
                    label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                }
                break;
            }
            case NOT_SEEN_IN_CHAIN: {
                label.setIcon(progress0Icon);
                label.setText("");
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.notConfirmed"));

                // label.setText("NSIC");
                break;
            }
            case NOT_IN_BEST_CHAIN: {
                label.setIcon(progress0Icon);
                label.setText("");
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.notConfirmed"));
                // label.setText("NSIBC");
                break;
            }
            case OVERRIDDEN_BY_DOUBLE_SPEND: {
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
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
        }

        private ImageIcon getBuildingIcon(int numberOfBlocksEmbedded) {
            if (numberOfBlocksEmbedded < 0) {
                numberOfBlocksEmbedded = 0;
            }
            if (numberOfBlocksEmbedded > 6) {
                numberOfBlocksEmbedded = 6;
            }

            boolean isLeftToRight = ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight();

            switch (numberOfBlocksEmbedded) {
            case 0: {
                return progress0Icon;
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
                return progress0Icon;
            }
        }
    }

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("", controller);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));

            label.setText(value + SPACER);

            if (isSelected) {
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
        }
    }

    class TrailingJustifiedDateRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("", controller);
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));

            String formattedDate = "";
            if (value != null) {
                if (value instanceof Date) {
                    if (((Date) value).getTime() == 0) {
                        // date is actually missing - just keep a blank string
                    } else {
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
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
        }
    }

    class LeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("", controller);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
            label.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));
            label.setText((String) value);

            if (isSelected) {
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }

            return label;
        }
    }

    class PopUpDemo extends JPopupMenu {
        private static final long serialVersionUID = 1022706046979674798L;
        JMenuItem showTransactionsDetailsMenuItem;

        
        public PopUpDemo(WalletTableData rowTableData) {
            Action showTransactionDetailsAction = new ShowTransactionDetailsAction(controller, mainFrame, rowTableData);
            showTransactionsDetailsMenuItem = new JMenuItem(showTransactionDetailsAction);
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

            // The autoscroller can generate drag events outside the Table's range.
            if ((col == -1) || (row == -1)) {
                // do nothing
            } else {
                table.setRowSelectionInterval(row, row);
            }
            
            // get the transaction on the row
            WalletTableData rowTableData = walletTableModel.getRow(rowSorter.convertRowIndexToModel(row));
            PopUpDemo menu = new PopUpDemo(rowTableData);
            menu.show(e.getComponent(), e.getX(), e.getY());
        }
    }
}