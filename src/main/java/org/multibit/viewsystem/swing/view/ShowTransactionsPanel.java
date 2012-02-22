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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
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
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ShowTransactionsPanel extends JPanel implements DataProvider, View {

    private static final Logger log = LoggerFactory.getLogger(ShowTransactionsPanel.class);

    private static final long serialVersionUID = 1235108897887842662L;

    private MultiBitController controller;

    private JTable table;
    private WalletTableModel walletTableModel;

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

    public ShowTransactionsPanel(JFrame mainFrame, MultiBitController controller) {
        this.controller = controller;

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

        // sorter
        TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel>(table.getModel());
        table.setRowSorter(sorter);

        // sort by date descending
        List<TableRowSorter.SortKey> sortKeys = new ArrayList<TableRowSorter.SortKey>();
        sortKeys.add(new TableRowSorter.SortKey(1, SortOrder.DESCENDING));
        sorter.setSortKeys(sortKeys);
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
        sorter.setComparator(1, comparator);

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
   
    class ImageRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 154545L;

        JLabel label = new JLabel();

        ImageIcon tickIcon = MultiBitFrame.createImageIcon(TICK_ICON_FILE);
        ImageIcon progress0Icon = MultiBitFrame.createImageIcon(PROGRESS_0_ICON_FILE);
        ImageIcon progress1Icon = MultiBitFrame.createImageIcon(PROGRESS_1_ICON_FILE);
        ImageIcon progress2Icon = MultiBitFrame.createImageIcon(PROGRESS_2_ICON_FILE);
        ImageIcon progress3Icon = MultiBitFrame.createImageIcon(PROGRESS_3_ICON_FILE);
        ImageIcon progress4Icon = MultiBitFrame.createImageIcon(PROGRESS_4_ICON_FILE);
        ImageIcon progress5Icon = MultiBitFrame.createImageIcon(PROGRESS_5_ICON_FILE);
        ImageIcon rtlProgress1Icon = MultiBitFrame.createImageIcon(RTL_PROGRESS_1_ICON_FILE);
        ImageIcon rtlProgress2Icon = MultiBitFrame.createImageIcon(RTL_PROGRESS_2_ICON_FILE);
        ImageIcon rtlProgress3Icon = MultiBitFrame.createImageIcon(RTL_PROGRESS_3_ICON_FILE);
        ImageIcon rtlProgress4Icon = MultiBitFrame.createImageIcon(RTL_PROGRESS_4_ICON_FILE);
        ImageIcon rtlProgress5Icon = MultiBitFrame.createImageIcon(RTL_PROGRESS_5_ICON_FILE);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setOpaque(true);

            int numberOfBlocksEmbedded = (Integer) value;
            if (numberOfBlocksEmbedded < 0) {
                numberOfBlocksEmbedded = 0;
            }
            if (numberOfBlocksEmbedded > 6) {
                numberOfBlocksEmbedded = 6;
            }

            boolean isLeftToRight = ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight();
            switch (numberOfBlocksEmbedded) {
            case 0: {
                label.setIcon(progress0Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.notConfirmed"));
                break;
            }
            case 1: {
                if (isLeftToRight) {
                    label.setIcon(progress1Icon);
                } else {
                    label.setIcon(rtlProgress1Icon);
                }
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 2: {
                if (isLeftToRight) {
                    label.setIcon(progress2Icon);
                } else {
                    label.setIcon(rtlProgress2Icon);
                }
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 3: {
                if (isLeftToRight) {
                    label.setIcon(progress3Icon);
                } else {
                    label.setIcon(rtlProgress3Icon);
                }
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 4: {
                if (isLeftToRight) {
                    label.setIcon(progress4Icon);
                } else {
                    label.setIcon(rtlProgress4Icon);
                }
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 5: {
                if (isLeftToRight) {
                    label.setIcon(progress5Icon);
                } else {
                    label.setIcon(rtlProgress5Icon);
                }
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 6: {
                label.setIcon(tickIcon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.isConfirmed"));
                break;
            }
            default:
                label.setIcon(progress0Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.notConfirmed"));
            }

            if (!label.getBackground().equals(table.getSelectionBackground())) {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
            }
            return label;
        }
    }

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("", controller);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));

            label.setText(value + SPACER);
            if (!label.getBackground().equals(table.getSelectionBackground())) {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
            }
            return label;
        }
    }

    class TrailingJustifiedDateRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("", controller);
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
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

            if (!label.getBackground().equals(table.getSelectionBackground())) {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
            }
            return label;
        }
    }

    class LeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("", controller);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
            label.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            label.setOpaque(true);
            label.setBorder(new EmptyBorder(new Insets(1, TABLE_BORDER, 1, TABLE_BORDER)));
            label.setText((String) value);

            if (!label.getBackground().equals(table.getSelectionBackground())) {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
            }
            return label;
        }
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

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
    }

    public WalletTableModel getWalletTableModel() {
        return walletTableModel;
    }

    public JPanel getFormPanel() {
        return null;
    }

    public JTextField getLabelTextField() {
        return null;
    }
}