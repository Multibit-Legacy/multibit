package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.watermark.FillPainter;
import org.multibit.viewsystem.swing.watermark.WatermarkPainter;
import org.multibit.viewsystem.swing.watermark.WatermarkViewport;

public class ShowTransactionsPanel extends JPanel implements DataProvider, View {

    private static final long serialVersionUID = 1235108897887842662L;
    
    private MultiBitController controller;
    
    private JTable table;
    private WalletTableModel walletTableModel;

    private Data data;
       
    private static final String SPACER = "   "; // 3 spaces

    private static final String PROGRESS_0_ICON_FILE = "/images/progress0.jpg";
    private static final String PROGRESS_1_ICON_FILE = "/images/progress1.jpg";
    private static final String PROGRESS_2_ICON_FILE = "/images/progress2.jpg";
    private static final String PROGRESS_3_ICON_FILE = "/images/progress3.jpg";
    private static final String PROGRESS_4_ICON_FILE = "/images/progress4.jpg";
    private static final String PROGRESS_5_ICON_FILE = "/images/progress5.jpg";
    private static final String TICK_ICON_FILE = "/images/tick.jpg";

    public ShowTransactionsPanel(JFrame mainFrame, MultiBitController controller) {
        this.controller = controller;
     
        data = new Data();

        initUI();
    }

    private void initUI() {
        createWalletPanel();
    }


    private void createWalletPanel() {
        setOpaque(false);

        setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        walletTableModel = new WalletTableModel(controller);
        table = new JTable(walletTableModel);
        table.setOpaque(false);
        table.setShowGrid(false);

        // use status icons
        table.getColumnModel().getColumn(0).setCellRenderer(new ImageRenderer());

        // date right justified
        table.getColumnModel().getColumn(1).setCellRenderer(new RightJustifiedDateRenderer());

        // center column headers
        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);

        // description left justified
        table.getColumnModel().getColumn(2).setCellRenderer(new LeftJustifiedRenderer());

        // credit and debit right justified
        table.getColumnModel().getColumn(3).setCellRenderer(new RightJustifiedRenderer());
        table.getColumnModel().getColumn(4).setCellRenderer(new RightJustifiedRenderer());

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
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        WatermarkPainter bgPainter = new FillPainter();
        WatermarkViewport vp = new WatermarkViewport(bgPainter, null);
        vp.setView(table);
        scrollPane.setViewport(vp);

        add(scrollPane, constraints);
    }
 
    public Data getData() {
        return data;
    }


    class ImageRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 154545L;

        JLabel label = new JLabel();

        ImageIcon tickIcon = createImageIcon(TICK_ICON_FILE);
        ImageIcon progress0Icon = createImageIcon(PROGRESS_0_ICON_FILE);
        ImageIcon progress1Icon = createImageIcon(PROGRESS_1_ICON_FILE);
        ImageIcon progress2Icon = createImageIcon(PROGRESS_2_ICON_FILE);
        ImageIcon progress3Icon = createImageIcon(PROGRESS_3_ICON_FILE);
        ImageIcon progress4Icon = createImageIcon(PROGRESS_4_ICON_FILE);
        ImageIcon progress5Icon = createImageIcon(PROGRESS_5_ICON_FILE);

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setOpaque(false);

            int numberOfBlocksEmbedded = ((Integer) value).intValue();
            if (numberOfBlocksEmbedded < 0) {
                numberOfBlocksEmbedded = 0;
            }
            if (numberOfBlocksEmbedded > 6) {
                numberOfBlocksEmbedded = 6;
            }

            switch (numberOfBlocksEmbedded) {
            case 0: {
                label.setIcon(progress0Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.notConfirmed"));
                break;
            }
            case 1: {
                label.setIcon(progress1Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 2: {
                label.setIcon(progress2Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 3: {
                label.setIcon(progress3Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 4: {
                label.setIcon(progress4Icon);
                label.setToolTipText(controller.getLocaliser().getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 5: {
                label.setIcon(progress5Icon);
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
            return label;
        }
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("org.multibit.ViewerFrame#createImageIcon: Could not find file: " + path);
            return null;
        }
    }
    
    class RightJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.RIGHT);
            label.setOpaque(false);

            label.setText((String) value + SPACER);

            return label;
        }
    }

    class RightJustifiedDateRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.RIGHT);
            label.setOpaque(false);

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

            return label;
        }
    }

    class LeftJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.LEFT);
            label.setOpaque(false);

            label.setText((String) value);

            return label;
        }
    }

    class CenterJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setOpaque(false);

            label.setText((String) value);

            return label;
        }
    }

    public String getDescription() {
        // not required in Swing View
        return null;
    }

    public void displayView() {
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
}