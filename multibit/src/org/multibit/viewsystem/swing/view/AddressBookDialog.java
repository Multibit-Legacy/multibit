package org.multibit.viewsystem.swing.view;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.lang.reflect.Method;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;

import org.multibit.Localiser;
import org.multibit.viewsystem.swing.CreateEditAddressDialog;

/*
 * dialog displaying the address book
 */
public class AddressBookDialog extends JDialog {
    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.4D;

    private static final long serialVersionUID = 7123413615342923041L;

    private Localiser localiser;

    private JFrame mainFrame;

    private JDialog thisDialog;

    private AddressBookTableModel tableModel;

    private JTabbedPane tabbedPane;

    public AddressBookDialog(Localiser localiser, JFrame mainFrame) {
        this(localiser, mainFrame, true);
    }

    public AddressBookDialog(Localiser localiser, JFrame mainFrame, boolean showReceiving) {
        this.localiser = localiser;
        this.mainFrame = mainFrame;

        this.thisDialog = this;

        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);

        setTitle(localiser.getString("addressBookFrame.title"));

        sizeAndCenter();

        initUI();

        pack();

        if (showReceiving) {
            tabbedPane.setSelectedIndex(0);
        } else {
            tabbedPane.setSelectedIndex(1);
        }
        setVisible(true);
    }

    private void sizeAndCenter() {
        // get the screen size as a java dimension
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        int height = (int) (screenSize.height * PROPORTION_OF_SCREEN_TO_FILL);
        int width = (int) (screenSize.width * PROPORTION_OF_SCREEN_TO_FILL);

        // set the jframe height and width
        setPreferredSize(new Dimension(width, height));
        double startPositionRatio = (1 - PROPORTION_OF_SCREEN_TO_FILL) / 2;
        setLocation((int) (width * startPositionRatio), (int) (height * startPositionRatio));

        // TODO remember screen size and position in config file
    }

    private void initUI() {
        positionDialogRelativeToParent(this, 0.25D, 0.1D);
        setMinimumSize(new Dimension(300, 400));

        Container contentPane = getContentPane();
        contentPane.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JComponent tabPane = createTabPane();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.92;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(tabPane, constraints);

        JComponent buttonPanel = createButtonPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.08;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(buttonPanel, constraints);
    }

    private JTabbedPane createTabPane() {
        tabbedPane = new JTabbedPane();

        JComponent panel1 = createReceivingAddressesPanel();
        tabbedPane.addTab(localiser.getString("addressBookDialog.receivingAddressesTabText"), null,
                panel1, "");
        tabbedPane.setMnemonicAt(0, KeyEvent.VK_1);

        JComponent panel2 = createSendingAddressesPanel();
        tabbedPane.addTab(localiser.getString("addressBookDialog.sendingAddressesTabText"), null,
                panel2, "");
        tabbedPane.setMnemonicAt(1, KeyEvent.VK_2);

        return tabbedPane;
    }

    private JPanel createReceivingAddressesPanel() {
        JPanel receiveAddressPanel = new JPanel();
        receiveAddressPanel.setOpaque(false);

        receiveAddressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        tableModel = new AddressBookTableModel(localiser, true);
        JTable table = new JTable(tableModel);
        table.setOpaque(false);
        table.setShowGrid(false);
        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);

        // label left justified
        // /table.getColumnModel().getColumn(0).setCellRenderer(new
        // LeftJustifiedRenderer());

        // address left justified
        // table.getColumnModel().getColumn(1).setCellRenderer(new
        // LeftJustifiedRenderer());

        TableColumn tableColumn = table.getColumnModel().getColumn(0); // label
        tableColumn.setPreferredWidth(40);

        tableColumn = table.getColumnModel().getColumn(1); // address
        tableColumn.setPreferredWidth(120);

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        receiveAddressPanel.add(scrollPane, constraints);

        return receiveAddressPanel;
    }

    private JPanel createSendingAddressesPanel() {
        JPanel sendAddressPanel = new JPanel();
        sendAddressPanel.setOpaque(false);

        sendAddressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        tableModel = new AddressBookTableModel(localiser, false);
        JTable table = new JTable(tableModel);
        table.setOpaque(false);
        table.setShowGrid(false);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);
        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);

        // label left justified
        // table.getColumnModel().getColumn(0).setCellRenderer(new
        // LeftJustifiedRenderer());

        // address left justified
        // table.getColumnModel().getColumn(1).setCellRenderer(new
        // LeftJustifiedRenderer());

        TableColumn tableColumn = table.getColumnModel().getColumn(0); // label
        tableColumn.setPreferredWidth(40);

        tableColumn = table.getColumnModel().getColumn(1); // address
        tableColumn.setPreferredWidth(120);

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        sendAddressPanel.add(scrollPane, constraints);

        return sendAddressPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        JButton copyAddressButton = new JButton();
        copyAddressButton.setText(localiser.getString("addressBookDialog.copyAddressButton"));
        copyAddressButton.setToolTipText(localiser
                .getString("addressBookDialog.copyAddressButton.tooltip"));
        copyAddressButton.addActionListener(new CopyAddressButtonListener());
        buttonPanel.add(copyAddressButton);

        JButton createNewButton = new JButton();
        createNewButton.setText(localiser.getString("addressBookDialog.createNewButton"));
        createNewButton.setToolTipText(localiser
                .getString("addressBookDialog.createNewButton.tooltip"));
        createNewButton.addActionListener(new CreateNewButtonListener());
        buttonPanel.add(createNewButton);

        JButton editButton = new JButton();
        editButton.setText(localiser.getString("addressBookDialog.editButton"));
        editButton.setToolTipText(localiser.getString("addressBookDialog.editButton.tooltip"));
        editButton.addActionListener(new EditButtonListener());
        buttonPanel.add(editButton);

        JButton okButton = new JButton();
        okButton.setText(localiser.getString("addressBookDialog.okButton"));
        okButton.setToolTipText(localiser.getString("addressBookDialog.okButton.tooltip"));
        okButton.addActionListener(new OkButtonListener());

        buttonPanel.add(okButton);

        return buttonPanel;
    }

    /**
     * Positions the specified dialog at a position relative to its parent.
     * 
     * @param dialog
     *            the dialog to be positioned.
     * @param horizontalPercent
     *            the relative location.
     * @param verticalPercent
     *            the relative location.
     */
    private void positionDialogRelativeToParent(final JDialog dialog,
            final double horizontalPercent, final double verticalPercent) {
        final Dimension d = dialog.getSize();
        final Dimension p = mainFrame.getSize();

        final int baseX = mainFrame.getX() - d.width;
        final int baseY = mainFrame.getY() - d.height;
        final int w = d.width + p.width;
        final int h = d.height + p.height;
        int x = baseX + (int) (horizontalPercent * w);
        int y = baseY + (int) (verticalPercent * h);

        // make sure the dialog fits completely on the screen...
        final Rectangle s = getMaximumWindowBounds();
        x = Math.min(x, (s.width - d.width));
        x = Math.max(x, 0);
        y = Math.min(y, (s.height - d.height));
        y = Math.max(y, 0);

        dialog.setBounds(x + s.x, y + s.y, d.width, d.height);

    }

    /**
     * Computes the maximum bounds of the current screen device. If this method
     * is called on JDK 1.4, Xinerama-aware results are returned. (See
     * Sun-Bug-ID 4463949 for details).
     * 
     * @return the maximum bounds of the current screen.
     */
    private Rectangle getMaximumWindowBounds() {
        final GraphicsEnvironment localGraphicsEnvironment = GraphicsEnvironment
                .getLocalGraphicsEnvironment();
        try {
            final Method method = GraphicsEnvironment.class.getMethod("getMaximumWindowBounds",
                    (Class[]) null);
            return (Rectangle) method.invoke(localGraphicsEnvironment, (Object[]) null);
        } catch (Exception e) {
            // ignore ... will fail if this is not a JDK 1.4 ..
        }

        final Dimension s = Toolkit.getDefaultToolkit().getScreenSize();
        return new Rectangle(0, 0, s.width, s.height);
    }

    class RightJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus, int row, int column) {
            label.setHorizontalAlignment(SwingConstants.RIGHT);
            label.setOpaque(false);

            label.setText((String) value);

            return label;
        }
    }

    // class LeftJustifiedRenderer extends DefaultTableCellRenderer {
    // private static final long serialVersionUID = 1549545L;
    //
    // JLabel label = new JLabel();
    //
    // public Component getTableCellRendererComponent(JTable table, Object
    // value,
    // boolean isSelected, boolean hasFocus, int row, int column) {
    // label.setHorizontalAlignment(SwingConstants.LEFT);
    // label.setOpaque(false);
    //
    //
    // label.setText((String) value);
    //
    // return label;
    // }
    // }
    //
    // class CenterJustifiedRenderer extends DefaultTableCellRenderer {
    // private static final long serialVersionUID = 1549545L;
    //
    // JLabel label = new JLabel();
    //
    // public Component getTableCellRendererComponent(JTable table, Object
    // value,
    // boolean isSelected, boolean hasFocus, int row, int column) {
    // label.setHorizontalAlignment(SwingConstants.CENTER);
    // label.setOpaque(false);
    //
    // label.setText((String) value);
    //
    // return label;
    // }
    // }
    //
    private class OkButtonListener implements ActionListener {
        OkButtonListener() {
        }

        public void actionPerformed(ActionEvent e) {
            thisDialog.setVisible(false);
        }
    }

    private class CopyAddressButtonListener implements ActionListener {
        CopyAddressButtonListener() {
        }

        public void actionPerformed(ActionEvent e) {
            JOptionPane.showMessageDialog(thisDialog, "TODO - Copy Address");
        }
    }

    private class CreateNewButtonListener implements ActionListener {
        CreateNewButtonListener() {
        }

        public void actionPerformed(ActionEvent e) {
            CreateEditAddressDialog createAddressDialog = null;
            if (tabbedPane.getSelectedIndex() == 0) {
                createAddressDialog = new CreateEditAddressDialog(mainFrame, localiser,
                        "addressBookDialog.createReceivingAddressDialog.title",
                        "addressBookDialog.createReceivingAddressDialog.helpTextKey1",
                        "addressBookDialog.createReceivingAddressDialog.helpTextKey2");
            } else {
                createAddressDialog = new CreateEditAddressDialog(mainFrame, localiser,
                        "addressBookDialog.createSendingAddressDialog.title",
                        "addressBookDialog.createSendingAddressDialog.helpTextKey1",
                        "addressBookDialog.createSendingAddressDialog.helpTextKey2");
            }
            createAddressDialog.setVisible(true);
        }
    }

    private class EditButtonListener implements ActionListener {
        EditButtonListener() {
        }

        public void actionPerformed(ActionEvent e) {
            JOptionPane.showMessageDialog(thisDialog, "TODO - Edit Address And Label");
        }
    }
}