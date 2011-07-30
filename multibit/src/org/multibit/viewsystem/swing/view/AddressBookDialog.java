package org.multibit.viewsystem.swing.view;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.viewsystem.swing.action.CopyAddressAction;
import org.multibit.viewsystem.swing.action.CreateOrEditAddressAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;

/*
 * dialog displaying the address book
 */
public class AddressBookDialog extends MultiBitDialog implements DataProvider {
    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.4D;

    private static final long serialVersionUID = 7123413615342923041L;

    private MultiBitController controller;

    private AddressBookTableModel tableModel;

    private JTabbedPane tabbedPane;

    public AddressBookDialog(MultiBitController controller, JFrame mainFrame) {
        this(controller, mainFrame, true);
    }

    public AddressBookDialog(MultiBitController controller, JFrame mainFrame, boolean isReceiving) {
        super(mainFrame);
        this.controller = controller;

        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);

        setTitle(controller.getLocaliser().getString("addressBookDialog.title"));

        sizeAndCenter();

        initUI();

        pack();

        displayTab(isReceiving);

        final MultiBitController finalController = controller;
        tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                if (tabbedPane.getSelectedIndex() == 0) {
                    // now on receiving
                    finalController
                            .setActionForwardToSibling(ActionForward.FORWARD_TO_ADDRESS_BOOK_RECEIVING);
                } else {
                    // now on sending
                    finalController
                            .setActionForwardToSibling(ActionForward.FORWARD_TO_ADDRESS_BOOK_SENDING);
                }
            }
        });

        setVisible(true);
    }

    public void displayTab(boolean isReceiving) {
        if (tabbedPane != null) {
            if (isReceiving) {
                tabbedPane.setSelectedIndex(0);
            } else {
                tabbedPane.setSelectedIndex(1);
            }
        }
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
    }

    private JTabbedPane createTabPane() {
        tabbedPane = new JTabbedPane();

        JComponent panel1 = createReceivingAddressesPanel();
        tabbedPane.addTab(
                controller.getLocaliser().getString("addressBookDialog.receivingAddressesTabText"),
                null, panel1, "");
        tabbedPane.setMnemonicAt(0, KeyEvent.VK_1);

        JComponent panel2 = createSendingAddressesPanel();
        tabbedPane.addTab(
                controller.getLocaliser().getString("addressBookDialog.sendingAddressesTabText"),
                null, panel2, "");
        tabbedPane.setMnemonicAt(1, KeyEvent.VK_2);
        return tabbedPane;
    }

    private JPanel createReceivingAddressesPanel() {
        JPanel receiveAddressPanel = new JPanel();
        receiveAddressPanel.setOpaque(false);

        receiveAddressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        tableModel = new AddressBookTableModel(controller.getLocaliser(), true);
        JTable table = new JTable(tableModel);
        table.setOpaque(false);
        table.setShowGrid(false);
        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);

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

        JComponent buttonPanel = createReceivingButtonPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.08;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveAddressPanel.add(buttonPanel, constraints);

        return receiveAddressPanel;
    }

    private JPanel createSendingAddressesPanel() {
        JPanel sendAddressPanel = new JPanel();
        sendAddressPanel.setOpaque(false);

        sendAddressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        tableModel = new AddressBookTableModel(controller.getLocaliser(), false);
        JTable table = new JTable(tableModel);
        table.setOpaque(false);
        table.setShowGrid(false);
        table.setRowSelectionAllowed(true);
        table.setColumnSelectionAllowed(false);
        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);

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

        JComponent buttonPanel = createSendingButtonPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.08;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendAddressPanel.add(buttonPanel, constraints);

        return sendAddressPanel;
    }

    private JPanel createReceivingButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CopyAddressAction copyAddressAction = new CopyAddressAction(controller, this);
        JButton copyAddressButton = new JButton(copyAddressAction);
        buttonPanel.add(copyAddressButton);

        CreateOrEditAddressAction createNewReceivingAddressAction = new CreateOrEditAddressAction(
                controller, true, true, this);
        JButton createNewButton = new JButton(createNewReceivingAddressAction);
        buttonPanel.add(createNewButton);

        CreateOrEditAddressAction editReceivingAddressAction = new CreateOrEditAddressAction(
                controller, false, true, this);
        JButton editButton = new JButton(editReceivingAddressAction);
        buttonPanel.add(editButton);

        OkBackToParentAction okBackToParentAction = new OkBackToParentAction(controller);
        JButton okButton = new JButton(okBackToParentAction);

        buttonPanel.add(okButton);

        return buttonPanel;
    }

    private JPanel createSendingButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CopyAddressAction copyAddressAction = new CopyAddressAction(controller, this);
        JButton copyAddressButton = new JButton(copyAddressAction);
        buttonPanel.add(copyAddressButton);

        CreateOrEditAddressAction createNewSendingAddressAction = new CreateOrEditAddressAction(
                controller, true, false, this);
        JButton createNewButton = new JButton(createNewSendingAddressAction);
        buttonPanel.add(createNewButton);

        CreateOrEditAddressAction editSendingAddressAction = new CreateOrEditAddressAction(
                controller, false, false, this);
        JButton editButton = new JButton(editSendingAddressAction);
        buttonPanel.add(editButton);

        OkBackToParentAction okBackToParentAction = new OkBackToParentAction(controller);
        JButton okButton = new JButton(okBackToParentAction);

        buttonPanel.add(okButton);

        return buttonPanel;
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

    public Data getData() {
        // TODO return the currently selected address so that it can be edited,
        // transfered to other views etc
        return null;
    }
}