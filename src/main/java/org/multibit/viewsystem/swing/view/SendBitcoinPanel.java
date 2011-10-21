package org.multibit.viewsystem.swing.view;

import com.google.bitcoin.core.Address;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.*;
import org.multibit.qrcode.BitcoinURI;
import org.multibit.qrcode.QRCodeEncoderDecoder;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopySendAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewSendingAddressAction;
import org.multibit.viewsystem.swing.action.PasteAddressAction;
import org.multibit.viewsystem.swing.action.SendBitcoinConfirmAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.StringTokenizer;

public class SendBitcoinPanel extends JPanel implements DataProvider, View {

    private static final long serialVersionUID = -2065108865497111662L;

    private final Logger log = LoggerFactory.getLogger(SendBitcoinPanel.class);

    private static final String SEND_BITCOIN_BIG_ICON_FILE = "/images/send-big.jpg";
    private static final String DRAG_HERE_ICON_FILE = "/images/dragHere.png";

    private MultiBitFrame mainFrame;
    private MultiBitController controller;

    private JTextField addressTextField;

    private JTextField labelTextField;

    private JTextField amountTextField;

    private JPanel formPanel;

    private AddressBookTableModel addressesTableModel;

    private JTable addressesTable;

    private SelectionListener addressesListener;

    private int selectedAddressRow;

    private JPanel qrCodePanel;
    private JLabel qrCodeLabel;
    private static final int QRCODE_WIDTH = 140;
    private static final int QRCODE_HEIGHT = 140;

    private static final int MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING = 30;
    private static final int MINIMUM_QRCODE_PANEL_VERTICAL_SPACING = 80;
    
    public SendBitcoinPanel(MultiBitFrame mainFrame, MultiBitController controller) {
        this.mainFrame = mainFrame;
        this.controller = controller;

        initUI();
        loadForm();

        labelTextField.requestFocusInWindow();
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 220));
        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));
        setLayout(new GridBagLayout());
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.5;
        constraints.weighty = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createFormPanel(), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.5;
        constraints.weighty = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createQRCodePanel(), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1.0;
        constraints.weighty = 1.2;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createAddressesPanel(), constraints);
    }

    private JPanel createFormPanel() {
        formPanel = new JPanel();
        formPanel.setBorder(new DashedBorder());
        formPanel.setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);

        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(flowLayout);

        formPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 5;
        constraints.weighty = 0.10;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler1, constraints);

        ImageIcon bigIcon = createImageIcon(SEND_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        formPanel.add(new JLabel(bigIcon), constraints);

        JLabel helpLabel1 = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel1, constraints);

        JLabel helpLabel2 = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.helpLabel2.message"));
        helpLabel2.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel2, constraints);

        JLabel helpLabel3 = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.helpLabel3.message"));
        helpLabel3.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel3, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler2, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler3, constraints);

        JLabel addressLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 4.0;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(addressLabel, constraints);

        addressTextField = new JTextField(35);
        addressTextField.setHorizontalAlignment(JTextField.LEFT);
        addressTextField.setMinimumSize(new Dimension(MultiBitFrame.WIDTH_OF_LONG_FIELDS, 24));
        addressTextField.setMaximumSize(new Dimension(MultiBitFrame.WIDTH_OF_LONG_FIELDS, 24));

        addressTextField.addKeyListener(new QRCodeKeyListener());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(addressTextField, constraints);

        ImageIcon copyIcon = createImageIcon(MultiBitFrame.COPY_ICON_FILE);
        CopySendAddressAction copyAddressAction = new CopySendAddressAction(controller, this, copyIcon);
        JButton copyAddressButton = new JButton(copyAddressAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 5;
        constraints.weightx = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(copyAddressButton, constraints);

        ImageIcon pasteIcon = createImageIcon(MultiBitFrame.PASTE_ICON_FILE);
        PasteAddressAction pasteAddressAction = new PasteAddressAction(controller, this, pasteIcon);
        JButton pasteAddressButton = new JButton(pasteAddressAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 5;
        constraints.gridy = 5;
        constraints.weightx = 9;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(pasteAddressButton, constraints);

        JLabel labelLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(labelLabel, constraints);

        labelTextField = new JTextField("", 35);
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        labelTextField.setMinimumSize(new Dimension(MultiBitFrame.WIDTH_OF_LONG_FIELDS, 24));
        labelTextField.setMaximumSize(new Dimension(MultiBitFrame.WIDTH_OF_LONG_FIELDS, 24));

        labelTextField.addKeyListener(new QRCodeKeyListener());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 6;
        constraints.weightx = 0.15;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(labelTextField, constraints);

        JLabel amountLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel"));
        amountLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.gridwidth = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.30;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(amountLabel, constraints);

        amountTextField = new JTextField("", 20);
        amountTextField.setHorizontalAlignment(JTextField.RIGHT);
        amountTextField.setMinimumSize(new Dimension(MultiBitFrame.WIDTH_OF_AMOUNT_FIELD, 24));
        amountTextField.setMaximumSize(new Dimension(MultiBitFrame.WIDTH_OF_AMOUNT_FIELD, 24));
        amountTextField.addKeyListener(new QRCodeKeyListener());

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.1;
        constraints.weighty = 0.30;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(amountTextField, constraints);

        JLabel amountUnitLabel = new JLabel(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel"));
        amountUnitLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel.tooltip"));
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 2.0;
        constraints.weighty = 0.30;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(amountUnitLabel, constraints);

        SendBitcoinConfirmAction sendBitcoinConfirmAction = new SendBitcoinConfirmAction(controller, this);
        JButton sendButton = new JButton(sendBitcoinConfirmAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 8;
        constraints.weightx = 10;
        constraints.weighty = 0.4;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(sendButton, constraints);

        return formPanel;
    }

    private JPanel createQRCodePanel() {
        qrCodePanel = new JPanel();
        qrCodePanel.setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);

        qrCodePanel.setMinimumSize(new Dimension(280, 200));
        qrCodePanel.setLayout(new GridBagLayout());
        qrCodeLabel = new JLabel("", createImageIcon(DRAG_HERE_ICON_FILE), JLabel.CENTER);
        qrCodeLabel.setMinimumSize(new Dimension(QRCODE_WIDTH, QRCODE_HEIGHT));
        qrCodeLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.dragBitcoinLabel.tooltip"));

        qrCodeLabel.setVerticalTextPosition(JLabel.BOTTOM);
        qrCodeLabel.setHorizontalTextPosition(JLabel.CENTER);

        // copy image support
        qrCodeLabel.setTransferHandler(new ImageSelection());

        // drag support
        MouseListener listener = new MouseAdapter() {
            public void mousePressed(MouseEvent me) {
                JComponent comp = (JComponent) me.getSource();
                TransferHandler handler = comp.getTransferHandler();
                handler.exportAsDrag(comp, me, TransferHandler.COPY);
            }
        };
        qrCodeLabel.addMouseListener(listener);

        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(filler1, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(qrCodeLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.01;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        qrCodePanel.add(filler2, constraints);

        return qrCodePanel;
    }

    private JPanel createAddressesPanel() {
        JPanel addressPanel = new JPanel();
        addressPanel.setBackground(MultiBitFrame.BACKGROUND_COLOR);
        addressPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));

        // get the stored previously selected send address

        addressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        addressesTableModel = new AddressBookTableModel(controller, false);
        addressesTable = new JTable(addressesTableModel);
        addressesTable.setOpaque(true);
        addressesTable.setShowGrid(false);
        addressesTable.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        addressesTable.setRowSelectionAllowed(true);
        addressesTable.setColumnSelectionAllowed(false);
        // TODO make sure table cannot be edited by double click

        TableColumn tableColumn = addressesTable.getColumnModel().getColumn(0); // label
        tableColumn.setPreferredWidth(40);
        // label left justified
        tableColumn.setCellRenderer(new LeftJustifiedRenderer());

        tableColumn = addressesTable.getColumnModel().getColumn(1); // address
        tableColumn.setPreferredWidth(120);
        // addresses left justified
        tableColumn.setCellRenderer(new LeftJustifiedRenderer());

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressPanel.add(createAddressesHeaderPanel(), constraints);

        JScrollPane scrollPane = new JScrollPane(addressesTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.getViewport().setBackground(MultiBitFrame.BACKGROUND_COLOR);
        scrollPane.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker()));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        addressPanel.add(scrollPane, constraints);

        // add on a selection listener
        addressesListener = new SelectionListener();
        addressesTable.getSelectionModel().addListSelectionListener(addressesListener);

        return addressPanel;
    }

    private JPanel createAddressesHeaderPanel() {
        JPanel addressesHeaderPanel = new AddressesPanel();
        addressesHeaderPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        CreateNewSendingAddressAction createNewSendingAddressAction = new CreateNewSendingAddressAction(controller, this);
        JButton createNewButton = new JButton(createNewSendingAddressAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(createNewButton, constraints);

        JLabel titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.sendingAddressesTitle"));
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        titleLabel.setFont(font);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(titleLabel, constraints);

        return addressesHeaderPanel;
    }

    public Data getData() {
        Data data = new Data();
        Item addressItem = new Item(MultiBitModel.SEND_ADDRESS);
        addressItem.setNewValue(addressTextField.getText());
        data.addItem(MultiBitModel.SEND_ADDRESS, addressItem);

        Item labelItem = new Item(MultiBitModel.SEND_LABEL);
        labelItem.setNewValue(labelTextField.getText());
        data.addItem(MultiBitModel.SEND_LABEL, labelItem);

        Item amountItem = new Item(MultiBitModel.SEND_AMOUNT);
        amountItem.setNewValue(amountTextField.getText());
        data.addItem(MultiBitModel.SEND_AMOUNT, amountItem);

        return data;
    }

    public void loadForm() {
        // get the current address, label and amount from the model
        String address = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        String label = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
        String amount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);

        if (address != null) {
            addressTextField.setText(address);
        }
        if (label != null) {
            labelTextField.setText(label);
        }
        if (amount != null) {
            amountTextField.setText(amount);
        }
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    protected ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            log.error("SendBitcoinPanel#createImageIcon: Could not find file: " + path);
            return null;
        }
    }

    public String getDescription() {
        return controller.getLocaliser().getString("sendBitcoinPanel.title");
    }

    public void displayView() {
        loadForm();
        selectRows();
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
    }

    /**
     * select the rows that correspond to the current data
     */
    public void selectRows() {
        // stop listener firing
        addressesTable.getSelectionModel().removeListSelectionListener(addressesListener);

        String address = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);

        // see if the current address is on the table and select it
        int rowToSelect = addressesTableModel.findRowByAddress(address, false);
        if (rowToSelect >= 0) {
            addressesTable.getSelectionModel().setSelectionInterval(rowToSelect, rowToSelect);
            selectedAddressRow = rowToSelect;
        }

        // scroll to visible
        addressesTable.scrollRectToVisible(addressesTable.getCellRect(rowToSelect, 0, false));
        // put the listeners back
        addressesTable.getSelectionModel().addListSelectionListener(addressesListener);
    }

    class SelectionListener implements ListSelectionListener {
        SelectionListener() {
        }

        public void valueChanged(ListSelectionEvent e) {
            if (e.getSource() instanceof DefaultListSelectionModel && !e.getValueIsAdjusting()) {
                // Column selection changed
                int firstIndex = e.getFirstIndex();
                int lastIndex = e.getLastIndex();

                if (selectedAddressRow == firstIndex) {
                    selectedAddressRow = lastIndex;
                } else {
                    if (selectedAddressRow == lastIndex) {
                        selectedAddressRow = firstIndex;
                    }
                }
                AddressBookData rowData = addressesTableModel.getAddressBookDataByRow(selectedAddressRow, false);
                if (rowData != null) {
                    controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, rowData.getAddress());
                    controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, rowData.getLabel());
                    addressTextField.setText(rowData.getAddress());
                    labelTextField.setText(rowData.getLabel());
                }
            }
        }
    }

    class QRCodeKeyListener implements KeyListener {
        /** Handle the key typed event from the text field. */
        public void keyTyped(KeyEvent e) {
        }

        /** Handle the key-pressed event from the text field. */
        public void keyPressed(KeyEvent e) {
            // do nothing
        }

        /** Handle the key-released event from the text field. */
        public void keyReleased(KeyEvent e) {
            String address = addressTextField.getText();
            String amount = amountTextField.getText();
            String label = labelTextField.getText();
            AddressBookData addressBookData = new AddressBookData(label, address);
            addressesTableModel.setAddressBookDataByRow(addressBookData, selectedAddressRow, false);
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, address);
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, label);
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, amount);
        }
    }

    public void setAddressBookDataByRow(AddressBookData addressBookData) {
        addressTextField.setText(addressBookData.getAddress());
        addressesTableModel.setAddressBookDataByRow(addressBookData, selectedAddressRow, false);
    }

    public JTextField getLabelTextField() {
        return labelTextField;
    }

    public JPanel getFormPanel() {
        return formPanel;
    }

    class LeftJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.LEFT);
            label.setOpaque(true);

            label.setText((String) value);

            if (isSelected) {
                label.setBackground(table.getSelectionBackground());
                label.setForeground(table.getSelectionForeground());
            } else {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : MultiBitFrame.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }
  
            return label;
        }
    }

    class ImageSelection extends TransferHandler implements Transferable {
        private static final long serialVersionUID = 756395092284264645L;

        private DataFlavor flavors[];
        private DataFlavor urlFlavor, uriListAsStringFlavor, uriListAsReaderFlavor;

        private Image image;

        public int getSourceActions(JComponent c) {
            return TransferHandler.COPY;
        }

        public boolean canImport(JComponent comp, DataFlavor flavor[]) {
            if (!(comp instanceof JLabel) && !(comp instanceof AbstractButton)) {
                return false;
            }

            if (flavors == null) {
                flavors = new DataFlavor[] { DataFlavor.imageFlavor };
                try {
                    urlFlavor = new DataFlavor("application/x-java-url; class=java.net.URL");
                    uriListAsStringFlavor = new DataFlavor("text/uri-list; class=java.lang.String");
                    uriListAsReaderFlavor = new DataFlavor("text/uri-list;class=java.io.Reader");
                    flavors = new DataFlavor[] { DataFlavor.imageFlavor, urlFlavor, uriListAsStringFlavor, uriListAsReaderFlavor };
                } catch (ClassNotFoundException cnfe) {
                    cnfe.printStackTrace();
                }
            }

            for (int i = 0, n = flavor.length; i < n; i++) {
                for (int j = 0, m = flavors.length; j < m; j++) {
                    if (flavor[i].equals(flavors[j])) {
                        return true;
                    }
                }
            }
            return false;
        }

        public Transferable createTransferable(JComponent comp) {
            // Clear
            image = null;

            if (comp instanceof JLabel) {
                JLabel label = (JLabel) comp;
                Icon icon = label.getIcon();
                if (icon instanceof ImageIcon) {
                    image = ((ImageIcon) icon).getImage();
                    return this;
                }
            }
            return null;
        }

        private boolean processDecodedString(String decodedString, JLabel label, ImageIcon icon) {
            // decode the string to an AddressBookData
            BitcoinURI bitcoinURI = new BitcoinURI(controller, decodedString);

            if (bitcoinURI.isParsedOk()) {
                log.debug("SendBitcoinPanel - ping 1");
                Address address = bitcoinURI.getAddress();
                log.debug("SendBitcoinPanel - ping 2");
                String addressString = address.toString();
                log.debug("SendBitcoinPanel - ping 3");
                String amountString = amountTextField.getText();
                if (bitcoinURI.getAmount() != null) {
                    amountString = Localiser.bitcoinValueToString4(bitcoinURI.getAmount(), false, false);
                }
                log.debug("SendBitcoinPanel - ping 4");
                String decodedLabel = bitcoinURI.getLabel();

                log.debug("SendBitcoinPanel#imageSelection#importData = addressString = " + addressString + ", amountString = "
                        + amountString + ", label = " + decodedLabel);
                log.debug("SendBitcoinPanel - ping 5");

                AddressBookData addressBookData = new AddressBookData(decodedLabel, addressString);
                log.debug("SendBitcoinPanel - ping 6");
                // see if the address is already in the address book
                // see if the current address is on the table and
                // select it
                int rowToSelect = addressesTableModel.findRowByAddress(addressBookData.getAddress(), false);
                if (rowToSelect >= 0) {
                    addressesTableModel.setAddressBookDataByRow(addressBookData, rowToSelect, false);
                    addressesTable.getSelectionModel().setSelectionInterval(rowToSelect, rowToSelect);
                    selectedAddressRow = rowToSelect;
                } else {
                    // add a new row to the table
                    controller.getModel().getActiveWalletWalletInfo().addSendingAddress(addressBookData);

                    // select new row
                    rowToSelect = addressesTableModel.findRowByAddress(addressBookData.getAddress(), false);
                    if (rowToSelect >= 0) {
                        addressesTable.getSelectionModel().setSelectionInterval(rowToSelect, rowToSelect);
                        selectedAddressRow = rowToSelect;
                    }
                }
                // scroll to visible
                addressesTable.scrollRectToVisible(addressesTable.getCellRect(rowToSelect, 0, false));
                addressesTable.invalidate();
                addressesTable.validate();
                addressesTable.repaint();
                mainFrame.invalidate();
                mainFrame.validate();
                mainFrame.repaint();

                log.debug("SendBitcoinPanel - ping 7");
                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, addressString);
                log.debug("SendBitcoinPanel - ping 8");
                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, decodedLabel);
                log.debug("SendBitcoinPanel - ping 9");

                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, amountString);
                log.debug("SendBitcoinPanel - ping 10");
                addressTextField.setText(addressString);
                log.debug("SendBitcoinPanel - ping 11");
                amountTextField.setText(amountString);
                log.debug("SendBitcoinPanel - ping 12");
                labelTextField.setText(decodedLabel);
                log.debug("SendBitcoinPanel - ping 13");
                mainFrame.updateStatusLabel("");
                label.setIcon(icon);
                label.setToolTipText(decodedString);
                return true;
            } else {
                mainFrame.updateStatusLabel(controller.getLocaliser().getString("sendBitcoinPanel.couldNotUnderstandQRcode",
                        new Object[] { decodedString }));
                return false;
            }

        }

        public boolean importData(JComponent comp, Transferable transferable) {
            if (comp instanceof JLabel) {
                log.debug("importData - 1");

                JLabel label = (JLabel) comp;
                image = getDropData(transferable, label);
                log.debug("importData - 2 - image = " + image);

                if (image != null) {
                    BufferedImage bufferedImage;
                    log.debug("importData - 2.1");
                    if (image.getWidth(qrCodeLabel) + MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING > qrCodePanel.getWidth()
                            || image.getHeight(qrCodeLabel) + MINIMUM_QRCODE_PANEL_VERTICAL_SPACING > qrCodePanel.getHeight()) {
                        // scale image
                        double qrCodeWidth = (double)qrCodePanel.getWidth();
                        double qrCodeHeight = (double)qrCodePanel.getHeight();
                        double xScale = qrCodeWidth / (double)(image.getWidth(qrCodeLabel) + MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING);
                        double yScale = qrCodeHeight / (double)(image.getHeight(qrCodeLabel) + MINIMUM_QRCODE_PANEL_VERTICAL_SPACING);
                        double scaleFactor = Math.min(xScale, yScale);
                        bufferedImage = toBufferedImage(image, (int) (image.getWidth(qrCodeLabel) * scaleFactor),
                                (int) (image.getHeight(qrCodeLabel) * scaleFactor));
                    } else {
                        // no resize
                        bufferedImage = toBufferedImage(image, -1, -1);
                    }
                    log.debug("importData - 2.2");
                    ImageIcon icon = new ImageIcon(bufferedImage);

                    // decode the QRCode to a String
                    QRCodeEncoderDecoder qrCodeEncoderDecoder = new QRCodeEncoderDecoder(image.getWidth(qrCodeLabel),
                            image.getHeight(qrCodeLabel));
                    log.debug("importData - 2.3");

                    String decodedString = qrCodeEncoderDecoder.decode(toBufferedImage(image, -1, -1));
                    log.debug("importData - 3 - decodedResult = " + decodedString);
                    log.info("SendBitcoinPanel#imageSelection#importData = decodedString = {}", decodedString);
                    return processDecodedString(decodedString, label, icon);
                }
            }
            return false;
        }

        @SuppressWarnings("rawtypes")
        private Image getDropData(Transferable transferable, JComponent label) {
            try {
                // try to get an image
                if (transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
                    log.debug("image flavor is supported");
                    Image img = (Image) transferable.getTransferData(DataFlavor.imageFlavor);
                    if (img != null && img.getWidth(null) != -1) {
                        return img;
                    }
                }
                if (transferable.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                    log.debug("javaFileList is supported");
                    java.util.List list = (java.util.List) transferable.getTransferData(DataFlavor.javaFileListFlavor);
                    for (Object aList : list) {
                    File f = (File) aList;
                    ImageIcon icon = new ImageIcon(f.getAbsolutePath());
                    if (icon.getImage() != null) {
                      return icon.getImage();
                    }
                  }
                }
                
                if (transferable.isDataFlavorSupported(uriListAsStringFlavor)) {
                    log.debug("uriListAsStringFlavor is supported");
                    String uris = (String) transferable.getTransferData(uriListAsStringFlavor);

                    // url-lists are defined by rfc 2483 as crlf-delimited
                    // TODO iterate over list for all of them
                    StringTokenizer izer = new StringTokenizer(uris, "\r\n");
                    if (izer.hasMoreTokens()) {
                        String uri = izer.nextToken();
                        log.debug("uri = " + uri);
                        java.awt.Image image = getURLImage(new URL(uri));

                        if (image != null) {
                            return image;
                        }
                        
                        ImageIcon uriIcon = new ImageIcon(uri);
                      if(uriIcon.getImage() != null) {
                          return uriIcon.getImage();
                      }
                    }
                }

                
                if (transferable.isDataFlavorSupported(uriListAsReaderFlavor)) {
                    log.debug("uriListAsReaderFlavor is supported");

                    BufferedReader read = new BufferedReader(uriListAsReaderFlavor.getReaderForText(transferable));
                    // Remove 'file://' from file name
                    String fileName = read.readLine().substring(7).replace("%20", " ");
                    // Remove 'localhost' from OS X file names
                    if (fileName.substring(0, 9).equals("localhost")) {
                        fileName = fileName.substring(9);
                    }
                    read.close();

                    java.awt.Image image = getFileImage(new File(fileName));

                    if (image != null) {
                        return image;
                    }
                }
                

                if (transferable.isDataFlavorSupported(urlFlavor)) {
                    log.debug("urlFlavor is supported");
                    URL url = (URL) transferable.getTransferData(urlFlavor);
                    log.debug("url = " + url);
                    java.awt.Image image = getURLImage(url);

                    if (image != null) {
                        return image;
                    }
                    
                    ImageIcon urlIcon = new ImageIcon(url);
                  if (urlIcon.getImage() != null) {
                      return urlIcon.getImage();
                  }
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            } catch (UnsupportedFlavorException e) {

                e.printStackTrace();
            }
            return null;
        }

        private Image getURLImage(URL url) {
            Image imageToReturn = null;
            
            try {
               imageToReturn = ImageIO.read(url);
            } catch (IOException e) {
                e.printStackTrace();
            } 
            return imageToReturn;
        }

        private Image getFileImage(File file) {
            Image imageToReturn = null;
            
            try {
               imageToReturn = ImageIO.read(file);
            } catch (IOException e) {
                e.printStackTrace();
            } 
            return imageToReturn;
        }
        
        // Transferable
        public Object getTransferData(DataFlavor flavor) {
            if (isDataFlavorSupported(flavor)) {
                if (DataFlavor.imageFlavor.equals(flavor)) {
                    return image;
                } else {
                    if (DataFlavor.javaFileListFlavor.equals(flavor)) {
                        java.util.List<File> list = new java.util.LinkedList<File>();
                        
                        // write the image to the output stream
                        File swatchFile = new File("swatch.png");
                        try {
                            ImageIO.write( toBufferedImage(image, -1, -1), "png", new File("swatch.png"));
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                        list.add(swatchFile);
                        return list;
                      
                    }
                }
            }
            return null;
        }

        public DataFlavor[] getTransferDataFlavors() {
            return flavors;
        }

        public boolean isDataFlavorSupported(DataFlavor flavor) {
            for (int j = 0, m = flavors.length; j < m; j++) {
                if (flavor.equals(flavors[j])) {
                    return true;
                }
            }
            return false;
        }

        public BufferedImage toBufferedImage(Image image, int width, int height) {
            log.debug("SendBitCoinPanel#toBufferedImage - 1");
            if (image == null) {
                return null;
            }
            if (width == -1) {
                width = image.getWidth(null);
            }
            if (height == -1) {
                height = image.getHeight(null);
            }
            // draw original image to thumbnail image object and
            // scale it to the new size on-the-fly
            log.debug("SendBitCoinPanel#toBufferedImage - 2.2, image = " + image + ",width = " + width + ", height = "
                    + height);

            BufferedImage bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

            log.debug("SendBitCoinPanel#toBufferedImage - 2.3, bufferedImage = " + bufferedImage);

            Graphics2D g2 = bufferedImage.createGraphics();
            
            log.debug("SendBitCoinPanel#toBufferedImage - 3");
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g2.drawImage(image, 0, 0, width, height, null);
            log.debug("SendBitCoinPanel#toBufferedImage - 4");
            g2.dispose();
            return bufferedImage;
        }

        // This method returns a buffered image with the contents of an image
        public BufferedImage toBufferedImage2(Image image, int width, int height) {
            if (width == -1) {
                width = image.getWidth(null);
            }
            if (height == -1) {
                height = image.getHeight(null);
            }

            // This code ensures that all the pixels in the image are loaded
            image = new ImageIcon(image).getImage();
            log.debug("SendBitCoinPanel#toBufferedImage - 2");

            // Create a buffered image with a format that's compatible with the
            // screen
            BufferedImage bimage = null;
            GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            log.debug("SendBitCoinPanel#toBufferedImage - 2.1");
            try {
                // Determine the type of transparency of the new buffered image
                int transparency = Transparency.OPAQUE;

                // Create the buffered image
                GraphicsDevice gs = ge.getDefaultScreenDevice();
                log.debug("SendBitCoinPanel#toBufferedImage - 2.2");

                GraphicsConfiguration gc = gs.getDefaultConfiguration();
                log.debug("SendBitCoinPanel#toBufferedImage - 2.3, image = " + image + ",width = " + width
                        + ", height = " + height);

                bimage = gc.createCompatibleImage(width, height, transparency);
                log.debug("SendBitCoinPanel#toBufferedImage - 2.4");

            } catch (HeadlessException e) {
                // The system does not have a screen
            }
            log.debug("SendBitCoinPanel#toBufferedImage - 3 - bimage = " + bimage);

            if (bimage == null) {
                // Create a buffered image using the default color model
                int type = BufferedImage.TYPE_INT_RGB;
                bimage = new BufferedImage(width, height, type);
            }

            // Copy image to buffered image
            Graphics2D g = bimage.createGraphics();

            // Paint the image onto the buffered image
            g.drawImage(image, 0, 0, width, height, null);

            g.dispose();

            log.debug("SendBitCoinPanel#toBufferedImage - 4 - bimage = " + bimage);

            return bimage;
        }
    }
}
