package org.multibit.viewsystem.swing.view;

import org.multibit.controller.MultiBitController;
import org.multibit.model.*;
import org.multibit.qrcode.BitcoinURI;
import org.multibit.qrcode.QRCodeEncoderDecoder;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyQRCodeImageAction;
import org.multibit.viewsystem.swing.action.CopyQRCodeTextAction;
import org.multibit.viewsystem.swing.action.CopyReceiveAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.util.Vector;

public class ReceiveBitcoinPanel extends JPanel implements DataProvider, View {

    private static final Logger log = LoggerFactory.getLogger(ReceiveBitcoinPanel.class);

    private static final long serialVersionUID = -2065108865497842662L;

    private static final String RECEIVE_BITCOIN_BIG_ICON_FILE = "/images/receive-big.jpg";

    private MultiBitController controller;

    private JTextArea addressTextArea;

    private JTextField labelTextField;

    private JTextField amountTextField;

    private JPanel formPanel;

    private JButton copyQRCodeTextButton;

    private AddressBookTableModel addressesTableModel;

    private JTable addressesTable;

    private SelectionListener addressesListener;

    private int selectedAddressRow;

    private JLabel qrCodeLabel;
    private JTextArea qrCodeTextArea;
    private QRCodeEncoderDecoder qrCodeEncoderDecoder;
    private static final int QRCODE_WIDTH = 140;
    private static final int QRCODE_HEIGHT = 140;

    public ReceiveBitcoinPanel(JFrame mainFrame, MultiBitController controller) {
        this.controller = controller;

        initUI();
        loadForm();

        labelTextField.requestFocusInWindow();
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 220));
        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        setLayout(new GridBagLayout());

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
        constraints.weightx = 1;
        constraints.weighty = 0.10;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler1, constraints);

        ImageIcon bigIcon = createImageIcon(RECEIVE_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        formPanel.add(new JLabel(bigIcon), constraints);

        JLabel helpLabel1 = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel1, constraints);

        JLabel helpLabel2 = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel2.message"));
        helpLabel2.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel2, constraints);

        JLabel helpLabel3 = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel3.message"));
        helpLabel3.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(helpLabel3, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler2, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler3, constraints);

        JLabel addressLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 1;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        formPanel.add(addressLabel, constraints);

        addressTextArea = new JTextArea("", 35, 1);
        addressTextArea.setEditable(false);
        addressTextArea.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(0, 4, 0, 4, MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR),
                BorderFactory.createMatteBorder(2, 0, 0, 0, Color.WHITE)));
        addressTextArea.setMinimumSize(new Dimension(MultiBitFrame.WIDTH_OF_LONG_FIELDS, 24));
        addressTextArea.setMaximumSize(new Dimension(MultiBitFrame.WIDTH_OF_LONG_FIELDS, 24));
        

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.15;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(addressTextArea, constraints);

        ImageIcon copyIcon = createImageIcon(MultiBitFrame.COPY_ICON_FILE);
        CopyReceiveAddressAction copyAddressAction = new CopyReceiveAddressAction(controller, this, copyIcon);
        JButton copyAddressButton = new JButton(copyAddressAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 5;
        constraints.weightx = 10;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(copyAddressButton, constraints);

   
        JLabel labelLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
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
        constraints.weighty = 0.20;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(labelTextField, constraints);

        JLabel amountLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel"));
        amountLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.RIGHT);
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
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

        JLabel amountUnitLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel"));
        amountUnitLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel.tooltip"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 2.0;
        constraints.weighty = 0.30;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(amountUnitLabel, constraints);

        JLabel filler4 = new JLabel("");
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 8;
        constraints.weightx = 10;
        constraints.weighty = 1.0;
        constraints.anchor = GridBagConstraints.LINE_START;
        formPanel.add(filler4, constraints);

        return formPanel;
    }

    private JPanel createQRCodePanel() {
        JPanel qrCodePanel = new JPanel();
        qrCodePanel.setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);
        qrCodePanel.setMinimumSize(new Dimension(280, 200));
        qrCodePanel.setLayout(new GridBagLayout());
        qrCodeLabel = new JLabel("", null, JLabel.CENTER);
        qrCodeLabel.setVerticalTextPosition(JLabel.BOTTOM);
        qrCodeLabel.setHorizontalTextPosition(JLabel.CENTER);

        // copy image support
        qrCodeLabel.setTransferHandler(new ImageSelection(false));

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
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.02;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        qrCodePanel.add(filler1, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(qrCodeLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.02;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(filler2, constraints);

        qrCodeTextArea = new JTextArea(2, 40);
        qrCodeTextArea.setEditable(false);
        qrCodeTextArea.setLineWrap(true);
         qrCodeTextArea.setFont(qrCodeTextArea.getFont().deriveFont(12.0F));
        JScrollPane scrollPane = new JScrollPane(qrCodeTextArea);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, MultiBitFrame.DARK_BACKGROUND_COLOR.darker()));

        scrollPane.setMinimumSize(new Dimension(220, 40));
       
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 1;
        constraints.weighty = 2;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(scrollPane, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 1;
        constraints.weighty = 0.4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel qrCodeButtonPanel = createQRCodeButtonPanel();
        qrCodeButtonPanel.setOpaque(false);
        qrCodePanel.add(qrCodeButtonPanel, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.05;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        qrCodePanel.add(filler3, constraints);

        return qrCodePanel;
    }

    private JPanel createAddressesPanel() {
        JPanel addressPanel = new JPanel();
        addressPanel.setOpaque(true);
        addressPanel.setBackground(MultiBitFrame.BACKGROUND_COLOR);

        addressPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));

        // get the stored previously selected receive address

        addressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        addressesTableModel = new AddressBookTableModel(controller, true);
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

        CreateNewReceivingAddressAction createNewReceivingAddressAction = new CreateNewReceivingAddressAction(controller, this);
        JButton createNewButton = new JButton(createNewReceivingAddressAction);
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
        titleLabel.setText(controller.getLocaliser().getString("receiveBitcoinPanel.receivingAddressesTitle"));
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

    private JPanel createQRCodeButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(flowLayout);

        CopyQRCodeTextAction copyQRCodeTextAction = new CopyQRCodeTextAction(controller, this);
        copyQRCodeTextButton = new JButton(copyQRCodeTextAction);
        buttonPanel.add(copyQRCodeTextButton);

        CopyQRCodeImageAction copyQRCodeImageAction = new CopyQRCodeImageAction(controller, this);
        JButton copyQRCodeImageButton = new JButton(copyQRCodeImageAction);
        buttonPanel.add(copyQRCodeImageButton);

        return buttonPanel;
    }

    public Data getData() {
        Data data = new Data();
        Item addressItem = new Item(MultiBitModel.RECEIVE_ADDRESS);
        addressItem.setNewValue(addressTextArea.getText());
        data.addItem(MultiBitModel.RECEIVE_ADDRESS, addressItem);

        Item labelItem = new Item(MultiBitModel.RECEIVE_LABEL);
        labelItem.setNewValue(labelTextField.getText());
        data.addItem(MultiBitModel.RECEIVE_LABEL, labelItem);

        Item amountItem = new Item(MultiBitModel.RECEIVE_AMOUNT);
        amountItem.setNewValue(amountTextField.getText());
        data.addItem(MultiBitModel.RECEIVE_AMOUNT, amountItem);

        Item uriTextItem = new Item(MultiBitModel.RECEIVE_URI_TEXT);
        uriTextItem.setNewValue(qrCodeTextArea.getText());
        data.addItem(MultiBitModel.RECEIVE_URI_TEXT, uriTextItem);

        Item uriImageItem = new Item(MultiBitModel.RECEIVE_URI_IMAGE);
        uriImageItem.setNewValue(qrCodeLabel);
        data.addItem(MultiBitModel.RECEIVE_URI_IMAGE, uriImageItem);

        return data;
    }

    public void loadForm() {
        // get the current address, label and amount from the model
        String address = controller.getModel().getWalletPreference(MultiBitModel.RECEIVE_ADDRESS);
        String label = controller.getModel().getWalletPreference(MultiBitModel.RECEIVE_LABEL);
        String amount = controller.getModel().getWalletPreference(MultiBitModel.RECEIVE_AMOUNT);

        // if the currently stored address is missing or is not in this wallet,
        // pick
        // the address book's first receiving address
        boolean pickFirstReceivingAddress = false;
        if (address == null || address == "") {
            pickFirstReceivingAddress = true;
        } else {
            WalletInfo addressBook = controller.getModel().getWalletInfo();
            if (addressBook != null) {
                if (!addressBook.containsReceivingAddress(address)) {
                    pickFirstReceivingAddress = true;
                }
            }
        }

        if (pickFirstReceivingAddress) {
            WalletInfo addressBook = controller.getModel().getWalletInfo();
            if (addressBook != null) {
                Vector<AddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        AddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            address = addressBookData.getAddress();
                            label = addressBookData.getLabel();
                            controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_ADDRESS, address);
                            controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_LABEL, label);
                        }
                    }
                }
            }
        }

        if (address != null) {
            addressTextArea.setText(address);
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
            log.error("ReceiveBitcoinPanel#createImageIcon: Could not find file: " + path);
            return null;
        }
    }

    public String getDescription() {
        return controller.getLocaliser().getString("receiveBitcoinPanel.title");
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

        String address = controller.getModel().getWalletPreference(MultiBitModel.RECEIVE_ADDRESS);
        displayQRCode(BitcoinURI.convertToBitcoinURI(address, amountTextField.getText(), labelTextField.getText()));

        // see if the current address is on the table and select it
        int rowToSelect = addressesTableModel.findRowByAddress(address, true);
        if (rowToSelect >= 0) {
            addressesTable.getSelectionModel().setSelectionInterval(rowToSelect, rowToSelect);
            selectedAddressRow = rowToSelect;
        }

        // scroll to visible
        addressesTable.scrollRectToVisible(addressesTable.getCellRect(rowToSelect, 0, false));
        // put the listeners back
        addressesTable.getSelectionModel().addListSelectionListener(addressesListener);
    }

    /**
     * display the specified string as a QRCode
     */
    private void displayQRCode(String stringToDisplay) {
        if (qrCodeEncoderDecoder == null) {
            qrCodeEncoderDecoder = new QRCodeEncoderDecoder(QRCODE_WIDTH, QRCODE_HEIGHT);
        }
        if (stringToDisplay != null && !stringToDisplay.equals("")) {
            try {
                BufferedImage image = qrCodeEncoderDecoder.encode(stringToDisplay);
                ImageIcon icon = new ImageIcon(image);
                qrCodeLabel.setIcon(icon);
                qrCodeTextArea.setText(stringToDisplay);
            } catch (IllegalArgumentException iae) {
                log.error(iae.getMessage(), iae);

            }
        }
    }
  
    class LeftJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                int row, int column) {
            label.setHorizontalAlignment(SwingConstants.LEFT);
            label.setBackground(MultiBitFrame.BACKGROUND_COLOR);
            label.setOpaque(true);   

            label.setText((String) value);

            if (!label.getBackground().equals(table.getSelectionBackground())) {
                Color backgroundColor = (row % 2 == 0 ? Color.WHITE : MultiBitFrame.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
            }
            return label;
        }
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
                AddressBookData rowData = addressesTableModel.getAddressBookDataByRow(selectedAddressRow, true);
                if (rowData != null) {
                    controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_ADDRESS, rowData.getAddress());
                    controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_LABEL, rowData.getLabel());
                    addressTextArea.setText(rowData.getAddress());
                    labelTextField.setText(rowData.getLabel());
                    displayQRCode(BitcoinURI.convertToBitcoinURI(rowData.getAddress(), amountTextField.getText(),
                            labelTextField.getText()));
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
            String address = addressTextArea.getText();
            String amount = amountTextField.getText();
            String label = labelTextField.getText();
            AddressBookData addressBookData = new AddressBookData(label, address);

            WalletInfo walletInfo = controller.getModel().getWalletInfo();
            if (walletInfo == null) {
                walletInfo = new WalletInfo(controller.getModel().getWalletFilename());
                controller.getModel().setWalletInfo(walletInfo);
            }
            addressesTableModel.setAddressBookDataByRow(addressBookData, selectedAddressRow, true);
            controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_ADDRESS, address);
            controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_LABEL, label);
            controller.getModel().setWalletPreference(MultiBitModel.RECEIVE_AMOUNT, amount);


            displayQRCode(BitcoinURI.convertToBitcoinURI(address, amount, label));
        }
    }

    public JTextField getLabelTextField() {
        return labelTextField;
    }

    public JPanel getFormPanel() {
        return formPanel;
    }
}
