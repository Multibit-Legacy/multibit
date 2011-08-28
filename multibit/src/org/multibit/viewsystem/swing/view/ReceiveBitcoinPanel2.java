package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.TransferHandler;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;

import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBook;
import org.multibit.model.AddressBookData;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.qrcode.BitcoinURI;
import org.multibit.qrcode.ImageSelection;
import org.multibit.qrcode.QRCodeEncoderDecoder;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyQRCodeImageAction;
import org.multibit.viewsystem.swing.action.CopyQRCodeTextAction;
import org.multibit.viewsystem.swing.action.CreateOrEditAddressAction;

public class ReceiveBitcoinPanel2 extends JPanel implements DataProvider, View {

    private static final long serialVersionUID = -2065108865497842662L;

    private static final String RECEIVE_BITCOIN_BIG_ICON_FILE = "/images/receive-big.jpg";

    private MultiBitController controller;

    private JTextArea addressTextArea;

    private JTextField labelTextField;

    private JTextField amountTextField;

    private JButton copyQRCodeTextButton;

    private AddressBookTableModel receivingAddressesTableModel;

    private JTable receivingAddressesTable;

    private SelectionListener receivingAddressesListener;

    private int selectedReceivingRow;

    private JLabel qrCodeLabel;
    private JTextArea qrCodeTextArea;
    private QRCodeEncoderDecoder qrCodeEncoderDecoder;
    private static final int QRCODE_WIDTH = 140;
    private static final int QRCODE_HEIGHT = 140;

    public ReceiveBitcoinPanel2(JFrame mainFrame, MultiBitController controller) {
        this.controller = controller;

        initUI();

        loadForm();
        labelTextField.requestFocusInWindow();
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 220));
        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));
        setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1.4;
        constraints.weighty = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createReceiveBitcoinsPanel(), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.6;
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
        add(createReceivingAddressesPanel(), constraints);
    }

    private JPanel createReceiveBitcoinsPanel() {
        JPanel receiveBitcoinsPanel = new JPanel();

        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(flowLayout);

        receiveBitcoinsPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        JPanel filler1 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.weighty = 0.10;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler1, constraints);

        ImageIcon receiveBigIcon = createImageIcon(RECEIVE_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.SOUTHWEST;
        receiveBitcoinsPanel.add(new JLabel(receiveBigIcon), constraints);

        JLabel helpLabel1 = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel1, constraints);

        JLabel helpLabel2 = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel2.message"));
        helpLabel2.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel2, constraints);

        JLabel helpLabel3 = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.helpLabel3.message"));
        helpLabel3.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.08;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel3, constraints);

        JPanel filler2 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler2, constraints);

        JPanel filler3 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler3, constraints);

        JLabel addressLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel"));
        addressLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(addressLabel, constraints);

        addressTextArea = new JTextArea("", 35, 1);
        addressTextArea.setEditable(false);
        addressTextArea.setBorder(BorderFactory.createMatteBorder(0, 4, 0, 4, this.getBackground()));
        addressTextArea.setMinimumSize(new Dimension(110, 18));
        addressTextArea.setMaximumSize(new Dimension(110, 18));

        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 2;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(addressTextArea, constraints);

        JLabel labelLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel"));
        labelLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(labelLabel, constraints);

        labelTextField = new JTextField("", 35);
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        labelTextField.addKeyListener(new QRCodeKeyListener());
        constraints.gridx = 2;
        constraints.gridy = 6;
        constraints.weightx = 2;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(labelTextField, constraints);

        JLabel amountLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel"));
        amountLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.gridwidth = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(amountLabel, constraints);

        amountTextField = new JTextField("", 20);
        amountTextField.setHorizontalAlignment(JTextField.RIGHT);
        amountTextField.addKeyListener(new QRCodeKeyListener());
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(amountTextField, constraints);

        JLabel amountUnitLabel = new JLabel(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel"));
        amountUnitLabel.setToolTipText(controller.getLocaliser().getString("receiveBitcoinPanel.amountUnitLabel.tooltip"));
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 0.4;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(amountUnitLabel, constraints);

        JLabel filler4 = new JLabel("");
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 4;
        constraints.gridy = 8;
        constraints.weightx = 2.5;
        constraints.weighty = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler4, constraints);

        return receiveBitcoinsPanel;
    }

    private JPanel createQRCodePanel() {
        JPanel qrCodePanel = new JPanel();
        qrCodePanel.setMinimumSize(new Dimension(240, 200));
        qrCodePanel.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.GRAY));
        qrCodePanel.setLayout(new GridBagLayout());
        qrCodeLabel = new JLabel("", null, JLabel.CENTER);
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
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.02;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(filler2, constraints);

        qrCodeTextArea = new JTextArea(3, 40);
        qrCodeTextArea.setEditable(false);
        qrCodeTextArea.setLineWrap(true);
        qrCodeTextArea.setMinimumSize(new Dimension(220, 60));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 1;
        constraints.weighty = 6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(qrCodeTextArea, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 1;
        constraints.weighty = 0.4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        qrCodePanel.add(createQRCodeButtonPanel(), constraints);

        JPanel filler3 = new JPanel();
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

    private JPanel createReceivingAddressesPanel() {
        JPanel receiveAddressPanel = new JPanel();
        receiveAddressPanel.setOpaque(false);

        receiveAddressPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));

        // get the stored previously selected receive address

        receiveAddressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        receivingAddressesTableModel = new AddressBookTableModel(controller.getLocaliser(), controller.getModel()
                .getAddressBook(), true);
        receivingAddressesTable = new JTable(receivingAddressesTableModel);
        receivingAddressesTable.setOpaque(false);
        receivingAddressesTable.setShowGrid(false);
        receivingAddressesTable.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        receivingAddressesTable.setRowSelectionAllowed(true);
        receivingAddressesTable.setColumnSelectionAllowed(false);
        // TODO make sure table cannot be edited by double click

        TableColumn tableColumn = receivingAddressesTable.getColumnModel().getColumn(0); // label
        tableColumn.setPreferredWidth(40);

        tableColumn = receivingAddressesTable.getColumnModel().getColumn(1); // address
        tableColumn.setPreferredWidth(120);

        JLabel titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(controller.getLocaliser().getString("receiveBitcoinPanel.receivingAddressesTitle"));
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        titleLabel.setFont(font);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveAddressPanel.add(titleLabel, constraints);

        JComponent buttonPanel = createReceivingButtonPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveAddressPanel.add(buttonPanel, constraints);

        JScrollPane scrollPane = new JScrollPane(receivingAddressesTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        receiveAddressPanel.add(scrollPane, constraints);

        // add on a selection listener
        receivingAddressesListener = new SelectionListener(receivingAddressesTable, true);
        receivingAddressesTable.getSelectionModel().addListSelectionListener(receivingAddressesListener);

        return receiveAddressPanel;
    }

    private JPanel createReceivingButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CreateOrEditAddressAction createNewReceivingAddressAction = new CreateOrEditAddressAction(controller, true, true, this);
        JButton createNewButton = new JButton(createNewReceivingAddressAction);
        buttonPanel.add(createNewButton);

        CreateOrEditAddressAction editReceivingAddressAction = new CreateOrEditAddressAction(controller, false, true, this);
        JButton editButton = new JButton(editReceivingAddressAction);
        buttonPanel.add(editButton);

        return buttonPanel;
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
        Item receiveAddressItem = new Item(MultiBitModel.RECEIVE_ADDRESS);
        receiveAddressItem.setNewValue(addressTextArea.getText());
        data.addItem(MultiBitModel.RECEIVE_ADDRESS, receiveAddressItem);

        Item receiveLabelItem = new Item(MultiBitModel.RECEIVE_LABEL);
        receiveLabelItem.setNewValue(labelTextField.getText());
        data.addItem(MultiBitModel.RECEIVE_LABEL, receiveLabelItem);

        Item uriTextItem = new Item(MultiBitModel.RECEIVE_URI_TEXT);
        uriTextItem.setNewValue(qrCodeTextArea.getText());
        data.addItem(MultiBitModel.RECEIVE_URI_TEXT, uriTextItem);

        Item uriImageItem = new Item(MultiBitModel.RECEIVE_URI_IMAGE);
        uriImageItem.setNewValue(qrCodeLabel);
        data.addItem(MultiBitModel.RECEIVE_URI_IMAGE, uriImageItem);

        return data;
    }

    public void loadForm() {
        // get the current receive address and label from the model
        String receiveAddress = controller.getModel().getUserPreference(MultiBitModel.RECEIVE_ADDRESS);
        String receiveLabel = controller.getModel().getUserPreference(MultiBitModel.RECEIVE_LABEL);

        // if the currently stored address is missing or is not in this wallet,
        // pick
        // the address book's first receiving address
        boolean pickFirstReceivingAddress = false;
        if (receiveAddress == null || receiveAddress == "") {
            pickFirstReceivingAddress = true;
        } else {
            AddressBook addressBook = controller.getModel().getAddressBook();
            if (addressBook != null) {
                if (!addressBook.containsReceivingAddress(receiveAddress)) {
                    pickFirstReceivingAddress = true;
                }
            }
        }

        if (pickFirstReceivingAddress) {
            AddressBook addressBook = controller.getModel().getAddressBook();
            if (addressBook != null) {
                Set<AddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        AddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            receiveAddress = addressBookData.getAddress();
                            receiveLabel = addressBookData.getLabel();
                            controller.getModel().setUserPreference(MultiBitModel.RECEIVE_ADDRESS, receiveAddress);
                            controller.getModel().setUserPreference(MultiBitModel.RECEIVE_LABEL, receiveLabel);
                        }
                    }
                }
            }
        }

        if (receiveAddress != null) {
            addressTextArea.setText(receiveAddress);
        }
        if (receiveLabel != null) {
            labelTextField.setText(receiveLabel);
        }
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    protected ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("ReceiveBitcoinPanel#createImageIcon: Could not find file: " + path);
            return null;
        }
    }

    public String getDescription() {
        return controller.getLocaliser().getString("receiveBitcoinPanel.title");
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not used in this viewSystem

    }

    public void displayView() {
        loadForm();
        selectRows();
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        // TODO Auto-generated method stub

    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
    }

    /**
     * select the rows that correspond to the current receiving and sending data
     */
    public void selectRows() {
        // stop listener firing
        receivingAddressesTable.getSelectionModel().removeListSelectionListener(receivingAddressesListener);

        String receiveAddress = controller.getModel().getUserPreference(MultiBitModel.RECEIVE_ADDRESS);
        displayQRCode(BitcoinURI.convertToBitcoinURI(receiveAddress, amountTextField.getText(), labelTextField.getText()));

        // see if the current receive address is on the table and select it
        int receivingRowToSelect = receivingAddressesTableModel.findRowByAddress(receiveAddress, true);
        if (receivingRowToSelect >= 0) {
            receivingAddressesTable.getSelectionModel().setSelectionInterval(receivingRowToSelect, receivingRowToSelect);
            selectedReceivingRow = receivingRowToSelect;
        }

        // put the listeners back
        receivingAddressesTable.getSelectionModel().addListSelectionListener(receivingAddressesListener);
    }

    /**
     * display the specified string as a QRCode
     */
    private void displayQRCode(String stringToDisplay) {
        if (qrCodeEncoderDecoder == null) {
            qrCodeEncoderDecoder = new QRCodeEncoderDecoder(QRCODE_WIDTH, QRCODE_HEIGHT);
        }
        BufferedImage image = qrCodeEncoderDecoder.encode(stringToDisplay);
        ImageIcon icon = new ImageIcon(image);
        qrCodeLabel.setIcon(icon);
        qrCodeTextArea.setText(stringToDisplay);
    }

    class SelectionListener implements ListSelectionListener {
        JTable table;
        boolean isReceiving;

        // It is necessary to keep the table since it is not possible
        // to determine the table from the event's source
        SelectionListener(JTable table, boolean isReceiving) {
            this.table = table;
            this.isReceiving = isReceiving;
        }

        public void valueChanged(ListSelectionEvent e) {
            if (e.getSource() instanceof DefaultListSelectionModel && !e.getValueIsAdjusting()) {
                // Column selection changed
                int firstIndex = e.getFirstIndex();
                int lastIndex = e.getLastIndex();

                if (selectedReceivingRow == firstIndex) {
                    selectedReceivingRow = lastIndex;
                } else {
                    if (selectedReceivingRow == lastIndex) {
                        selectedReceivingRow = firstIndex;
                    }
                }
                AddressBookData rowData = receivingAddressesTableModel.getAddressBookDataByRow(selectedReceivingRow, true);
                if (rowData != null) {
                    controller.getModel().setUserPreference(MultiBitModel.RECEIVE_ADDRESS, rowData.getAddress());
                    controller.getModel().setUserPreference(MultiBitModel.RECEIVE_LABEL, rowData.getLabel());
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
            displayQRCode(BitcoinURI.convertToBitcoinURI(addressTextArea.getText(), amountTextField.getText(),
                    labelTextField.getText()));

        }

//        private void displayInfo(KeyEvent e, String keyStatus) {
//            // You should only rely on the key char if the event
//            // is a key typed event.
//            int id = e.getID();
//            String keyString;
//            if (id == KeyEvent.KEY_TYPED) {
//                char c = e.getKeyChar();
//                keyString = "key character = '" + c + "'";
//            } else {
//                int keyCode = e.getKeyCode();
//                keyString = "key code = " + keyCode + " (" + KeyEvent.getKeyText(keyCode) + ")";
//            }
//
//            int modifiersEx = e.getModifiersEx();
//            String modString = "extended modifiers = " + modifiersEx;
//            String tmpString = KeyEvent.getModifiersExText(modifiersEx);
//            if (tmpString.length() > 0) {
//                modString += " (" + tmpString + ")";
//            } else {
//                modString += " (no extended modifiers)";
//            }
//
//            String actionString = "action key? ";
//            if (e.isActionKey()) {
//                actionString += "YES";
//            } else {
//                actionString += "NO";
//            }
//
//            String locationString = "key location: ";
//            int location = e.getKeyLocation();
//            if (location == KeyEvent.KEY_LOCATION_STANDARD) {
//                locationString += "standard";
//            } else if (location == KeyEvent.KEY_LOCATION_LEFT) {
//                locationString += "left";
//            } else if (location == KeyEvent.KEY_LOCATION_RIGHT) {
//                locationString += "right";
//            } else if (location == KeyEvent.KEY_LOCATION_NUMPAD) {
//                locationString += "numpad";
//            } else { // (location == KeyEvent.KEY_LOCATION_UNKNOWN)
//                locationString += "unknown";
//            }
//        }
    }
}
