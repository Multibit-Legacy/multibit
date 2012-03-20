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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.qrcode.BitcoinURI;
import org.multibit.qrcode.QRCodeEncoderDecoder;
import org.multibit.qrcode.SwatchGenerator;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyQRCodeImageAction;
import org.multibit.viewsystem.swing.action.CopyQRCodeTextAction;
import org.multibit.viewsystem.swing.action.MnemonicUtil;
import org.multibit.viewsystem.swing.action.PasteSwatchAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.components.VerticalGradientPanel;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;

/**
 * Abstract parent class for SendBitcoinPanel and ReceiveBitcoinPanel
 * 
 * @author jim
 * 
 */
public abstract class AbstractTradePanel extends JPanel implements View, DataProvider {

    public boolean isShowSidePanel() {
        return showSidePanel;
    }

    public void setShowSidePanel(boolean showSidePanel) {
        this.showSidePanel = showSidePanel;
    }

    private static final long serialVersionUID = 7227169670412230264L;

    private static final Logger log = LoggerFactory.getLogger(AbstractTradePanel.class);

    private static final int MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING = 30;
    private static final int MINIMUM_QRCODE_PANEL_VERTICAL_SPACING = 80;

    private static final int TABLE_BORDER = 3;

    protected MultiBitFrame mainFrame;

    protected MultiBitController controller;

    protected MultiBitTextArea labelTextArea;

    protected MultiBitTextField amountTextField;

    protected JPanel formPanel;

    protected AddressBookTableModel addressesTableModel;

    protected JTable addressesTable;

    protected MultiBitTextField addressTextField;
    protected MultiBitTextArea addressTextArea;

    protected int selectedAddressRow;

    protected SelectionListener addressesListener;

    protected MultiBitButton createNewButton;

    protected JLabel titleLabel;

    protected JPanel qrCodePanel;
    protected MultiBitLabel qrCodeLabel;

    protected static final int QRCODE_WIDTH = 140;
    protected static final int QRCODE_HEIGHT = 140;

    protected static final int TEXTFIELD_VERTICAL_DELTA = 6;

    private final int STENT_DELTA = 4;

    protected MultiBitButton copyQRCodeTextButton;
    protected MultiBitButton pasteSwatchButton;;

    protected MultiBitButton sidePanelButton;
    protected boolean showSidePanel = false;

    private final AbstractTradePanel thisAbstractTradePanel;

    private SwatchGenerator swatchGenerator;

    /**
     * map that maps one of the key constants in this class to the actual key to
     * use for localisation
     * 
     * this map is filled up in the constructors of the concrete impls
     */
    protected Map<String, String> localisationKeyConstantToKeyMap;

    protected String ADDRESSES_TITLE = "addressesTitle";
    protected String CREATE_NEW_TOOLTIP = "createNewTooltip";

    public AbstractTradePanel(MultiBitFrame mainFrame, MultiBitController controller) {
        this.mainFrame = mainFrame;
        this.controller = controller;
        this.thisAbstractTradePanel = this;

        setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(2 * ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));

        localisationKeyConstantToKeyMap = new HashMap<String, String>();
        populateLocalisationMap();

        initUI();

        labelTextArea.requestFocusInWindow();
        
        displaySidePanel();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * is it the receive bitcoin panel (return true) or the send bitcoin panel
     * (return false)
     */
    protected abstract boolean isReceiveBitcoin();

    protected abstract String getAddressConstant();

    protected abstract String getLabelConstant();

    protected abstract String getAmountConstant();

    protected abstract String getUriImageConstant();

    protected abstract Action getCreateNewAddressAction();

    /**
     * method for concrete impls to populate the localisation map
     */
    protected abstract void populateLocalisationMap();

    /**
     * get the layout stent for all the keys on the left hand side of the panel
     */
    protected int calculateStentWidth() {
        String[] keys = new String[] { "sendBitcoinPanel.addressLabel", "sendBitcoinPanel.labelLabel",
                "sendBitcoinPanel.amountLabel", "receiveBitcoinPanel.addressLabel", "receiveBitcoinPanel.labelLabel",
                "receiveBitcoinPanel.amountLabel" };

        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this) + STENT_DELTA;

        return stentWidth;
    }

    /**
     * get a localisation string - the key varies according to the concrete impl
     */
    protected String getLocalisationString(String keyConstant, Object[] data) {
        String stringToReturn = "";
        // get the localisation key
        if (localisationKeyConstantToKeyMap != null && keyConstant != null) {
            String key = localisationKeyConstantToKeyMap.get(keyConstant);
            stringToReturn = controller.getLocaliser().getString(key, data);
        }
        return stringToReturn;
    }

    protected void initUI() {
        setMinimumSize(new Dimension(550, 220));
        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));
        setLayout(new GridBagLayout());
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        String showSidePanelText = controller.getModel().getUserPreference(MultiBitModel.SHOW_SIDE_PANEL);
        if (Boolean.TRUE.toString().equals(showSidePanelText)) {
            showSidePanel = true;
        }

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

    public void displaySidePanel() {
        if (sidePanelButton == null) {
            return;
        }

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        if (showSidePanel) {
            // show less
            sidePanelButton.setIcon(ImageLoader.createImageIcon(ImageLoader.ARROW_RIGHT_ICON_FILE));
            sidePanelButton.setText(controller.getLocaliser().getString("sendBitcoinPanel.showLess.text"));
            sidePanelButton.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.showLess.tooltip"));
            sidePanelButton.setMnemonic(mnemonicUtil.getMnemonic(controller.getLocaliser().getString(
                    "sendBitcoinPanel.showLess.mnemonic")));
            sidePanelButton.setVerticalTextPosition(JLabel.BOTTOM);
            sidePanelButton.setHorizontalTextPosition(JLabel.LEFT);

            if (qrCodePanel != null) {
                qrCodePanel.setVisible(true);
            }
        } else {
            // show more
            sidePanelButton.setIcon(ImageLoader.createImageIcon(ImageLoader.ARROW_LEFT_ICON_FILE));
            sidePanelButton.setText(controller.getLocaliser().getString("sendBitcoinPanel.showMore.text"));
            sidePanelButton.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.showMore.tooltip"));
            sidePanelButton.setMnemonic(mnemonicUtil.getMnemonic(controller.getLocaliser().getString(
                    "sendBitcoinPanel.showMore.mnemonic")));
            sidePanelButton.setVerticalTextPosition(JLabel.BOTTOM);
            sidePanelButton.setHorizontalTextPosition(JLabel.RIGHT);

            if (qrCodePanel != null) {
                qrCodePanel.setVisible(false);
            }
        }
    }

    protected abstract JPanel createFormPanel();

    protected abstract void loadForm();

    protected JPanel createAddressesHeaderPanel() {
        JPanel addressesHeaderPanel = new VerticalGradientPanel();

        addressesHeaderPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel("");
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(filler1, constraints);

        createNewButton = new MultiBitButton(getCreateNewAddressAction(), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(createNewButton, constraints);

        titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(getLocalisationString(ADDRESSES_TITLE, null));
        titleLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        addressesHeaderPanel.add(titleLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.6;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(filler2, constraints);

        return addressesHeaderPanel;
    }

    protected JPanel createAddressesPanel() {
        JPanel addressPanel = new JPanel();
        addressPanel.setOpaque(true);
        addressPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        addressPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));

        // get the stored previously selected receive address

        addressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        addressesTableModel = new AddressBookTableModel(controller, isReceiveBitcoin());
        addressesTable = new JTable(addressesTableModel);
        addressesTable.setOpaque(true);
        addressesTable.setShowGrid(false);
        addressesTable.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        addressesTable.setRowSelectionAllowed(true);
        addressesTable.setColumnSelectionAllowed(false);
        addressesTable.setRowHeight(getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight());

        // TODO make sure table cannot be edited by double click
        // justify column headers
        TableCellRenderer renderer = addressesTable.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            label.setHorizontalAlignment(JLabel.LEFT);
        } else {
            label.setHorizontalAlignment(JLabel.RIGHT);
        }
        addressesTable.getTableHeader().setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        TableColumn tableColumn = addressesTable.getColumnModel().getColumn(0); // label
        tableColumn.setPreferredWidth(40);
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            tableColumn.setCellRenderer(new LeftJustifiedRenderer());
        } else {
            tableColumn.setCellRenderer(new RightJustifiedRenderer());
        }

        // description leading justified (set explicitly as it does not seem to
        // work otherwise)
        tableColumn = addressesTable.getColumnModel().getColumn(1); // address
        tableColumn.setPreferredWidth(120);
        // addresses leading justified (set explicitly as it does not seem to
        // work otherwise)
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            tableColumn.setCellRenderer(new LeftJustifiedRenderer());
        } else {
            tableColumn.setCellRenderer(new RightJustifiedRenderer());
        }

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.05;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressPanel.add(createAddressesHeaderPanel(), constraints);

        JScrollPane scrollPane = new JScrollPane(addressesTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.setViewportBorder(BorderFactory.createEmptyBorder());
        scrollPane.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker()));

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
                AddressBookData rowData = addressesTableModel.getAddressBookDataByRow(selectedAddressRow,
                        thisAbstractTradePanel.isReceiveBitcoin());
                if (rowData != null) {
                    controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAddressConstant(),
                            rowData.getAddress());
                    controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getLabelConstant(), rowData.getLabel());
                    if (addressTextArea != null) {
                        addressTextArea.setText(rowData.getAddress());
                    }
                    if (addressTextField != null) {
                        addressTextField.setText(rowData.getAddress());
                    }
                    labelTextArea.setText(rowData.getLabel());

                    displaySwatch(rowData.getAddress(), amountTextField.getText(), labelTextArea.getText());
                }
            }
        }
    }

    class LeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
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

    class LeftJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549115L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEFT);
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

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1999545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
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

    class RightJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 2299545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.RIGHT);
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

    protected JPanel createQRCodePanel() {
        qrCodePanel = new JPanel();
        qrCodePanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        qrCodePanel.setOpaque(true);
        qrCodePanel.setMinimumSize(new Dimension(280, 200));
        qrCodePanel.setLayout(new GridBagLayout());
        qrCodeLabel = new MultiBitLabel("", JLabel.CENTER);
        qrCodeLabel.setMinimumSize(new Dimension(QRCODE_WIDTH, QRCODE_HEIGHT));

        qrCodeLabel.setVerticalTextPosition(JLabel.BOTTOM);
        qrCodeLabel.setHorizontalTextPosition(JLabel.CENTER);
        qrCodeLabel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        qrCodeLabel.setOpaque(true);

        if (!isReceiveBitcoin()) {
            qrCodeLabel.setText(controller.getLocaliser().getString("sendBitcoinPanel.dragBitcoinLabel.text"));
            qrCodeLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.dragBitcoinLabel.tooltip"));
        }

        // copy/ drag image support
        if (isReceiveBitcoin()) {
            qrCodeLabel.setTransferHandler(new ImageSelection(this, false));
        } else {
            qrCodeLabel.setTransferHandler(new ImageSelection(this, true));
        }

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
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        qrCodePanel.add(filler1, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;

        JScrollPane scrollPane = new JScrollPane(qrCodeLabel);
        scrollPane.setOpaque(true);
        scrollPane.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.setViewportBorder(BorderFactory.createEmptyBorder());

        scrollPane.setMinimumSize(new Dimension(200, 160));

        qrCodePanel.add(scrollPane, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 1;
        constraints.weighty = 0.4;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
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

    protected JPanel createQRCodeButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEADING);
        buttonPanel.setLayout(flowLayout);

        CopyQRCodeTextAction copyQRCodeTextAction = new CopyQRCodeTextAction(controller, this);
        copyQRCodeTextButton = new MultiBitButton(copyQRCodeTextAction, controller);
        buttonPanel.add(copyQRCodeTextButton);

        CopyQRCodeImageAction copyQRCodeImageAction = new CopyQRCodeImageAction(controller, this);
        MultiBitButton copyQRCodeImageButton = new MultiBitButton(copyQRCodeImageAction, controller);
        buttonPanel.add(copyQRCodeImageButton);

        if (!isReceiveBitcoin()) {
            PasteSwatchAction pasteSwatchAction = new PasteSwatchAction(controller, this);
            pasteSwatchButton = new MultiBitButton(pasteSwatchAction, controller);
            buttonPanel.add(pasteSwatchButton);
        }

        return buttonPanel;
    }

    @Override
    public void displayView() {
        loadForm();
        selectRows();

        // disable any new changes if another process has changed the wallet
        if (controller.getModel().getActivePerWalletModelData() != null
                && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
            // files have been changed by another process - disallow edits
            mainFrame.setUpdatesStoppedTooltip(labelTextArea);
            labelTextArea.setEditable(false);
            labelTextArea.setEnabled(false);
            mainFrame.setUpdatesStoppedTooltip(amountTextField);
            amountTextField.setEditable(false);
            amountTextField.setEnabled(false);

            if (createNewButton != null) {
                createNewButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(createNewButton);
            }
            if (pasteSwatchButton != null) {
                pasteSwatchButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(pasteSwatchButton);
            }
        } else {
            labelTextArea.setToolTipText(null);
            labelTextArea.setEditable(true);
            labelTextArea.setEnabled(true);
            amountTextField.setToolTipText(null);
            amountTextField.setEditable(true);
            amountTextField.setEnabled(true);
            if (createNewButton != null) {
                createNewButton.setEnabled(true);
                createNewButton.setToolTipText(getLocalisationString(CREATE_NEW_TOOLTIP, null));
            }
            if (pasteSwatchButton != null) {
                pasteSwatchButton.setEnabled(true);
                pasteSwatchButton.setToolTipText(controller.getLocaliser().getString("pasteSwatchAction.tooltip"));
            }

        }
    }

    @Override
    public void navigateAwayFromView() {
    }

    protected class QRCodeKeyListener implements KeyListener {
        /** Handle the key typed event from the text field. */
        public void keyTyped(KeyEvent e) {
        }

        /** Handle the key-pressed event from the text field. */
        public void keyPressed(KeyEvent e) {
            // do nothing
        }

        /** Handle the key-released event from the text field. */
        public void keyReleased(KeyEvent e) {
            String address = null;
            if (addressTextArea != null) {
                address = addressTextArea.getText();
            }
            if (addressTextField != null) {
                address = addressTextField.getText();
            }
            String amount = amountTextField.getText();
            String label = labelTextArea.getText();
            AddressBookData addressBookData = new AddressBookData(label, address);

            WalletInfo walletInfo = controller.getModel().getActiveWalletWalletInfo();
            if (walletInfo == null) {
                walletInfo = new WalletInfo(controller.getModel().getActiveWalletFilename());
                controller.getModel().setActiveWalletInfo(walletInfo);
            }
            address = WhitespaceTrimmer.trim(address);
            addressesTableModel.setAddressBookDataByRow(addressBookData, selectedAddressRow, isReceiveBitcoin());
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAddressConstant(), address);
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getLabelConstant(), label);
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAmountConstant(), amount);
            controller.getModel().getActivePerWalletModelData().setDirty(true);

            displaySwatch(address, amount, label);
        }
    }

    /**
     * display the address, amount and label as a swatch
     */
    private void displaySwatch(String address, String amount, String label) {
        if (swatchGenerator == null) {
            swatchGenerator = new SwatchGenerator(controller);
        }
        try {
            BufferedImage image = swatchGenerator.generateSwatch(address, amount, label);
            ImageIcon icon;
            if (image != null) {
                icon = new ImageIcon(image);
            } else {
                icon = new ImageIcon();
            }
            if (qrCodeLabel != null) {
                qrCodeLabel.setIcon(icon);
            }
        } catch (RuntimeException re) {
            // swatch generation failed
            log.error(re.getMessage(), re);
        }
    }

    public boolean processDroppedImage(Image image) {
        if (image == null) {
            return false;
        }

        BufferedImage bufferedImage;
        log.debug("importData - 2.1");
        if (image.getWidth(qrCodeLabel) + MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING > qrCodePanel.getWidth()
                || image.getHeight(qrCodeLabel) + MINIMUM_QRCODE_PANEL_VERTICAL_SPACING > qrCodePanel.getHeight()) {
            // scale image
            double qrCodeWidth = (double) qrCodePanel.getWidth();
            double qrCodeHeight = (double) qrCodePanel.getHeight();
            double xScale = qrCodeWidth / (double) (image.getWidth(qrCodeLabel) + MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING);
            double yScale = qrCodeHeight / (double) (image.getHeight(qrCodeLabel) + MINIMUM_QRCODE_PANEL_VERTICAL_SPACING);
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
        log.debug("importData - 3 - decodedResult = {}", decodedString);
        log.info("importData = decodedString = {}", decodedString);
        return processDecodedString(decodedString, icon);
    }

    public boolean processDecodedString(String decodedString, ImageIcon icon) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
            return false;
        } else {
            // decode the string to an AddressBookData
            // TODO Consider handling the possible runtime exception at a
            // suitable level for recovery

            // Early MultiBit versions did not URL encode the label hence may
            // have illegal embedded spaces - convert to ENCODED_SPACE_CHARACTER
            // i.e be lenient
            String uriString = decodedString.toString().replace(" ", MultiBitController.ENCODED_SPACE_CHARACTER);
            BitcoinURI bitcoinURI = new BitcoinURI(controller.getMultiBitService().getNetworkParameters(), uriString);

            log.debug("SendBitcoinPanel - ping 1");
            Address address = bitcoinURI.getAddress();
            log.debug("SendBitcoinPanel - ping 2");
            String addressString = address.toString();
            log.debug("SendBitcoinPanel - ping 3");
            String amountString = amountTextField.getText();
            if (bitcoinURI.getAmount() != null) {
                amountString = controller.getLocaliser().bitcoinValueToString(bitcoinURI.getAmount(), false, false);
            }
            log.debug("SendBitcoinPanel - ping 4");
            String decodedLabel = null;
            try {
                decodedLabel = java.net.URLDecoder.decode(bitcoinURI.getLabel(), "UTF-8");
            } catch (UnsupportedEncodingException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            log.debug("SendBitcoinPanel#processDecodedString addressString = " + addressString + ", amountString = " + amountString
                    + ", label = " + decodedLabel);
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
                controller.getModel().getActivePerWalletModelData().setDirty(true);

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
            labelTextArea.setText(decodedLabel);
            log.debug("SendBitcoinPanel - ping 13");
            controller.updateStatusLabel("");

            if (icon != null) {
                qrCodeLabel.setIcon(icon);
            } else {
                displaySwatch(addressString, amountString, decodedLabel);
            }
            return true;

            // mainFrame.updateStatusLabel(controller.getLocaliser().getString("sendBitcoinPanel.couldNotUnderstandQRcode",
            // new Object[] { decodedString }));
            // return false;
        }
    }

    /**
     * select the rows that correspond to the current data
     */
    public void selectRows() {
        // stop listener firing
        addressesTable.getSelectionModel().removeListSelectionListener(addressesListener);

        String address = controller.getModel().getActiveWalletPreference(getAddressConstant());
        displaySwatch(address, amountTextField.getText(), labelTextArea.getText());

        // see if the current address is on the table and select it
        int rowToSelect = addressesTableModel.findRowByAddress(address, isReceiveBitcoin());
        if (rowToSelect >= 0) {
            addressesTable.getSelectionModel().setSelectionInterval(rowToSelect, rowToSelect);
            selectedAddressRow = rowToSelect;
        }

        // scroll to visible
        addressesTable.scrollRectToVisible(addressesTable.getCellRect(rowToSelect, 0, false));
        addressesTable.invalidate();
        addressesTable.validate();
        addressesTable.repaint();

        // put the listeners back
        addressesTable.getSelectionModel().addListSelectionListener(addressesListener);
    }

    public Data getData() {
        Data data = new Data();

        Item isReceiveBitcoinItem = new Item(MultiBitModel.IS_RECEIVE_BITCOIN);
        isReceiveBitcoinItem.setNewValue(Boolean.toString(isReceiveBitcoin()));
        data.addItem(MultiBitModel.IS_RECEIVE_BITCOIN, isReceiveBitcoinItem);

        Item addressItem = new Item(getAddressConstant());
        if (addressTextArea != null) {
            addressItem.setNewValue(addressTextArea.getText());
        }
        if (addressTextField != null) {
            addressItem.setNewValue(addressTextField.getText());
        }
        data.addItem(getAddressConstant(), addressItem);

        Item labelItem = new Item(getLabelConstant());
        labelItem.setNewValue(labelTextArea.getText());
        data.addItem(getLabelConstant(), labelItem);

        Item amountItem = new Item(getAmountConstant());
        amountItem.setNewValue(amountTextField.getText());
        data.addItem(getAmountConstant(), amountItem);

        Item uriImageItem = new Item(getUriImageConstant());
        uriImageItem.setNewValue(qrCodeLabel);
        data.addItem(getUriImageConstant(), uriImageItem);

        return data;
    }

    public JTextArea getLabelTextArea() {
        return labelTextArea;
    }

    public JPanel getFormPanel() {
        return formPanel;
    }

    public JPanel getQRCodePanel() {
        return qrCodePanel;
    }

    public JLabel getQRCodeLabel() {
        return qrCodeLabel;
    }

    private BufferedImage toBufferedImage(Image image, int width, int height) {
        log.debug("toBufferedImage - 1");
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
        log.debug("toBufferedImage - 2.2, image = {} ,width = {}, height = {}", new Object[] { image, width, height });

        BufferedImage bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        log.debug("toBufferedImage - 2.3, bufferedImage = {}", bufferedImage);

        Graphics2D g2 = bufferedImage.createGraphics();

        log.debug("toBufferedImage - 3");
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2.drawImage(image, 0, 0, width, height, null);
        log.debug("toBufferedImage - 4");
        g2.dispose();
        return bufferedImage;
    }
}
