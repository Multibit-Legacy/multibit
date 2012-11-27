/**
 * Copyright 2012 multibit.org
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
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.SystemColor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import org.joda.money.Money;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterListener;
import org.multibit.exchange.CurrencyInfo;
import org.multibit.exchange.ExchangeRate;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.qrcode.QRCodeEncoderDecoder;
import org.multibit.qrcode.QRCodeGenerator;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.dataproviders.BitcoinFormDataProvider;
import org.multibit.viewsystem.dataproviders.CopyQRCodeImageDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyQRCodeImageAction;
import org.multibit.viewsystem.swing.action.MnemonicUtil;
import org.multibit.viewsystem.swing.action.PasteSwatchAction;
import org.multibit.viewsystem.swing.action.ZoomAction;
import org.multibit.viewsystem.swing.view.components.DashedBorder;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextArea;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.WalletMajorVersion;
import com.google.bitcoin.uri.BitcoinURI;

/**
 * Abstract parent class for SendBitcoinPanel and ReceiveBitcoinPanel
 * 
 * @author jim
 * 
 */
public abstract class AbstractTradePanel extends JPanel implements View, CopyQRCodeImageDataProvider, BitcoinFormDataProvider, CurrencyConverterListener {

    public boolean isShowSidePanel() {
        return showSidePanel;
    }

    public void setShowSidePanel(boolean showSidePanel) {
        AbstractTradePanel.showSidePanel = showSidePanel;
    }

    private static final long serialVersionUID = 7227169670412230264L;

    private static final Logger log = LoggerFactory.getLogger(AbstractTradePanel.class);

    private static final int MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING = 30;
    private static final int MINIMUM_QRCODE_PANEL_VERTICAL_SPACING = 80;

    private static final int TABLE_BORDER = 3;

    protected static final int PREFERRED_NUMBER_OF_LABEL_ROWS = 3;

    protected MultiBitFrame mainFrame;

    protected MultiBitController controller;

    protected MultiBitTextArea labelTextArea;

    protected MultiBitTextField amountBTCTextField;
    protected MultiBitTextField amountFiatTextField;
    protected MultiBitLabel amountEqualsLabel;
    protected MultiBitLabel amountUnitFiatLabel;
    
    protected Money parsedAmountBTC = null;
    protected Money parsedAmountFiat = null;

    protected JPanel upperPanel;

    protected AddressBookTableModel addressesTableModel;

    protected JTable addressesTable;

    protected MultiBitTextField addressTextField;

    protected int selectedAddressRowModel;

    protected SelectionListener addressesListener;

    protected MultiBitButton createNewButton;
    protected MultiBitButton deleteButton;

    protected Action  createNewAddressAction;
    protected Action  deleteAddressAction;

    protected JLabel titleLabel;

    protected MultiBitLabel qrCodeLabel;
    protected JScrollPane qrCodeScrollPane;

    public final static int QR_CODE_LEFT_COLUMN = 11;

    protected static final int QRCODE_WIDTH = 140;
    protected static final int QRCODE_HEIGHT = 140;

    protected static final int TEXTFIELD_VERTICAL_DELTA = 16;
    protected static final int HELP_BUTTON_INDENT = 6;
    protected static final int AMOUNT_BTC_INDENT = 4;

    private final int STENT_DELTA = 4;

    protected FontMetrics fontMetrics;
    protected int separatorSize;
    protected int smallSeparatorSize;

    protected MultiBitLabel displayUsingLabel;
    protected MultiBitButton copyQRCodeImageButton;
    protected MultiBitButton pasteSwatchButton;
    protected MultiBitButton zoomButton;
    protected JPanel qrCodeButtonPanelStent1;
    protected JPanel qrCodeButtonPanelStent2;
    protected JPanel qrCodeButtonPanelStent3;
    protected JPanel qrCodeButtonPanelStent4;

    protected JPanel forcer2;

    protected MultiBitButton sidePanelButton;
    protected static boolean showSidePanel = false;

    private final AbstractTradePanel thisAbstractTradePanel;

    private QRCodeGenerator qrCodeGenerator;

    /**
     * map that maps one of the key constants in this class to the actual key to
     * use for localisation
     * 
     * this map is filled up in the constructors of the concrete impls
     */
    protected Map<String, String> localisationKeyConstantToKeyMap;

    protected String ADDRESSES_TITLE = "addressesTitle";
    protected String CREATE_NEW_TOOLTIP = "createNewTooltip";
    protected String DELETE_TOOLTIP = "deleteTooltip";

    public AbstractTradePanel(MultiBitFrame mainFrame, MultiBitController controller) {
        this.mainFrame = mainFrame;
        this.controller = controller;
        this.thisAbstractTradePanel = this;

        setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(2 * ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));

        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();

        fontMetrics = this.getFontMetrics(font);

        separatorSize = (int) (fontMetrics.getHeight() * 0.5);
        smallSeparatorSize = (int) (fontMetrics.getHeight() * 0.2);

        localisationKeyConstantToKeyMap = new HashMap<String, String>();
        populateLocalisationMap();

        initUI();

        labelTextArea.requestFocusInWindow();

        displaySidePanel();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        CurrencyConverter.INSTANCE.addCurrencyConverterListener(this);
    }

    /**
     * is it the receive bitcoin panel (return true) or the send bitcoin panel
     * (return false)
     */
    protected abstract boolean isReceiveBitcoin();

    public abstract String getAddressConstant();

    public abstract String getLabelConstant();

    public abstract String getAmountConstant();

    protected abstract Action getCreateNewAddressAction();

    protected abstract Action getDeleteAddressAction();

    /**
     * method for concrete impls to populate the localisation map
     */
    protected abstract void populateLocalisationMap();
    
    public abstract void checkDeleteSendingEnabled();   

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
        setLayout(new GridBagLayout());
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        String showSidePanelText = controller.getModel().getUserPreference(MultiBitModel.SHOW_SIDE_PANEL);
        if (!Boolean.FALSE.toString().equals(showSidePanelText)) {
            showSidePanel = true;
        }

        GridBagConstraints constraints = new GridBagConstraints();
        GridBagConstraints constraints2 = new GridBagConstraints();

        upperPanel = new JPanel(new GridBagLayout());
        upperPanel.setOpaque(false);
        createFormPanel(upperPanel, constraints2);
        createQRCodePanel(upperPanel, constraints2);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 0.5;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(upperPanel, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 2.0;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createAddressesPanel(), constraints);
    }

    protected void createFormPanelStentsAndForcers(JPanel panel, GridBagConstraints constraints) {
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        panel.add(MultiBitTitledPanel.createStent(calculateStentWidth(), separatorSize), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS, separatorSize),
                constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(
                MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS, fontMetrics.getHeight()
                        * PREFERRED_NUMBER_OF_LABEL_ROWS), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(
                MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS, separatorSize),
                constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(MultiBitTitledPanel.createStent(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), separatorSize),
                constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS, separatorSize),
                constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(MultiBitTitledPanel.createStent(separatorSize), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 7;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        panel.add(MultiBitTitledPanel.createStent(separatorSize), constraints);

        constraints.fill = GridBagConstraints.VERTICAL;
        constraints.gridx = 10;
        constraints.gridy = 1;
        constraints.weightx = 0.02;
        constraints.weighty = 10000;
        constraints.gridwidth = 1;
        constraints.gridheight = 8;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel dashedStent = MultiBitTitledPanel.createStent(smallSeparatorSize);
        dashedStent.setBorder(new DashedBorder(controller.getLocaliser().getLocale()));
        panel.add(dashedStent, constraints);

        JPanel forcer1 = new JPanel();
        forcer1.setOpaque(false);
        // forcer1.setBorder(BorderFactory.createLineBorder(Color.CYAN));

        forcer1.setMaximumSize(new Dimension(QRCODE_WIDTH, 1));
        forcer1.setPreferredSize(new Dimension(QRCODE_WIDTH, 1));
        forcer1.setMinimumSize(new Dimension((int) (QRCODE_WIDTH * 0.1), 1));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 9;
        constraints.gridy = 7;
        constraints.weightx = 10000;
        constraints.weighty = 4.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        panel.add(forcer1, constraints);

        forcer2 = new JPanel();
        forcer2.setOpaque(false);
        forcer2.setMaximumSize(new Dimension((int) (QRCODE_WIDTH * 1.2), 1));
        forcer2.setPreferredSize(new Dimension(QRCODE_WIDTH, 1));
        forcer2.setMinimumSize(new Dimension((int) (QRCODE_WIDTH * 1.0), 1));
       
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = QR_CODE_LEFT_COLUMN;
        constraints.gridy = 7;
        constraints.weightx = 1;
        constraints.weighty = 1.0;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        panel.add(forcer2, constraints);
    }

    public void displaySidePanel() {
        setQRCodePanelVisible(showSidePanel);

        if (sidePanelButton == null) {
            return;
        }

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        if (showSidePanel) {
            // show less
            if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
                sidePanelButton.setIcon(ImageLoader.createImageIcon(ImageLoader.SIDE_PANEL_HIDE_ICON_FILE));
            } else {
                sidePanelButton.setIcon(ImageLoader.createImageIcon(ImageLoader.SIDE_PANEL_HIDE_RTL_ICON_FILE));
            }
            sidePanelButton.setText("");
            sidePanelButton.setBorderPainted(false);
            sidePanelButton.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.showLess.tooltip"));
            sidePanelButton.setMnemonic(mnemonicUtil.getMnemonic(controller.getLocaliser().getString(
                    "sendBitcoinPanel.showLess.mnemonic")));
            sidePanelButton.setVerticalTextPosition(JLabel.BOTTOM);
            sidePanelButton.setHorizontalTextPosition(JLabel.LEFT);
        } else {
            // show more
            if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
                sidePanelButton.setIcon(ImageLoader.createImageIcon(ImageLoader.SIDE_PANEL_SHOW_ICON_FILE));
            } else {
                sidePanelButton.setIcon(ImageLoader.createImageIcon(ImageLoader.SIDE_PANEL_SHOW_RTL_ICON_FILE));
            }
            sidePanelButton.setText("");
            sidePanelButton.setBorderPainted(false);
            sidePanelButton.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.showMore.tooltip"));
            sidePanelButton.setMnemonic(mnemonicUtil.getMnemonic(controller.getLocaliser().getString(
                    "sendBitcoinPanel.showMore.mnemonic")));
            sidePanelButton.setVerticalTextPosition(JLabel.BOTTOM);
            sidePanelButton.setHorizontalTextPosition(JLabel.LEFT);
        }
    }

    protected abstract JPanel createFormPanel(JPanel panel, GridBagConstraints constraints);

    protected abstract void loadForm();

    protected JPanel createAddressesHeaderPanel() {
        JPanel addressesHeaderPanel = new JPanel();
        addressesHeaderPanel.setOpaque(false);
        addressesHeaderPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        addressesHeaderPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, SystemColor.windowBorder));
        addressesHeaderPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(MultiBitTitledPanel.createStent(HELP_BUTTON_INDENT), constraints);

        createNewAddressAction = getCreateNewAddressAction();
        createNewButton = new MultiBitButton(createNewAddressAction, controller);
        createNewButton.setText(controller.getLocaliser().getString("crudButton.new"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(createNewButton, constraints);

        int offset = 0;
        deleteAddressAction = getDeleteAddressAction();
        if (isReceiveBitcoin()) {
            // Put in a stent
            MultiBitButton dummyDeleteButton = new MultiBitButton(deleteAddressAction, controller);
            JPanel deleteButtonStent = MultiBitTitledPanel.createStent(dummyDeleteButton.getPreferredSize().width, dummyDeleteButton.getPreferredSize().height);
            offset = 1;
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 2;
            constraints.gridy = 0;
            constraints.gridwidth = 1;
            constraints.weightx = 0.1;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            addressesHeaderPanel.add(deleteButtonStent, constraints);
        } else {
            deleteButton = new MultiBitButton(deleteAddressAction, controller);
            offset = 1;
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 2;
            constraints.gridy = 0;
            constraints.gridwidth = 1;
            constraints.weightx = 0.1;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.LINE_START;
            addressesHeaderPanel.add(deleteButton, constraints);

            
        }

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2 + offset;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        addressesHeaderPanel.add(MultiBitTitledPanel.createStent(HELP_BUTTON_INDENT * 2), constraints);

        titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        titleLabel.setText(getLocalisationString(ADDRESSES_TITLE, null));
        titleLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3 + offset;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(titleLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 4;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1000;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        addressesHeaderPanel.add(filler2, constraints);

        return addressesHeaderPanel;
    }

    protected JPanel createAddressesPanel() {
        JPanel addressPanel = new JPanel();
        addressPanel.setOpaque(true);
        addressPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        addressPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        addressesTableModel = new AddressBookTableModel(controller, isReceiveBitcoin());
        addressesTable = new JTable(addressesTableModel);
        addressesTable.setOpaque(true);
        addressesTable.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        addressesTable.setShowGrid(false);
        addressesTable.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        addressesTable.setRowSelectionAllowed(true);
        addressesTable.setColumnSelectionAllowed(false);
        addressesTable.setRowHeight(getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight());

        // row sorter
        TableRowSorter<TableModel> rowSorter = new TableRowSorter<TableModel>(addressesTable.getModel());
        addressesTable.setRowSorter(rowSorter);

        // sort by date descending
        List<TableRowSorter.SortKey> sortKeys = new ArrayList<TableRowSorter.SortKey>();
        sortKeys.add(new TableRowSorter.SortKey(0, SortOrder.ASCENDING));
        rowSorter.setSortKeys(sortKeys);
        Comparator<String> comparator = new Comparator<String>() {
            
            Collator collator;
            
            public int compare(String o1, String o2) {
                if (collator == null) {
                    collator = Collator.getInstance(controller.getLocaliser().getLocale());
                }
                if (o1 == null) {
                    return 1;
                } else if (o2 == null) {
                    return -1;
                } else if ("".equals(o1) && !"".equals(o2)) {
                    return 1;
                } else if ("".equals(o2) && !"".equals(o1)) {
                    return -1;
                } else {
                    return collator.compare(o1, o2);
                }
            }
        };
        rowSorter.setComparator(0, comparator);

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
        scrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        scrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);

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

    protected void setQRCodePanelVisible(boolean visible) {
        if (displayUsingLabel != null) {
            displayUsingLabel.setVisible(visible);
        }

        if (qrCodeScrollPane != null) {
            qrCodeScrollPane.setVisible(visible);
        }

        if (copyQRCodeImageButton != null) {
            copyQRCodeImageButton.setVisible(visible);
        }

        if (pasteSwatchButton != null) {
            pasteSwatchButton.setVisible(visible);
        }

        if (zoomButton != null) {
            zoomButton.setVisible(visible);
        }

        if (qrCodeButtonPanelStent1 != null) {
            qrCodeButtonPanelStent1.setVisible(visible);
        }

        if (qrCodeButtonPanelStent2 != null) {
            qrCodeButtonPanelStent2.setVisible(visible);
        }

        if (qrCodeButtonPanelStent3 != null) {
            qrCodeButtonPanelStent3.setVisible(visible);
        }
        
        if (qrCodeButtonPanelStent4 != null) {
            qrCodeButtonPanelStent4.setVisible(visible);
        }

        if (forcer2 != null) {
            forcer2.setVisible(visible);
        }

        if (qrCodeLabel != null) {
            qrCodeLabel.invalidate();
            qrCodeLabel.validate();
        }
        if (upperPanel != null) {
            upperPanel.invalidate();
            upperPanel.validate();
        }
        thisAbstractTradePanel.repaint();
    }

    class SelectionListener implements ListSelectionListener {
        boolean enabled;

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        SelectionListener() {
            enabled = true;
        }

        public void valueChanged(ListSelectionEvent event) {
            if (enabled) {
                int viewRow = addressesTable.getSelectedRow();
                if (viewRow >= 0) {
                    selectedAddressRowModel = addressesTable.convertRowIndexToModel(viewRow);
                    AddressBookData rowData = addressesTableModel.getAddressBookDataByRow(selectedAddressRowModel,
                            thisAbstractTradePanel.isReceiveBitcoin());
                    if (rowData != null) {
                        controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAddressConstant(),
                                rowData.getAddress());
                        controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getLabelConstant(),
                                rowData.getLabel());
                        if (addressTextField != null) {
                            addressTextField.setText(rowData.getAddress());
                        }
                        labelTextArea.setText(rowData.getLabel());

                        String amountForQRCode = "";
                        if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                            amountForQRCode = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
                        }
                        displayQRCode(rowData.getAddress(), amountForQRCode, labelTextArea.getText());
                    }
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
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
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
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
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
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
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
                Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                        : ColorAndFontConstants.BACKGROUND_COLOR);
                label.setBackground(backgroundColor);
                label.setForeground(table.getForeground());
            }
            return label;
        }
    }

    protected void createQRCodePanel(JPanel panel, GridBagConstraints constraints) {
        qrCodeLabel = new MultiBitLabel("", JLabel.CENTER);
        qrCodeLabel.setVerticalTextPosition(JLabel.BOTTOM);
        qrCodeLabel.setHorizontalTextPosition(JLabel.CENTER);
        qrCodeLabel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        qrCodeLabel.setOpaque(true);
        qrCodeLabel.setMinimumSize(new Dimension(QRCODE_WIDTH, QRCODE_HEIGHT));

        setDragLabelTextAndTooltip();

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

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = QR_CODE_LEFT_COLUMN;
        constraints.gridy = 3;
        constraints.weightx = 1;
        constraints.weighty = 0.5;
        constraints.gridwidth = 5;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.BASELINE;

        qrCodeScrollPane = new JScrollPane(qrCodeLabel);
        qrCodeScrollPane.setOpaque(true);
        qrCodeScrollPane.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        qrCodeScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        qrCodeScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        qrCodeScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        qrCodeScrollPane.setBorder(BorderFactory.createEmptyBorder());
        qrCodeScrollPane.setViewportBorder(BorderFactory.createEmptyBorder());
        qrCodeScrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        qrCodeScrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);

        panel.add(qrCodeScrollPane, constraints);

        constraints.fill = GridBagConstraints.VERTICAL;
        constraints.gridx = QR_CODE_LEFT_COLUMN + 6;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 5;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.BASELINE;
        
        JPanel verticalStent = MultiBitTitledPanel.createStent(1, QRCODE_HEIGHT);
        //verticalStent.setBorder(BorderFactory.createLineBorder(Color.MAGENTA));
        panel.add(verticalStent, constraints);

        createQRCodeButtonPanel(panel, constraints);
    }

    private void setDragLabelTextAndTooltip() {
        if (!isReceiveBitcoin()) {
            Icon icon = qrCodeLabel.getIcon();
            if (icon == null || icon.getIconHeight() == -1) {
                qrCodeLabel.setText(createCenteredMultilineLabelText(controller.getLocaliser().getString("sendBitcoinPanel.dragBitcoinLabelQRcode.text")));
            } else {
                qrCodeLabel.setText("");                
            }
            qrCodeLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.dragBitcoinLabelQRcode.tooltip"));
        }
    }

    protected void updateQRCodePanel() {
        String address = null;

        if (addressTextField != null) {
            address = addressTextField.getText();
        }
        String amount = null;
        if (amountBTCTextField != null) {
            parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
            
            if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                amount = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
            }
        }
        String label = "";
        if (labelTextArea != null) {
            label = labelTextArea.getText();
        }
        displayQRCode(address, amount, label);
        qrCodeLabel.invalidate();
        upperPanel.invalidate();
        qrCodeLabel.validate();
        upperPanel.validate();
        thisAbstractTradePanel.repaint();
    }

    protected void createQRCodeButtonPanel(JPanel panel, GridBagConstraints constraints) {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridBagLayout());
        buttonPanel.setOpaque(false);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = QR_CODE_LEFT_COLUMN;
        constraints.gridy = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_LEADING;
        panel.add(buttonPanel, constraints);

        GridBagConstraints constraints2 = new GridBagConstraints();

        CopyQRCodeImageAction copyQRCodeImageAction = new CopyQRCodeImageAction(controller, this,
                ImageLoader.createImageIcon(ImageLoader.COPY_ICON_FILE));
        copyQRCodeImageButton = new MultiBitButton(copyQRCodeImageAction, controller);

        PasteSwatchAction pasteSwatchAction = new PasteSwatchAction(controller, this,
                ImageLoader.createImageIcon(ImageLoader.PASTE_ICON_FILE));
        pasteSwatchButton = new MultiBitButton(pasteSwatchAction, controller);

        qrCodeButtonPanelStent1 = MultiBitTitledPanel.createStent(smallSeparatorSize);
        qrCodeButtonPanelStent2 = MultiBitTitledPanel.createStent(smallSeparatorSize);
        qrCodeButtonPanelStent3 = MultiBitTitledPanel.createStent(smallSeparatorSize);

        ZoomAction zoomAction = new ZoomAction(controller, ImageLoader.createImageIcon(ImageLoader.ZOOM_ICON_FILE), mainFrame, this);
        zoomButton = new MultiBitButton(zoomAction, controller);
        zoomButton.setText("");

        qrCodeButtonPanelStent4 = MultiBitTitledPanel.createStent((int)zoomButton.getPreferredSize().getWidth(), (int)zoomButton.getPreferredSize().getHeight());

        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        buttonPanel.add(qrCodeButtonPanelStent1);

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        buttonPanel.add(copyQRCodeImageButton, constraints2);

        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 3;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        buttonPanel.add(qrCodeButtonPanelStent2);

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 4;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        if (isReceiveBitcoin()) {
            buttonPanel.add(zoomButton, constraints2);
            
            constraints2.fill = GridBagConstraints.NONE;
            constraints2.gridx = 5;
            constraints2.gridy = 0;
            constraints2.weightx = 0.1;
            constraints2.weighty = 1.0;
            constraints2.gridwidth = 1;
            constraints2.gridheight = 1;
            constraints2.anchor = GridBagConstraints.CENTER;
            buttonPanel.add(qrCodeButtonPanelStent3, constraints2);

            constraints2.fill = GridBagConstraints.NONE;
            constraints2.gridx = 6;
            constraints2.gridy = 0;
            constraints2.weightx = 0.1;
            constraints2.weighty = 1.0;
            constraints2.gridwidth = 1;
            constraints2.gridheight = 1;
            constraints2.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
            buttonPanel.add(qrCodeButtonPanelStent4, constraints2);
        } else {
            buttonPanel.add(pasteSwatchButton, constraints2);

            constraints2.fill = GridBagConstraints.NONE;
            constraints2.gridx = 5;
            constraints2.gridy = 0;
            constraints2.weightx = 0.1;
            constraints2.weighty = 1.0;
            constraints2.gridwidth = 1;
            constraints2.gridheight = 1;
            constraints2.anchor = GridBagConstraints.CENTER;
            buttonPanel.add(qrCodeButtonPanelStent3, constraints2);

            constraints2.fill = GridBagConstraints.NONE;
            constraints2.gridx = 6;
            constraints2.gridy = 0;
            constraints2.weightx = 0.1;
            constraints2.weighty = 1.0;
            constraints2.gridwidth = 1;
            constraints2.gridheight = 1;
            constraints2.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
            buttonPanel.add(zoomButton, constraints2);
        }

        JPanel forcerQR = new JPanel();
        forcerQR.setOpaque(false);
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 7;
        constraints2.gridy = 0;
        constraints2.weightx = 50.0;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        buttonPanel.add(forcerQR, constraints2);
    }

    protected JPanel createAmountPanel() {
        JPanel amountPanel = new JPanel();
        amountPanel.setOpaque(false);
        amountPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        amountPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints2 = new GridBagConstraints();
        int longFieldWidth = fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT);

        amountBTCTextField = new MultiBitTextField("", 10, controller);
        amountBTCTextField.setHorizontalAlignment(JTextField.TRAILING);
        amountBTCTextField.setMinimumSize(new Dimension((int) (longFieldWidth * 0.40), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountBTCTextField.setPreferredSize(new Dimension((int) (longFieldWidth * 0.40), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountBTCTextField.setMaximumSize(new Dimension((int) (longFieldWidth * 0.40), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountBTCTextField.addKeyListener(new AmountBTCKeyListener());
        
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 1.0;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        amountPanel.add(amountBTCTextField, constraints2);

        MultiBitLabel amountUnitBTCLabel = new MultiBitLabel(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel"));
        amountUnitBTCLabel.setHorizontalTextPosition(SwingConstants.LEADING);
        amountUnitBTCLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel.tooltip"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        amountPanel.add(amountUnitBTCLabel, constraints2);

        amountEqualsLabel = new MultiBitLabel("   =   "); // 3 spaces either side
        amountEqualsLabel.setHorizontalTextPosition(SwingConstants.CENTER);
        amountEqualsLabel.setFocusable(false);
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.03;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        amountPanel.add(amountEqualsLabel, constraints2);
        amountPanel.add(MultiBitTitledPanel.createStent(amountEqualsLabel.getPreferredSize().width, amountEqualsLabel.getPreferredSize().height), constraints2);

        amountFiatTextField = new MultiBitTextField("", 10, controller);
        amountFiatTextField.setHorizontalAlignment(JTextField.TRAILING);
        amountFiatTextField.setMinimumSize(new Dimension((int) (longFieldWidth * 0.3), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountFiatTextField.setPreferredSize(new Dimension((int) (longFieldWidth * 0.3), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        amountFiatTextField.setMaximumSize(new Dimension((int) (longFieldWidth * 0.3), getFontMetrics(
                FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
                + TEXTFIELD_VERTICAL_DELTA));
        //amountFiatTextField.addKeyListener(new QRCodeKeyListener());
        amountFiatTextField.addKeyListener(new AmountFiatKeyListener());

        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 4;
        constraints2.gridy = 0;
        constraints2.weightx = 1.0;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        amountPanel.add(amountFiatTextField, constraints2);
        amountPanel.add(MultiBitTitledPanel.createStent(amountFiatTextField.getPreferredSize().width, amountFiatTextField.getPreferredSize().height), constraints2);

        CurrencyInfo currencyInfo = CurrencyConverter.INSTANCE.getCurrencyCodeToInfoMap().get(CurrencyConverter.INSTANCE.getCurrencyUnit().getCurrencyCode());
        amountUnitFiatLabel = new MultiBitLabel("");
        int fiatCurrencySymbolPosition = 3;   // Prefix is default.
        if (currencyInfo != null) {
            amountUnitFiatLabel.setText(currencyInfo.getCurrencySymbol());
            if (!currencyInfo.isPrefix()) {
                fiatCurrencySymbolPosition = 5;
            }
        }
        amountUnitFiatLabel.setHorizontalTextPosition(SwingConstants.LEADING);
        amountUnitFiatLabel.setToolTipText(controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel.tooltip"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = fiatCurrencySymbolPosition;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        amountPanel.add(amountUnitFiatLabel, constraints2);  
        amountPanel.add(MultiBitTitledPanel.createStent(amountUnitFiatLabel.getPreferredSize().width, amountUnitFiatLabel.getPreferredSize().height), constraints2);
        return amountPanel;
    }
    
    @Override
    public void displayView() {
        loadForm();
        getAddressesTableModel().fireTableDataChanged();
        selectRows();

        // disable any new changes if another process has changed the wallet
        if (controller.getModel().getActivePerWalletModelData() != null
                && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
            // files have been changed by another process - disallow edits
            mainFrame.setUpdatesStoppedTooltip(labelTextArea);
            labelTextArea.setEditable(false);
            labelTextArea.setEnabled(false);
            mainFrame.setUpdatesStoppedTooltip(amountBTCTextField);
            amountBTCTextField.setEditable(false);
            amountBTCTextField.setEnabled(false);
            if (amountFiatTextField != null) {
                amountFiatTextField.setEditable(false);
                amountFiatTextField.setEnabled(false);
            }
            
            if (createNewButton != null) {
                createNewButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(createNewButton);
            }
            if (deleteButton != null) {
                deleteButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(deleteButton);
            }
            if (pasteSwatchButton != null) {
                pasteSwatchButton.setEnabled(false);
                mainFrame.setUpdatesStoppedTooltip(pasteSwatchButton);
            }
        } else {
            labelTextArea.setToolTipText(null);
            labelTextArea.setEditable(true);
            labelTextArea.setEnabled(true);
            amountBTCTextField.setToolTipText(null);
            amountBTCTextField.setEditable(true);
            amountBTCTextField.setEnabled(true);
            if (amountFiatTextField != null) {
                amountFiatTextField.setToolTipText(null);
                amountFiatTextField.setEditable(true);
                amountFiatTextField.setEnabled(true);
            }
            if (createNewButton != null) {
                createNewButton.setEnabled(true);
                createNewButton.setToolTipText(getLocalisationString(CREATE_NEW_TOOLTIP, null));
            }
            if (deleteButton != null) {
                deleteButton.setEnabled(true);
                deleteButton.setToolTipText(getLocalisationString(DELETE_TOOLTIP, null));
            }
            if (pasteSwatchButton != null) {
                pasteSwatchButton.setEnabled(true);
                pasteSwatchButton.setToolTipText(controller.getLocaliser().getString("pasteSwatchAction.tooltip"));
            }
        }
        
        if (CurrencyConverter.INSTANCE.isShowingFiat()) {
            if (amountFiatTextField != null) {
                amountFiatTextField.setVisible(true);
                amountEqualsLabel.setVisible(true);
                amountUnitFiatLabel.setVisible(true);
            }
            if (amountBTCTextField != null) {
                parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
            }
            updateFiatAmount();
        } else {
            if (amountFiatTextField != null) {
                amountFiatTextField.setVisible(false);
                amountEqualsLabel.setVisible(false);
                amountUnitFiatLabel.setVisible(false);
            }
        }
        updateQRCodePanel();
        displaySidePanel();
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
            if (addressTextField != null) {
                address = addressTextField.getText();
            }
            String amount = "";
            if (amountBTCTextField != null) {
                parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
                
                if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                    amount = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
                }
            }
            String label = labelTextArea.getText();
            AddressBookData addressBookData = new AddressBookData(label, address);

            WalletInfo walletInfo = controller.getModel().getActiveWalletWalletInfo();
            if (walletInfo == null) {
                walletInfo = new WalletInfo(controller.getModel().getActiveWalletFilename(), WalletMajorVersion.PROTOBUF_ENCRYPTED);
                controller.getModel().setActiveWalletInfo(walletInfo);
            }
            address = WhitespaceTrimmer.trim(address);
            addressesTableModel.setAddressBookDataByRow(addressBookData, selectedAddressRowModel, isReceiveBitcoin());

            selectRowInTableFromModelRow(selectedAddressRowModel);

            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAddressConstant(), address);
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getLabelConstant(), label);
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAmountConstant(), amount);
            controller.getModel().getActivePerWalletModelData().setDirty(true);

            displayQRCode(address, amount, label);
        }
    }
    

    protected class AmountBTCKeyListener implements KeyListener {
        /** Handle the key typed event in the amount BTC field */
        public void keyTyped(KeyEvent e) {
        }

        /** Handle the key-pressed event in the amount BTC field */
        public void keyPressed(KeyEvent e) {
            // do nothing
        }

        /** Handle the key-released event in the amount BTC field */
        public void keyReleased(KeyEvent e) {
            String address = null;
            if (addressTextField != null) {
                address = addressTextField.getText();
                address = WhitespaceTrimmer.trim(address);
            }
            String amount = "";
            if (amountBTCTextField != null) {
                parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
                
                if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                    amount = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
                }
            }

            String label = labelTextArea.getText();
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAmountConstant(), amount);
            controller.getModel().getActivePerWalletModelData().setDirty(true);

            updateFiatAmount();
            displayQRCode(address, amount, label);
        }
    }
    

    protected class AmountFiatKeyListener implements KeyListener {
        /** Handle the key typed event in the amount Fiat field */
        public void keyTyped(KeyEvent e) {
        }

        /** Handle the key-pressed event in the amount Fiat field */
        public void keyPressed(KeyEvent e) {
            // do nothing
        }

        /** Handle the key-released event in the amount Fiat field */
        public void keyReleased(KeyEvent e) {
            String address = null;
            if (addressTextField != null) {
                address = addressTextField.getText();
                address = WhitespaceTrimmer.trim(address);
            }
            String label = labelTextArea.getText();
            String amountFiat = amountFiatTextField.getText();
            String amountBTCAsString = updateBTCAmount(amountFiat);

            displayQRCode(address, amountBTCAsString, label);
        }
    }
    
    protected void updateFiatAmount() {
        // Convert the BTC into fiat and populate the fiat amount label.
        if (CurrencyConverter.INSTANCE.getRate() != null && CurrencyConverter.INSTANCE.isShowingFiat()) {
            try {
                if (parsedAmountBTC != null) {
                    parsedAmountFiat = CurrencyConverter.INSTANCE.convertToFiat(parsedAmountBTC.getAmount().toBigInteger());
                    String fiatText = CurrencyConverter.INSTANCE.getFiatAsLocalisedString(parsedAmountFiat, false, false);
                    if (amountFiatTextField != null) {
                        amountFiatTextField.setText(fiatText);
                    }
                }
            } catch (NumberFormatException nfe) {
                log.debug(nfe.getClass().getName() + " " + nfe.getMessage());
            }
        }
    }

    protected String updateBTCAmount(String amountFiat) {
        parsedAmountBTC = CurrencyConverter.INSTANCE.convertToBTC(amountFiat);
        if (parsedAmountBTC != null) {
            String amountBTCAsString = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false,
                    false);
            controller.getModel().setActiveWalletPreference(thisAbstractTradePanel.getAmountConstant(), amountBTCAsString);
            controller.getModel().getActivePerWalletModelData().setDirty(true);

            amountBTCTextField.setText(CurrencyConverter.INSTANCE.getBTCAsLocalisedString(parsedAmountBTC));
            return amountBTCAsString;
        } else {
            amountBTCTextField.setText("");
            return null;
        }
    }

    /**
     * Display the address, amount and label as a QR code.
     */
    public void displayQRCode(String address, String amount, String label) {
        if (qrCodeGenerator == null) {
            qrCodeGenerator = new QRCodeGenerator(controller);
        }
        try {
            BufferedImage image = qrCodeGenerator.generateQRcode(address, amount, label);
            ImageIcon icon;
            if (image != null) {
                icon = new ImageIcon(image);
            } else {
                icon = new ImageIcon();
            }
            if (qrCodeLabel != null) {
                qrCodeLabel.setIcon(icon);
                setDragLabelTextAndTooltip();
            }
        } catch (RuntimeException re) {
            // QR code generation failed
            log.error(re.getMessage(), re);
        }
    }

    public boolean processDroppedImage(Image image) {
        if (image == null) {
            return false;
        }

        BufferedImage bufferedImage;
        log.debug("importData - 2.1");
        if (image.getWidth(qrCodeLabel) + MINIMUM_QRCODE_PANEL_HORIZONTAL_SPACING > qrCodeLabel.getWidth()
                || image.getHeight(qrCodeLabel) + MINIMUM_QRCODE_PANEL_VERTICAL_SPACING > qrCodeLabel.getHeight()) {
            // scale image
            double qrCodeWidth = (double) qrCodeLabel.getWidth();
            double qrCodeHeight = (double) qrCodeLabel.getHeight();
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
            BitcoinURI bitcoinURI = new BitcoinURI(controller.getModel().getNetworkParameters(), uriString);

            log.debug("SendBitcoinPanel - ping 1");
            Address address = bitcoinURI.getAddress();
            log.debug("SendBitcoinPanel - ping 2");
            String addressString = address.toString();
            log.debug("SendBitcoinPanel - ping 3");
            String amountString = "";
            String amountStringLocalised = "";
            if (amountBTCTextField != null) {
                parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
                
                if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                    amountString = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
                    amountStringLocalised = CurrencyConverter.INSTANCE.getBTCAsLocalisedString(parsedAmountBTC);
                }
            }
            if (bitcoinURI.getAmount() != null) {
                amountString = controller.getLocaliser().bitcoinValueToStringNotLocalised(bitcoinURI.getAmount(), false, false);
            }
            log.debug("SendBitcoinPanel - ping 4");
            String decodedLabel = "";
            try {
                if (bitcoinURI.getLabel() != null) {
                    decodedLabel = java.net.URLDecoder.decode(bitcoinURI.getLabel(), "UTF-8");
                }
            } catch (UnsupportedEncodingException e) {
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
            int rowToSelectModel = addressesTableModel.findRowByAddress(addressBookData.getAddress(), false);
            if (rowToSelectModel >= 0) {
                addressesTableModel.setAddressBookDataByRow(addressBookData, rowToSelectModel, false);
                selectedAddressRowModel = rowToSelectModel;

                selectRowInTableFromModelRow(rowToSelectModel);
            } else {
                // add a new row to the table
                controller.getModel().getActiveWalletWalletInfo().addSendingAddress(addressBookData);
                controller.getModel().getActivePerWalletModelData().setDirty(true);

                addressesTableModel.fireTableDataChanged();

                // select new row
                rowToSelectModel = addressesTableModel.findRowByAddress(addressBookData.getAddress(), false);
                if (rowToSelectModel >= 0) {
                    selectedAddressRowModel = rowToSelectModel;

                    selectRowInTableFromModelRow(rowToSelectModel);
                }
            }
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
            amountBTCTextField.setText(amountStringLocalised);
            log.debug("SendBitcoinPanel - ping 12");
            labelTextArea.setText(decodedLabel);
            log.debug("SendBitcoinPanel - ping 13");
            Message message = new Message("");
            MessageManager.INSTANCE.addMessage(message);

            if (icon != null) {
                qrCodeLabel.setIcon(icon);
                setDragLabelTextAndTooltip();
            } else {
                displayQRCode(addressString, amountString, decodedLabel);
            }
            
            checkDeleteSendingEnabled();

            return true;
        }
    }

    /**
     * select the rows that correspond to the current data
     */
    public void selectRows() {
        // stop listener firing
        addressesListener.setEnabled(false);

        String address = controller.getModel().getActiveWalletPreference(getAddressConstant());
        String amount = "";
        if (amountBTCTextField != null) {
            parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
            
            if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                amount = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
            }
        }
        displayQRCode(address, amount, labelTextArea.getText());

        // see if the current address is on the table and select it
        int rowToSelectModel = addressesTableModel.findRowByAddress(address, isReceiveBitcoin());
        if (rowToSelectModel >= 0) {
            selectedAddressRowModel = rowToSelectModel;

            selectRowInTableFromModelRow(rowToSelectModel);
        }

        addressesTable.invalidate();
        addressesTable.validate();
        addressesTable.repaint();

        // enable listener
        addressesListener.setEnabled(true);
    }

    private void selectRowInTableFromModelRow(int rowToSelectModel) {
        if (rowToSelectModel < addressesTableModel.getRowCount()) {
            try {
                int rowToSelect = addressesTable.convertRowIndexToView(rowToSelectModel);
                addressesListener.setEnabled(false);
                addressesTable.getSelectionModel().setSelectionInterval(rowToSelect, rowToSelect);
                addressesListener.setEnabled(true);

                // scroll to visible
                addressesTable.scrollRectToVisible(addressesTable.getCellRect(rowToSelect, 0, false));
            } catch (ArrayIndexOutOfBoundsException e) {
                // absorb - row wrong
            }
        }
    }

    public JTextArea getLabelTextArea() {
        return labelTextArea;
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

    public AddressBookTableModel getAddressesTableModel() {
        return addressesTableModel;
    }

    public JTable getAddressesTable() {
        return addressesTable;
    }
    
    private String createCenteredMultilineLabelText(String labelText) {
        String centeredText = "<html><center>";
        String[] lines = labelText.split("\\|");
        if (lines != null) {
            for (int i = 0; i < lines.length ; i++) {
                if ( i > 0) {
                    centeredText += "<br>";
                }
                centeredText += lines[i];
            }
        }
        centeredText += "</center></html>";
        
        return centeredText;
    }

    // CopyQRCodeImageDataProvider methods
    @Override
    public JLabel getURIImage() {
        return qrCodeLabel;
    }
    
    @Override
    public String getAddress() {
        if (addressTextField != null) {
            return addressTextField.getText();
        } else {
            return null;
        }
    }
    
    @Override
    public String getLabel() {
        if (labelTextArea != null) {
            return labelTextArea.getText();
        } else {
            return null;
        }
    }
    
    @Override
    public String getAmount() {
        String amount = null;
        if (amountBTCTextField != null) {
            parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
            
            if (parsedAmountBTC != null && parsedAmountBTC.getAmount() != null) {
                amount = controller.getLocaliser().bitcoinValueToStringNotLocalised(parsedAmountBTC.getAmount().toBigInteger(), false, false);
            }
        }
       return amount;
    }

    @Override
    public String getAmountFiat() {
       if (amountFiatTextField != null) {
           return amountFiatTextField.getText();
       } else {
           return null;
       }
    }

    public MultiBitTextField getAddressTextField() {
        return addressTextField;
    }

    public Action getCreateNewSendingAddressAction() {
        return createNewAddressAction;
    }

    public Action getDeleteSendingAddressAction() {
        return deleteAddressAction;
    }
    
    @Override
    public void lostExchangeRate(ExchangeRate exchangeRate) {
        // TODO Auto-generated method stub
    }

    @Override
    public void foundExchangeRate(ExchangeRate exchangeRate) {
        updatedExchangeRate(exchangeRate);
    }

    @Override
    public void updatedExchangeRate(ExchangeRate exchangeRate) {
        if (amountBTCTextField != null) {
            parsedAmountBTC = CurrencyConverter.INSTANCE.parseToBTC(amountBTCTextField.getText());
        }
        updateFiatAmount();
    }   
}
