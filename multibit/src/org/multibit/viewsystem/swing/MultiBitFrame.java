package org.multibit.viewsystem.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import org.apache.log4j.Logger;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.multibit.viewsystem.swing.action.HelpAboutAction;
import org.multibit.viewsystem.swing.action.OpenAddressBookAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.action.ReceiveBitcoinAction;
import org.multibit.viewsystem.swing.action.SaveWalletAsAction;
import org.multibit.viewsystem.swing.action.SendBitcoinAction;
import org.multibit.viewsystem.swing.action.ShowHelpContentsAction;
import org.multibit.viewsystem.swing.action.ShowPreferencesAction;
import org.multibit.viewsystem.swing.view.AddressBookView;
import org.multibit.viewsystem.swing.view.CreateOrEditAddressView;
import org.multibit.viewsystem.swing.view.HelpAboutView;
import org.multibit.viewsystem.swing.view.HelpContentsView;
import org.multibit.viewsystem.swing.view.OpenWalletView;
import org.multibit.viewsystem.swing.view.ReceiveBitcoinView;
import org.multibit.viewsystem.swing.view.SaveWalletAsView;
import org.multibit.viewsystem.swing.view.SendBitcoinConfirmView;
import org.multibit.viewsystem.swing.view.SendBitcoinView;
import org.multibit.viewsystem.swing.view.ShowPreferencesView;
import org.multibit.viewsystem.swing.view.ToDoView;
import org.multibit.viewsystem.swing.view.ValidationErrorView;
import org.multibit.viewsystem.swing.watermark.FillPainter;
import org.multibit.viewsystem.swing.watermark.WatermarkPainter;
import org.multibit.viewsystem.swing.watermark.WatermarkViewport;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.Wallet;

/*
 * JFrame displaying Swing version of MultiBit
 */
public class MultiBitFrame extends JFrame implements ViewSystem {
    private static final int A_SMALL_NUMBER_OF_PIXELS = 100;
    private static final int A_LARGE_NUMBER_OF_PIXELS = 1000000;
    private static final int STATUSBAR_HEIGHT = 30;
    private static final int TOOLBAR_HEIGHT = 120;
    private static final String SAVE_AS_ICON_FILE = "/images/saveAs.jpg";
    private static final String OPEN_WALLET_ICON_FILE = "/images/openWallet.png";
    private static final String SEND_BITCOIN_ICON_FILE = "/images/send.jpg";
    private static final String RECEIVE_BITCOIN_ICON_FILE = "/images/receive.jpg";
    private static final String VIEW_ADDRESSBOOK_ICON_FILE = "/images/addressBook.jpg";
    private static final String PROGRESS_0_ICON_FILE = "/images/progress0.jpg";
    private static final String PROGRESS_1_ICON_FILE = "/images/progress1.jpg";
    private static final String PROGRESS_2_ICON_FILE = "/images/progress2.jpg";
    private static final String PROGRESS_3_ICON_FILE = "/images/progress3.jpg";
    private static final String PROGRESS_4_ICON_FILE = "/images/progress4.jpg";
    private static final String PROGRESS_5_ICON_FILE = "/images/progress5.jpg";
    private static final String TICK_ICON_FILE = "/images/tick.jpg";
    private static final String PREFERENCES_ICON_FILE = "/images/preferences.jpg";
    private static final String HELP_CONTENTS_ICON_FILE = "/images/helpContents.jpg";
    private static final String MULTIBIT_SMALL_ICON_FILE = "/images/multibit-small.jpg";
    private static final String MULTIBIT_ICON_FILE = "/images/multibit.gif";
    private static final String TITLE_SEPARATOR = " - ";
    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.7D;

    public static final String SPACER = "   "; // 3 spaces

    private static final long serialVersionUID = 7621813615342923041L;

    private MultiBitController controller;
    private MultiBitModel model;
    private Localiser localiser;

    private JTextField balanceTextField;

    private JLabel onlineStatusLabel, networkStatusLabel;
    private boolean isOnline;

    private MultiBitFrame thisFrame;
    private JTable table;
    private WalletTableModel walletTableModel;
    
    public Logger logger = Logger.getLogger(MultiBitFrame.class.getName());


    /**
     * the view that the controller is telling us to display an int - one of the
     * View constants
     * 
     */
    private int currentView;

    private Map<Integer, View> viewMap;

    private Timer refreshTimer;
    
    public MultiBitFrame(MultiBitController controller) {
        this.controller = controller;
        this.model = controller.getModel();
        this.localiser = controller.getLocaliser();
        this.thisFrame = this;

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        final MultiBitController finalController = controller;
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent arg0) {
                org.multibit.action.ExitAction exitAction = new org.multibit.action.ExitAction(finalController);
                exitAction.execute(null);
            }
        });

        sizeAndCenter();

        initUI();

        recreateAllViews(false);

        // initialise status bar settings
        nowOffline();
        networkStatusLabel.setText("");

        balanceTextField.setText(Localiser.bitcoinValueToFriendlyString(model.getBalance(), true, false));

        pack();
        setVisible(true);
        
        refreshTimer = new Timer();
        refreshTimer.schedule(new RefreshTimerTask(this), 0, 60000); // fires once a minute
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
        Container contentPane = getContentPane();
        contentPane.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        // set the application icon
        ImageIcon imageIcon = createImageIcon(MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }

        JToolBar toolBar = addMenuBarAndCreateToolBar(constraints, contentPane);
        toolBar.setMaximumSize(new Dimension(A_LARGE_NUMBER_OF_PIXELS, TOOLBAR_HEIGHT));
        toolBar.setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, TOOLBAR_HEIGHT));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.85;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        contentPane.add(toolBar, constraints);

        JPanel balancePanel = createBalancePanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.15;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        contentPane.add(balancePanel, constraints);

        JComponent walletPanel = createWalletPanel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(walletPanel, constraints);

        StatusBar statusBar = new StatusBar();
        statusBar.setMaximumSize(new Dimension(A_LARGE_NUMBER_OF_PIXELS, STATUSBAR_HEIGHT));
        statusBar.setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, STATUSBAR_HEIGHT));
        onlineStatusLabel = new JLabel();
        onlineStatusLabel.setHorizontalAlignment(SwingConstants.CENTER);
        networkStatusLabel = new JLabel();
        statusBar.setZones(new String[] { "online", "network" }, new JComponent[] { onlineStatusLabel, networkStatusLabel },
                new String[] { "12%", "*" });
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.01;
        contentPane.add(statusBar, constraints);
    }

    private JPanel createBalancePanel() {
        JPanel balancePanel = new JPanel();
        balancePanel.setMinimumSize(new Dimension(180, 30));
        balancePanel.setPreferredSize(new Dimension(180, 30));
        balancePanel.setOpaque(true);
        Border border = BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.BLACK),
                BorderFactory.createEmptyBorder(0, 3, 0, 3));
        balancePanel.setBorder(border);
        balancePanel.setBackground(Color.WHITE);

        balancePanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel balanceLabel = new JLabel(localiser.getString("multiBitFrame.balanceLabel"));

        balanceLabel.setFont(balanceLabel.getFont().deriveFont(java.awt.Font.BOLD));
        balanceLabel.setToolTipText(localiser.getString("multiBitFrame.balanceLabel.tooltip"));
        balanceLabel.setHorizontalAlignment(JLabel.RIGHT);

        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.2;
        constraints.anchor = GridBagConstraints.LINE_END;

        balancePanel.add(balanceLabel, constraints);

        balanceTextField = new JTextField();
        balanceTextField.setEditable(false);
        balanceTextField.setHorizontalAlignment(JTextField.RIGHT);
        balanceTextField.setBorder(BorderFactory.createEmptyBorder());
        balanceTextField.setBackground(Color.WHITE);
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.8;
        constraints.anchor = GridBagConstraints.LINE_END;
        balancePanel.add(balanceTextField, constraints);

        return balancePanel;
    }

    private JPanel createWalletPanel() {
        JPanel walletPanel = new JPanel();
        walletPanel.setOpaque(false);

        walletPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        walletTableModel = new WalletTableModel(model, controller);
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

        walletPanel.add(scrollPane, constraints);

        return walletPanel;
    }

    private JToolBar addMenuBarAndCreateToolBar(GridBagConstraints constraints, Container contentPane) {
        // Create the menu bar
        JMenuBar menuBar = new JMenuBar();

        // Create the toolBar
        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);

        // Build the File menu.
        JMenu fileMenu = new JMenu(localiser.getString("multiBitFrame.fileMenuText"));
        fileMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.fileMenuMnemonic"));
        menuBar.add(fileMenu);

        // Build the Trade menu.
        JMenu tradeMenu = new JMenu(localiser.getString("multiBitFrame.tradeMenuText"));
        tradeMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.tradeMenuMnemonic"));
        menuBar.add(tradeMenu);

        // Build the View menu.
        JMenu viewMenu = new JMenu(localiser.getString("multiBitFrame.viewMenuText"));
        viewMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.viewMenuMnemonic"));
        menuBar.add(viewMenu);

        // Build the Help menu.
        JMenu helpMenu = new JMenu(localiser.getString("multiBitFrame.helpMenuText"));
        helpMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.helpMenuMnemonic"));
        menuBar.add(helpMenu);

        // open wallet action
        JPanel openWalletPanel = new JPanel(new BorderLayout());
        openWalletPanel.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));

        OpenWalletAction openWalletAction = new OpenWalletAction(controller, createImageIcon(OPEN_WALLET_ICON_FILE));
        JMenuItem menuItem = new JMenuItem(openWalletAction);
        fileMenu.add(menuItem);
        JButton openWalletButton = new JButton(openWalletAction);
        openWalletButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        openWalletButton.setHorizontalTextPosition(AbstractButton.CENTER);
        openWalletPanel.add(openWalletButton);
        toolBar.add(openWalletPanel);

        // save wallet as action
        SaveWalletAsAction saveWalletAsAction = new SaveWalletAsAction(controller, createImageIcon(SAVE_AS_ICON_FILE), this);
        menuItem = new JMenuItem(saveWalletAsAction);
        fileMenu.add(menuItem);

        fileMenu.addSeparator();

        // exit action
        menuItem = new JMenuItem(new ExitAction(controller));
        fileMenu.add(menuItem);

        // show help contents action
        ShowHelpContentsAction showHelpContentsAction = new ShowHelpContentsAction(controller, localiser,
                createImageIcon(HELP_CONTENTS_ICON_FILE));
        menuItem = new JMenuItem(showHelpContentsAction);
        helpMenu.add(menuItem);

        // help about action
        HelpAboutAction helpAboutAction = new HelpAboutAction(controller, createImageIcon(MULTIBIT_SMALL_ICON_FILE), this);
        menuItem = new JMenuItem(helpAboutAction);
        helpMenu.add(menuItem);

        // receive bitcoin action
        JPanel receiveBitcoinPanel = new JPanel(new BorderLayout());
        receiveBitcoinPanel.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        ReceiveBitcoinAction receiveBitcoinAction = new ReceiveBitcoinAction(controller, localiser,
                createImageIcon(RECEIVE_BITCOIN_ICON_FILE), this);
        tradeMenu.add(receiveBitcoinAction);
        JButton receiveBitcoinButton = new JButton(receiveBitcoinAction);
        receiveBitcoinButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        receiveBitcoinButton.setHorizontalTextPosition(AbstractButton.CENTER);
        receiveBitcoinPanel.add(receiveBitcoinButton);
        toolBar.add(receiveBitcoinPanel);

        // send bitcoin action
        SendBitcoinAction sendBitcoinAction = new SendBitcoinAction(controller, createImageIcon(SEND_BITCOIN_ICON_FILE), this);
        menuItem = new JMenuItem(sendBitcoinAction);
        tradeMenu.add(menuItem);

        JPanel sendBitcoinPanel = new JPanel(new BorderLayout());
        sendBitcoinPanel.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        JButton sendBitcoinButton = new JButton(sendBitcoinAction);
        sendBitcoinButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        sendBitcoinButton.setHorizontalTextPosition(AbstractButton.CENTER);
        sendBitcoinPanel.add(sendBitcoinButton);
        toolBar.add(sendBitcoinPanel);

        // open address book
        JPanel openAddressBookReceivingPanel = new JPanel(new BorderLayout());
        openAddressBookReceivingPanel.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        OpenAddressBookAction openAddressBookReceivingAction = new OpenAddressBookAction(controller,
                createImageIcon(VIEW_ADDRESSBOOK_ICON_FILE), true, true);
        viewMenu.add(openAddressBookReceivingAction);
        JButton openAddressBookReceivingButton = new JButton(openAddressBookReceivingAction);
        openAddressBookReceivingButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        openAddressBookReceivingButton.setHorizontalTextPosition(AbstractButton.CENTER);
        openAddressBookReceivingPanel.add(openAddressBookReceivingButton);
        toolBar.add(openAddressBookReceivingPanel);

        // show preferences
        ShowPreferencesAction showPreferencesAction = new ShowPreferencesAction(controller,
                createImageIcon(PREFERENCES_ICON_FILE));
        viewMenu.add(showPreferencesAction);

        setJMenuBar(menuBar);

        return toolBar;
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
                label.setToolTipText(localiser.getString("multiBitFrame.status.notConfirmed"));
                break;
            }
            case 1: {
                label.setIcon(progress1Icon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 2: {
                label.setIcon(progress2Icon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 3: {
                label.setIcon(progress3Icon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 4: {
                label.setIcon(progress4Icon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 5: {
                label.setIcon(progress5Icon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.beingConfirmed"));
                break;
            }
            case 6: {
                label.setIcon(tickIcon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.isConfirmed"));
                break;
            }
            default:
                label.setIcon(progress0Icon);
                label.setToolTipText(localiser.getString("multiBitFrame.status.notConfirmed"));
            }
            return label;
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
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm");

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

    // MultiBitView methods
    /**
     * recreate all views
     */
    public void recreateAllViews() {
        recreateAllViews(true);
    }

    /**
     * recreate all views
     */
    public void recreateAllViews(boolean initUI) {
        // close down current view
        if (currentView != 0) {
            navigateAwayFromView(currentView, View.HOME_PAGE_VIEW, ViewSystem.newViewIsParentOfPrevious); // home
                                                                                                          // page
            // choice
            // here is
            // arbitary
        }

        if (initUI) {
            this.localiser = controller.getLocaliser();
            Container contentPane = getContentPane();
            contentPane.removeAll();
            initUI();
        }
        updateOnlineStatusText();
        balanceTextField.setText(Localiser.bitcoinValueToFriendlyString(model.getBalance(), true, false));

        String walletFilename = model.getWalletFilename();
        if (walletFilename == null) {
            setTitle(localiser.getString("multiBitFrame.title"));
        } else {
            setTitle(walletFilename + TITLE_SEPARATOR + localiser.getString("multiBitFrame.title"));
        }

        invalidate();
        validate();
        repaint();

        // create the views
        viewMap = new HashMap<Integer, View>();

        // home page view is placeholder - never used - the frame is the view
        viewMap.put(View.HOME_PAGE_VIEW, new ToDoView(controller, localiser, this));
        viewMap.put(View.HELP_ABOUT_VIEW, new HelpAboutView(controller, localiser, this));
        viewMap.put(View.HELP_CONTENTS_VIEW, new HelpContentsView(controller, localiser, this));
        viewMap.put(View.OPEN_WALLET_VIEW, new OpenWalletView(controller, localiser, this));
        viewMap.put(View.SAVE_WALLET_AS_VIEW, new SaveWalletAsView(controller, localiser, this));
        viewMap.put(View.RECEIVE_BITCOIN_VIEW, new ReceiveBitcoinView(controller, localiser, this));
        viewMap.put(View.SEND_BITCOIN_VIEW, new SendBitcoinView(controller, localiser, this));
        viewMap.put(View.SEND_BITCOIN_CONFIRM_VIEW, new SendBitcoinConfirmView(controller, localiser, this));
        viewMap.put(View.CREATE_NEW_RECEIVING_ADDRESS_VIEW,
                new CreateOrEditAddressView(controller, localiser, this, true, true));
        viewMap.put(View.CREATE_NEW_SENDING_ADDRESS_VIEW, new CreateOrEditAddressView(controller, localiser, this, true, false));
        viewMap.put(View.EDIT_RECEIVING_ADDRESS_VIEW, new CreateOrEditAddressView(controller, localiser, this, false, true));
        viewMap.put(View.EDIT_SENDING_ADDRESS_VIEW, new CreateOrEditAddressView(controller, localiser, this, false, false));
        viewMap.put(View.ADDRESS_BOOK_RECEIVING_VIEW, new AddressBookView(controller, localiser, this, true));
        viewMap.put(View.ADDRESS_BOOK_SENDING_VIEW, new AddressBookView(controller, localiser, this, false));
        viewMap.put(View.PREFERENCES_VIEW, new ShowPreferencesView(controller, this));
        viewMap.put(View.VALIDATION_ERROR_VIEW, new ValidationErrorView(controller, this));
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        if (currentView != 0) {
            View view = viewMap.get(currentView);
            if (view != null) {
                view.displayMessage(messageKey, messageData, titleKey);
            } else {
                System.out.println("MultiBitFrame#displayMessage - no view with id " + currentView
                        + " to display message with key " + messageKey);
            }
        } else {
            System.out.println("MultiBitFrame#displayMessage - no view on which to display message with key " + messageKey);
        }
    }

    /**
     * convert the view constant to the corresponding view object in our
     * viewsystem
     * 
     * @param viewToDisplay
     *            int
     * @return view object
     */
    private View lookupView(int viewToDisplay) {
        // by default return home page
        View viewToReturn = viewMap.get(View.HOME_PAGE_VIEW);

        switch (viewToDisplay) {
        case View.RECEIVE_BITCOIN_VIEW: {
            viewToReturn = viewMap.get(View.RECEIVE_BITCOIN_VIEW);
            break;
        }

        case View.SEND_BITCOIN_VIEW: {
            viewToReturn = viewMap.get(View.SEND_BITCOIN_VIEW);
            break;
        }

        case View.SEND_BITCOIN_CONFIRM_VIEW: {
            viewToReturn = viewMap.get(View.SEND_BITCOIN_CONFIRM_VIEW);
            break;
        }

        case View.ADDRESS_BOOK_RECEIVING_VIEW: {
            viewToReturn = viewMap.get(View.ADDRESS_BOOK_RECEIVING_VIEW);
            break;
        }

        case View.ADDRESS_BOOK_SENDING_VIEW: {
            viewToReturn = viewMap.get(View.ADDRESS_BOOK_SENDING_VIEW);
            break;
        }

        case View.CREATE_NEW_RECEIVING_ADDRESS_VIEW: {
            viewToReturn = viewMap.get(View.CREATE_NEW_RECEIVING_ADDRESS_VIEW);
            break;
        }

        case View.CREATE_NEW_SENDING_ADDRESS_VIEW: {
            viewToReturn = viewMap.get(View.CREATE_NEW_SENDING_ADDRESS_VIEW);
            break;
        }

        case View.EDIT_RECEIVING_ADDRESS_VIEW: {
            viewToReturn = viewMap.get(View.EDIT_RECEIVING_ADDRESS_VIEW);
            break;
        }

        case View.EDIT_SENDING_ADDRESS_VIEW: {
            viewToReturn = viewMap.get(View.EDIT_SENDING_ADDRESS_VIEW);
            break;
        }

        case View.HELP_ABOUT_VIEW: {
            viewToReturn = viewMap.get(View.HELP_ABOUT_VIEW);
            break;
        }

        case View.HELP_CONTENTS_VIEW: {
            viewToReturn = viewMap.get(View.HELP_CONTENTS_VIEW);
            break;
        }

        case View.OPEN_WALLET_VIEW: {
            viewToReturn = viewMap.get(View.OPEN_WALLET_VIEW);
            break;
        }

        case View.SAVE_WALLET_AS_VIEW: {
            viewToReturn = viewMap.get(View.SAVE_WALLET_AS_VIEW);
            break;
        }

        case View.PREFERENCES_VIEW: {
            viewToReturn = viewMap.get(View.PREFERENCES_VIEW);
            break;
        }

        case View.VALIDATION_ERROR_VIEW: {
            viewToReturn = viewMap.get(View.VALIDATION_ERROR_VIEW);
            break;
        }

        case View.HOME_PAGE_VIEW:
        default: {
            viewToReturn = viewMap.get(View.HOME_PAGE_VIEW);
        }
        }

        return viewToReturn;
    }

    /**
     * display next view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    public void displayView(int viewToDisplay) {
        currentView = viewToDisplay;

        if (viewToDisplay != View.HOME_PAGE_VIEW) {
            final View nextViewFinal = lookupView(viewToDisplay);

            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    nextViewFinal.displayView();
                }
            });
        }
    }

    /**
     * navigate away from view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextView, int relationshipOfNewViewToPrevious) {
        if (viewToNavigateAwayFrom != View.HOME_PAGE_VIEW) {
            final int nextViewFinal = nextView;
            final int relationshipOfNewViewToPreviousFinal = relationshipOfNewViewToPrevious;

            final View viewToNavigateAwayFromFinal = lookupView(viewToNavigateAwayFrom);

            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    viewToNavigateAwayFromFinal.navigateAwayFromView(nextViewFinal, relationshipOfNewViewToPreviousFinal);
                }
            });
        }
    }

    public void nowOnline() {
        isOnline = true;
        updateOnlineStatusText();
    }

    public void nowOffline() {
        isOnline = false;
        updateOnlineStatusText();
    }

    public void updateOnlineStatusText() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                String onlineStatus = isOnline ? localiser.getString("multiBitFrame.onlineText") : localiser
                        .getString("multiBitFrame.offlineText");
                if (isOnline) {
                    onlineStatusLabel.setForeground(new Color(0, 100, 0));
                } else {
                    onlineStatusLabel.setForeground(new Color(180, 0, 0));
                }
                onlineStatusLabel.setText(onlineStatus);
            }
        });
    }

    public void updateDownloadStatus(String updateDownloadStatus) {
        final String finalUpdateDownloadStatus = updateDownloadStatus;
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                networkStatusLabel.setText(finalUpdateDownloadStatus);
            }
        });
    }

    public void blockDownloaded() {
       logger.debug("blockDownloaded");;
       SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // update screen in case status bars have changed
                thisFrame.fireDataChanged();
                table.invalidate();
                table.validate();
                table.repaint();
                thisFrame.invalidate();
                thisFrame.validate();
                thisFrame.repaint();
            }
        });
    }

    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        // print out transaction details
        try {
            TransactionInput input = transaction.getInputs().get(0);
            Address from = input.getFromAddress();
            BigInteger value = transaction.getValueSentToMe(wallet);
            logger.debug("Received " + Localiser.bitcoinValueToFriendlyString(value, true, false) + " from "
                    + from.toString());
            wallet.saveToFile(new File(controller.getModel().getWalletFilename()));
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just crash.
            e.printStackTrace();
            throw new RuntimeException(e);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }

        logger.debug("MultiBitFrame#onCoinsReceived - wallet is currently:\n" + wallet.toString());
        fireDataChanged();
    }

    /**
     * update the UI after the model data has changed
     */
    public void fireDataChanged() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                balanceTextField.setText(Localiser
                        .bitcoinValueToFriendlyString(controller.getModel().getBalance(), true, false));

                // update wallet table model
                walletTableModel.recreateWalletData();
                table.invalidate();
                table.validate();
                table.repaint();
                thisFrame.invalidate();
                thisFrame.validate();
                thisFrame.repaint();
            }
        });
    }
}