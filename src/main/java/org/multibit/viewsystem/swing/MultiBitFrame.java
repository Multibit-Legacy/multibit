package org.multibit.viewsystem.swing;

import com.google.bitcoin.core.*;
import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.platform.GenericApplication;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.*;
import org.multibit.viewsystem.swing.view.*;
import org.multibit.viewsystem.swing.view.yourwallets.YourWalletsPanel;
import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.math.BigInteger;
import java.util.Timer;

/*
 * JFrame displaying Swing version of MultiBit
 *
 * TODO Consider breaking this into smaller classes
 */
public class MultiBitFrame extends JFrame implements ViewSystem, ApplicationListener {

    private static final Logger log = LoggerFactory.getLogger(MultiBitFrame.class);

    private static final int A_SMALL_NUMBER_OF_PIXELS = 100;
    private static final int A_LARGE_NUMBER_OF_PIXELS = 1000000;
    private static final int STATUSBAR_HEIGHT = 30;
    private static final int TOOLBAR_HEIGHT = 120;
    public static final String COPY_ICON_FILE = "/images/copy.png";
    public static final String PASTE_ICON_FILE = "/images/paste.png";

    private static final String YOUR_WALLETS_ICON_FILE = "/images/yourWallets.png";
    public static final String SINGLE_WALLET_ICON_FILE = "/images/singleWallet.png";
    private static final String CREATE_NEW_ICON_FILE = "/images/createNew.png";
    public static final String OPEN_WALLET_ICON_FILE = "/images/openWallet.png";
    private static final String SEND_BITCOIN_ICON_FILE = "/images/send.jpg";
    private static final String RECEIVE_BITCOIN_ICON_FILE = "/images/receive.jpg";
    private static final String PREFERENCES_ICON_FILE = "/images/preferences.png";
    private static final String HELP_CONTENTS_ICON_FILE = "/images/helpContents.png";
    private static final String MULTIBIT_SMALL_ICON_FILE = "/images/multibit-small.png";
    public static final String MULTIBIT_ICON_FILE = "/images/multibit.png";
    public static final String MULTIBIT_128_ICON_FILE = "/images/multibit128.png";
    public static final String QUESTION_MARK_ICON_FILE = "/images/questionMark.png";
    private static final String WALLET_ICON_FILE = "/images/wallet.png";
    private static final String RTL_WALLET_ICON_FILE = "/images/rtl_wallet.png";
    public static final String EXCLAMATION_MARK_ICON_FILE = "/images/exclamationMark.png";

    public static final String MULTIBIT_FONT_NAME = "Dialog";
    public static final int MULTIBIT_FONT_STYLE = Font.PLAIN;
    public static final int MULTIBIT_LARGE_FONT_SIZE = 15;

    public static final Color GOLD_COLOR = new Color(212, 160, 23);

    public static final Color BACKGROUND_COLOR = new Color(244, 244, 246);
    public static final Color VERY_LIGHT_BACKGROUND_COLOR = new Color(254, 254, 255);
    public static final Color DARK_BACKGROUND_COLOR = new Color(188, 212, 230); // beau
    // blue

    private static JTable COLOR_TABLE = new JTable();
    public static Color SELECTION_FOREGROUND_COLOR = COLOR_TABLE.getSelectionForeground();
    public static Color SELECTION_BACKGROUND_COLOR = COLOR_TABLE.getSelectionBackground();
    private Border normalBorder;
    private Border underlineBorder;

    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.72D;

    public static final int WIDTH_OF_LONG_FIELDS = 320;
    public static final int WIDTH_OF_AMOUNT_FIELD = 160;

    private static final long serialVersionUID = 7621813615342923041L;

    private MultiBitController controller;
    private MultiBitModel model;
    private Localiser localiser;

    private BlinkLabel estimatedBalanceTextLabel;
    private JLabel availableBalanceTextLabel;

    private JPanel activeWalletPanel;
    private JComboBox activeWalletComboBox;

    private JPanel yourWalletsPanel;
    private JPanel receiveBitcoinPanel;
    private JPanel sendBitcoinPanel;

    private JLabel onlineLabel;
    private JLabel statusLabel;
    private boolean isOnline;

    private MultiBitFrame thisFrame;

    private MultiBitButton yourWalletsButton;
    private MultiBitButton sendBitcoinButton;
    private MultiBitButton receiveBitcoinButton;

    private static final int TOOLTIP_DISMISSAL_DELAY = 10000; // millisecs

    /**
     * Provide the Application reference during construction
     */
    private final GenericApplication application;

    /**
     * the panel containing the main view
     */
    private JPanel viewPanel;

    public Logger logger = LoggerFactory.getLogger(MultiBitFrame.class.getName());

    /**
     * the view that the controller is telling us to display an int - one of the
     * View constants
     */
    private int currentView;

    private ViewFactory viewFactory;

    private Timer fileChangeTimer;

    private JPanel headerPanel;

    @SuppressWarnings("deprecation")
    public MultiBitFrame(MultiBitController controller, GenericApplication application) {
        this.controller = controller;
        this.model = controller.getModel();
        this.localiser = controller.getLocaliser();
        this.thisFrame = this;
        this.application = application;

        setCursor(Cursor.WAIT_CURSOR);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        setTitle(localiser.getString("multiBitFrame.title"));

        ToolTipManager.sharedInstance().setDismissDelay(TOOLTIP_DISMISSAL_DELAY);

        final MultiBitController finalController = controller;

        // TODO Examine how this fits in with the controller onQuit() event
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent arg0) {
                org.multibit.action.ExitAction exitAction = new org.multibit.action.ExitAction(finalController);
                exitAction.execute(null);
            }
        });

        getContentPane().setBackground(MultiBitFrame.BACKGROUND_COLOR);
        sizeAndCenter();

        normalBorder = BorderFactory.createEmptyBorder(0, 4, 7, 4);
        underlineBorder = BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 4, 3, 4), BorderFactory
            .createCompoundBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, SELECTION_BACKGROUND_COLOR),
                BorderFactory.createEmptyBorder(0, 0, 3, 0)));

        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        recreateAllViews(false);

        // initialise status bar settings
        nowOffline();
        updateStatusLabel("");

        estimatedBalanceTextLabel
            .setText(controller.getLocaliser().bitcoinValueToString4(model.getActiveWalletEstimatedBalance(), true, false));

        availableBalanceTextLabel.setText(controller.getLocaliser().getString("multiBitFrame.availableToSpend",
            new Object[]{controller.getLocaliser().bitcoinValueToString4(model.getActiveWalletAvailableBalance(), true, false)}));

        estimatedBalanceTextLabel.setFocusable(true);
        estimatedBalanceTextLabel.requestFocusInWindow();

        pack();
        setVisible(true);

        fileChangeTimer = new Timer();
        // fires once every minute
        fileChangeTimer.schedule(new FileChangeTimerTask(controller, this), 0, 60000);
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

        headerPanel = new HeaderPanel();
        headerPanel.setLayout(new GridBagLayout());
        headerPanel.setOpaque(true);

        JPanel balancePanel = createHeaderPanel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(balancePanel, constraints);

        JPanel toolBarPanel = addMenuBarAndCreateToolBar(constraints, contentPane);
        toolBarPanel.setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, TOOLBAR_HEIGHT));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(toolBarPanel, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.15;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        contentPane.add(headerPanel, constraints);

        viewPanel = new JPanel(new BorderLayout()); // initally blank
        viewPanel.setOpaque(true);
        viewPanel.setBackground(BACKGROUND_COLOR);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(viewPanel, constraints);

        StatusBar statusBar = new StatusBar(controller);
        statusBar.setMaximumSize(new Dimension(A_LARGE_NUMBER_OF_PIXELS, STATUSBAR_HEIGHT));
        statusBar.setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, STATUSBAR_HEIGHT));
        onlineLabel = new JLabel();
        onlineLabel.setHorizontalAlignment(SwingConstants.CENTER);
        onlineLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent arg0) {
                int blockHeight = -1;
                if (controller.getMultiBitService() != null) {
                    if (controller.getMultiBitService().getChain() != null) {
                        if (controller.getMultiBitService().getChain().getChainHead() != null) {
                            blockHeight = controller.getMultiBitService().getChain().getChainHead().getHeight();
                            onlineLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.numberOfBlocks",
                                new Object[]{"" + blockHeight}));
                        }
                    }
                }
            }
        });

        statusLabel = new JLabel();

        statusBar.addZone("online", onlineLabel, "12%", "left");
        statusBar.addZone("network", statusLabel, "*", "");
        statusBar.addZone("filler2", new JPanel(), "0", "right");

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 1;
        constraints.weighty = 0.01;
        contentPane.add(statusBar, constraints);
    }

    private JPanel createHeaderPanel() {
        JPanel headerPanel = new JPanel();

        headerPanel.setMinimumSize(new Dimension(700, 70));
        headerPanel.setPreferredSize(new Dimension(700, 70));
        headerPanel.setOpaque(false);
        headerPanel.setBackground(this.getBackground());

        headerPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel();
        filler1.setMinimumSize(new Dimension(30, 30));
        filler1.setMaximumSize(new Dimension(30, 30));
        filler1.setPreferredSize(new Dimension(30, 30));
        filler1.setOpaque(false);
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(filler1, constraints);

        JLabel walletIconLabel = new JLabel();
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            walletIconLabel.setIcon(createImageIcon(WALLET_ICON_FILE));
        } else {
            walletIconLabel.setIcon(createImageIcon(RTL_WALLET_ICON_FILE));
        }
        walletIconLabel.setOpaque(false);
        walletIconLabel.setMinimumSize(new Dimension(60, 80));
        walletIconLabel.setMaximumSize(new Dimension(60, 80));
        walletIconLabel.setPreferredSize(new Dimension(60, 80));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(walletIconLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setMinimumSize(new Dimension(10, 10));
        filler2.setMaximumSize(new Dimension(10, 10));
        filler2.setPreferredSize(new Dimension(10, 10));
        filler2.setOpaque(false);
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(filler2, constraints);

        estimatedBalanceTextLabel = new BlinkLabel();
        estimatedBalanceTextLabel.setHorizontalAlignment(JTextField.LEFT);
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
            MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 3);
        estimatedBalanceTextLabel.setFont(font);
        estimatedBalanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip"));

        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(estimatedBalanceTextLabel, constraints);

        availableBalanceTextLabel = new JLabel();
        availableBalanceTextLabel.setFont(availableBalanceTextLabel.getFont().deriveFont(12.0F));
        availableBalanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.availableToSpend.tooltip"));

        constraints.gridx = 4;
        constraints.gridy = 0;
        constraints.weightx = 3.0;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(availableBalanceTextLabel, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 0;
        constraints.weightx = 10;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(filler3, constraints);

        return headerPanel;
    }

    /**
     * @param constraints
     * @param contentPane
     *
     * @return
     */
    private JPanel addMenuBarAndCreateToolBar(GridBagConstraints constraints, Container contentPane) {
        // Create the menu bar
        JMenuBar menuBar = new JMenuBar();

        // Create the toolBar
        JPanel toolBarPanel = new JPanel();
        toolBarPanel.setOpaque(false);

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        // Build the File menu.
        JMenu fileMenu = new JMenu(localiser.getString("multiBitFrame.fileMenuText"));
        fileMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.fileMenuMnemonic"));
        menuBar.add(fileMenu);

        // Build the Trade menu.
        JMenu tradeMenu = new JMenu(localiser.getString("multiBitFrame.tradeMenuText"));
        tradeMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.tradeMenuMnemonic"));
        menuBar.add(tradeMenu);

        // Build the View menu.
        JMenu viewMenu = new JMenu(localiser.getString("multiBitFrame.viewMenuText"));
        viewMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.viewMenuMnemonic"));
        menuBar.add(viewMenu);

        // Build the Merchant menu.
        // see if it is required
        String showMerchantMenuString = controller.getModel().getUserPreference(MultiBitModel.SHOW_MERCHANT_MENU);
        boolean showMerchantMenu = Boolean.TRUE.toString().equalsIgnoreCase(showMerchantMenuString);
        JMenu merchantMenu = null;
        if (showMerchantMenu) {
            merchantMenu = new JMenu(localiser.getString("multiBitFrame.merchantMenuText"));
            merchantMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.merchantMenuMnemonic"));
            menuBar.add(merchantMenu);
        }

        // Build the Help menu.
        JMenu helpMenu = new JMenu(localiser.getString("multiBitFrame.helpMenuText"));
        helpMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.helpMenuMnemonic"));
        menuBar.add(helpMenu);

        // open wallet action
        OpenWalletAction openWalletAction = new OpenWalletAction(controller, createImageIcon(OPEN_WALLET_ICON_FILE));
        JMenuItem menuItem = new JMenuItem(openWalletAction);
        fileMenu.add(menuItem);

        // create new wallet action
        CreateNewWalletAction createNewWalletAction = new CreateNewWalletAction(controller,
            createImageIcon(CREATE_NEW_ICON_FILE), this);
        menuItem = new JMenuItem(createNewWalletAction);
        fileMenu.add(menuItem);

        // exit action
        if (!application.isMac()) {
            // non Macs have an Exit Menu item
            fileMenu.addSeparator();

            menuItem = new JMenuItem(new ExitAction(controller));
            fileMenu.add(menuItem);
        }

        // show help contents action
        ShowHelpContentsAction showHelpContentsAction = new ShowHelpContentsAction(controller, localiser,
            createImageIcon(HELP_CONTENTS_ICON_FILE));
        menuItem = new JMenuItem(showHelpContentsAction);
        helpMenu.add(menuItem);

        if (!application.isMac()) {
            // non Macs have a Help About menu item
            // help about action
            HelpAboutAction helpAboutAction = new HelpAboutAction(controller, createImageIcon(MULTIBIT_SMALL_ICON_FILE), this);
            menuItem = new JMenuItem(helpAboutAction);
            helpMenu.add(menuItem);
        }

        // Your Wallets action
        yourWalletsPanel = new JPanel(new BorderLayout());
        yourWalletsPanel.setBorder(normalBorder);
        yourWalletsPanel.setOpaque(false);

        YourWalletsAction myWalletsAction = new YourWalletsAction(controller, createImageIcon(YOUR_WALLETS_ICON_FILE));
        menuItem = new JMenuItem(myWalletsAction);
        viewMenu.add(menuItem);
        yourWalletsButton = new MultiBitButton(myWalletsAction);
        yourWalletsPanel.add(yourWalletsButton);

        activeWalletPanel = new JPanel(new BorderLayout());
        activeWalletPanel.setOpaque(false);
        activeWalletComboBox = createActiveWalletComboBox();
        activeWalletComboBox.setFont(yourWalletsButton.getFont());
        activeWalletComboBox.setVisible(false); // hidden until set
        activeWalletPanel.add(activeWalletComboBox, BorderLayout.NORTH);

        // receive bitcoin action
        receiveBitcoinPanel = new JPanel(new BorderLayout());
        receiveBitcoinPanel.setBorder(normalBorder);
        receiveBitcoinPanel.setOpaque(false);

        ReceiveBitcoinAction receiveBitcoinAction = new ReceiveBitcoinAction(controller, localiser,
            createImageIcon(RECEIVE_BITCOIN_ICON_FILE), this);
        tradeMenu.add(receiveBitcoinAction);
        receiveBitcoinButton = new MultiBitButton(receiveBitcoinAction);

        receiveBitcoinPanel.add(receiveBitcoinButton);

        // send bitcoin action
        SendBitcoinAction sendBitcoinAction = new SendBitcoinAction(controller, createImageIcon(SEND_BITCOIN_ICON_FILE), this);
        menuItem = new JMenuItem(sendBitcoinAction);
        tradeMenu.add(menuItem);

        sendBitcoinPanel = new JPanel(new BorderLayout());
        sendBitcoinPanel.setBorder(normalBorder);
        sendBitcoinPanel.setOpaque(false);
        sendBitcoinButton = new MultiBitButton(sendBitcoinAction);
        sendBitcoinButton.setHorizontalTextPosition(SwingConstants.LEADING);

        sendBitcoinPanel.add(sendBitcoinButton);

        // show preferences
        if (!application.isMac()) {
            // non Macs have a Preferences menu item
            // help about action
            ShowPreferencesAction showPreferencesAction = new ShowPreferencesAction(controller,
                createImageIcon(PREFERENCES_ICON_FILE));
            viewMenu.add(showPreferencesAction);
        }

        if (showMerchantMenu) {
            // create bulk addresses action
            ShowCreateBulkAddressesAction createBulkAddressesAction = new ShowCreateBulkAddressesAction(controller, null);
            menuItem = new JMenuItem(createBulkAddressesAction);
            merchantMenu.add(menuItem);
        }

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        filler1.setPreferredSize(new Dimension(4, 20));
        toolBarPanel.add(filler1);

        toolBarPanel.add(yourWalletsPanel);
        toolBarPanel.add(activeWalletPanel);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        filler2.setPreferredSize(new Dimension(90, 20));
        toolBarPanel.add(filler2);

        toolBarPanel.add(receiveBitcoinPanel);
        toolBarPanel.add(sendBitcoinPanel);
        toolBarPanel.setBorder(BorderFactory.createEmptyBorder());

        setJMenuBar(menuBar);

        return toolBarPanel;
    }

    /**
     * Returns an ImageIcon, or null if the path was invalid.
     */
    public static ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            log.error("org.multibit.MultiBitFrame#createImageIcon: Could not find file: " + path);
            return null;
        }
    }

    // MultiBitView methods

    /**
     * recreate all views
     */
    public void recreateAllViews(boolean clearCache) {
        recreateAllViews(clearCache, false);
    }

    /**
     * recreate all views
     */
    public void recreateAllViews(boolean clearCache, boolean initUI) {
        // do nothing on send
        if (View.SEND_BITCOIN_CONFIRM_VIEW == currentView) {
            return;
        }

        // close down current view
        if (currentView != 0) {
            navigateAwayFromView(currentView, View.TRANSACTIONS_VIEW, ViewSystem.NEW_VIEW_IS_PARENT_OF_PREVIOUS);
        }

        if (initUI) {
            this.localiser = controller.getLocaliser();
            Container contentPane = getContentPane();
            contentPane.removeAll();
            initUI();
            applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        }
        updateOnlineStatusText();

        updateHeader();

        setTitle(localiser.getString("multiBitFrame.title"));

        invalidate();
        validate();
        repaint();

        // recreate the views
        View yourWalletsView = null;
        if (!clearCache && viewFactory != null) {
            yourWalletsView = viewFactory.getView(View.YOUR_WALLETS_VIEW);
        }
        viewFactory = new ViewFactory(controller, this);
        if (!clearCache && yourWalletsView != null) {
            viewFactory.addView(View.YOUR_WALLETS_VIEW, yourWalletsView);
        }
    }

    public void setWalletFilename(String walletFilename) {
        if (walletFilename == null) {
            return;
        }

        File walletFile = new File(walletFilename);
        if (walletFile != null) {
            int loopIndex = 0;
            java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
            if (perWalletModelDataList != null) {
                for (PerWalletModelData loopModelData : perWalletModelDataList) {
                    if (loopModelData.getWalletFilename() != null
                        && loopModelData.getWalletFilename().equals(controller.getModel().getActiveWalletFilename())) {
                        if (loopIndex < activeWalletComboBox.getItemCount()) {
                            activeWalletComboBox.setSelectedIndex(loopIndex);
                        }

                        String walletDescription = loopModelData.getWalletDescription();
                        setActiveWalletTooltip(walletFile, walletDescription);

                        break;
                    }
                    loopIndex++;
                }
            }

            if (!walletFilename.equals("")) {
                activeWalletComboBox.setVisible(true);
            }
        }
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        if (currentView != 0) {
            View view = viewFactory.getView(currentView);
            if (view != null) {
                view.displayMessage(messageKey, messageData, titleKey);
            } else {
                log.debug("MultiBitFrame#displayMessage - no view with id {} to display message with key {}", currentView,
                    messageKey);
            }
        } else {
            log.debug("MultiBitFrame#displayMessage - no view on which to display message with key {}", messageKey);
        }
    }

    /**
     * display next view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    public void displayView(int viewToDisplay) {
        currentView = viewToDisplay;

        updateStatusLabel("");

        final View nextViewFinal = viewFactory.getView(viewToDisplay);
        final MultiBitFrame thisFrame = this;

        SwingUtilities.invokeLater(new Runnable() {
            @SuppressWarnings("deprecation")
            public void run() {

                yourWalletsPanel.setBorder(normalBorder);
                sendBitcoinPanel.setBorder(normalBorder);
                receiveBitcoinPanel.setBorder(normalBorder);

                if (nextViewFinal instanceof YourWalletsPanel) {
                    if (yourWalletsPanel != null) {
                        yourWalletsPanel.setBorder(underlineBorder);
                    }
                } else if (nextViewFinal instanceof SendBitcoinPanel) {
                    if (sendBitcoinPanel != null) {
                        sendBitcoinPanel.setBorder(underlineBorder);
                    }
                } else {
                    if (nextViewFinal instanceof ReceiveBitcoinPanel) {
                        if (receiveBitcoinPanel != null) {
                            receiveBitcoinPanel.setBorder(underlineBorder);
                        }
                    }
                }

                if (nextViewFinal instanceof JPanel) {
                    viewPanel.removeAll();
                    viewPanel.add((JPanel) nextViewFinal, BorderLayout.CENTER);
                }

                nextViewFinal.displayView();

                if (nextViewFinal instanceof JPanel) {
                    viewPanel.invalidate();
                    viewPanel.validate();
                    viewPanel.repaint();
                }

                thisFrame.setCursor(Cursor.DEFAULT_CURSOR);
            }
        });
    }

    /**
     * navigate away from view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextView, int relationshipOfNewViewToPrevious) {
        final int nextViewFinal = nextView;
        final int relationshipOfNewViewToPreviousFinal = relationshipOfNewViewToPrevious;

        final View viewToNavigateAwayFromFinal = viewFactory.getView(viewToNavigateAwayFrom);

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                viewToNavigateAwayFromFinal.navigateAwayFromView(nextViewFinal, relationshipOfNewViewToPreviousFinal);
            }
        });
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
                    onlineLabel.setForeground(new Color(0, 100, 0));
                    estimatedBalanceTextLabel.setBlinkEnabled(true);
                } else {
                    onlineLabel.setForeground(new Color(180, 0, 0));
                }
                onlineLabel.setText(onlineStatus);
            }
        });
    }

    public void updateStatusLabel(String newStatusLabel) {
        final String finalNewStatusLabel = newStatusLabel;
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                statusLabel.setText(finalNewStatusLabel);
            }
        });
    }

    public void blockDownloaded() {
        logger.debug("blockDownloaded");
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // update screen in case status bars have changed
                thisFrame.fireDataChanged();
                viewPanel.invalidate();
                viewPanel.validate();
                viewPanel.repaint();
                thisFrame.invalidate();
                thisFrame.validate();
                thisFrame.repaint();
            }
        });
    }

    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        // loop through all the wallets, updating them as required with the new
        // transaction
        try {
            java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            TransactionInput input = transaction.getInputs().get(0);
            Address from = input.getFromAddress();
            boolean dataHasChanged = false;
            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    // check bitcoin sent to me
                    BigInteger valueSentToMe = transaction.getValueSentToMe(perWalletModelData.getWallet());
                    if (valueSentToMe != null && valueSentToMe.compareTo(BigInteger.ZERO) > 0) {
                        logger.debug("Received " + controller.getLocaliser().bitcoinValueToString4(valueSentToMe, true, false)
                                + " from " + from.toString() + " to wallet '" + perWalletModelData.getWalletDescription() + "'");

                        // the perWalletModelData is marked as transactionDirty
                        // but is not written out immediately
                        // this is to avoid unnecessary 'Updates have stopped'
                        // when two MultiBit have a single wallet open.
                        // they will both get the incoming transaction
                        perWalletModelData.setTransactionDirty(true);
                        dataHasChanged = true;
                    }
                    // check bitcoin sent from me
                    BigInteger valueSentFromMe = transaction.getValueSentFromMe(perWalletModelData.getWallet());
                    if (valueSentFromMe != null && valueSentFromMe.compareTo(BigInteger.ZERO) > 0) {
                        logger.debug("Sent " + controller.getLocaliser().bitcoinValueToString4(valueSentFromMe, true, false)
                                + " from " + from.toString() + " from wallet '" + perWalletModelData.getWalletDescription()
                                + "'");

                        // the perWalletModelData is marked as transactionDirty
                        // but is not written out immediately
                        // this is to avoid unnecessary 'Updates have stopped'
                        // when two MultiBit have a single wallet open.
                        // they will both get the incoming transaction
                        perWalletModelData.setTransactionDirty(true);
                        dataHasChanged = true;
                    }
                    // check to see if the tx is mine, even though there is
                    // nothing sent to or from me
                    // this may be a transfer intrawallet
                    if (!dataHasChanged && transaction.isMine(perWalletModelData.getWallet())) {
                        logger.debug("Transaction " + transaction.getHashAsString()
                                + " is mine so wallet is marked transactionDirty  for wallet '"
                                + perWalletModelData.getWalletDescription() + "'");

                        // the perWalletModelData is marked as transactionDirty
                        // but is not written out immediately
                        // this is to avoid unnecessary 'Updates have stopped'
                        // when two MultiBit have a single wallet open.
                        // they will both get the incoming transaction
                        perWalletModelData.setTransactionDirty(true);
                        dataHasChanged = true;
                    }
                }
            }
            if (dataHasChanged) {
                fireDataChanged();
            }
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just crash.
            log.error(e.getMessage(), e);
            throw new IllegalStateException(e);
        }
    }

    public void onPendingCoinsReceived(Wallet wallet, Transaction transaction) {
        // loop through all the wallets, updating them as required with the new
        // transaction
        try {
            java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            TransactionInput input = transaction.getInputs().get(0);
            Address from = input.getFromAddress();
            boolean dataHasChanged = false;
            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    BigInteger value = transaction.getValueSentToMe(perWalletModelData.getWallet());
                    if (value != null && value.compareTo(BigInteger.ZERO) > 0) {
                        logger.debug("Received " + controller.getLocaliser().bitcoinValueToString4(value, true, false)
                                + " from " + from.toString() + " to wallet '" + perWalletModelData.getWalletDescription() + "'");
                        // the perWalletModelData is marked as transactionDirty
                        // but is not written out immediately
                        // this is to avoid unnecessary 'Updates have stopped'
                        // when two MultiBit have a single wallet open.
                        // they will both get the incoming transaction
                        perWalletModelData.setTransactionDirty(true);
                        dataHasChanged = true;
                    }
                    // check bitcoin sent from me
                    BigInteger valueSentFromMe = transaction.getValueSentFromMe(perWalletModelData.getWallet());
                    if (valueSentFromMe != null && valueSentFromMe.compareTo(BigInteger.ZERO) > 0) {
                        logger.debug("Sent " + controller.getLocaliser().bitcoinValueToString4(valueSentFromMe, true, false)
                                + " from " + from.toString() + " from wallet '" + perWalletModelData.getWalletDescription()
                                + "'");

                        // the perWalletModelData is marked as transactionDirty
                        // but is not written out immediately
                        // this is to avoid unnecessary 'Updates have stopped'
                        // when two MultiBit have a single wallet open.
                        // they will both get the incoming transaction
                        perWalletModelData.setTransactionDirty(true);
                        dataHasChanged = true;
                    }
                    // check to see if the tx is mine, even though there is
                    // nothing sent to or from me
                    // this may be a transfer intrawallet
                    if (!dataHasChanged && transaction.isMine(perWalletModelData.getWallet())) {
                        logger.debug("Transaction " + transaction.getHashAsString()
                                + " is mine so wallet is marked transactionDirty  for wallet '"
                                + perWalletModelData.getWalletDescription() + "'");

                        // the perWalletModelData is marked as transactionDirty
                        // but is not written out immediately
                        // this is to avoid unnecessary 'Updates have stopped'
                        // when two MultiBit have a single wallet open.
                        // they will both get the incoming transaction
                        perWalletModelData.setTransactionDirty(true);
                        dataHasChanged = true;
                    }
                    if (dataHasChanged) {
                        if (perWalletModelData.getWalletFilename().equals(controller.getModel().getActiveWalletFilename())) {
                            // blink the total
                            estimatedBalanceTextLabel.blink(controller.getLocaliser().bitcoinValueToString4(
                                    controller.getModel().getActiveWalletEstimatedBalance(), true, false));
                        }

                    }
                }
            }
            if (dataHasChanged) {
                fireDataChanged();
            }
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just crash.
            log.error(e.getMessage(), e);
            throw new IllegalStateException(e);
        }
    }

    /**
     * one of the wallets has been reorganised due to a block chain reorganise
     */
    public void onReorganise(Wallet wallet) {
        log.info("Wallet has been reorganised.");
        recreateAllViews(true);
    }

    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        if (controller.getModel().getActiveWalletFilename() != null
            && controller.getModel().getActiveWalletFilename().equals(perWalletModelData.getWalletFilename())) {
            updateStatusLabel(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + " "
                + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2"));
        }
        fireDataChanged();
    }

    /**
     * update the UI after the model data has changed
     */
    public void fireDataChanged() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // update the header
                updateHeader();

                // tell the current view to update itself
                View currentViewView = viewFactory.getView(currentView);
                if (currentViewView != null) {
                    currentViewView.updateView();
                }

                viewPanel.invalidate();
                thisFrame.invalidate();
                viewPanel.validate();
                thisFrame.validate();
                thisFrame.repaint();
            }
        });
    }

    private void updateHeader() {
        if (controller.getModel().getActivePerWalletModelData() != null
            && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
            // files have been changed by another process - blank totals
            // and put 'Updates stopped' message
            estimatedBalanceTextLabel.setText(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.text"));
            setUpdatesStoppedTooltip(estimatedBalanceTextLabel);
            availableBalanceTextLabel.setText("");
        } else {
            estimatedBalanceTextLabel.setText(controller.getLocaliser().bitcoinValueToString4(
                    controller.getModel().getActiveWalletEstimatedBalance(), true, false));
            estimatedBalanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip"));

            if (model.getActiveWalletAvailableBalance() != null
                && model.getActiveWalletAvailableBalance().equals(controller.getModel().getActiveWalletEstimatedBalance())) {
                availableBalanceTextLabel.setText("");
            } else {
                availableBalanceTextLabel.setText(controller.getLocaliser().getString(
                        "multiBitFrame.availableToSpend",
                        new Object[] { controller.getLocaliser().bitcoinValueToString4(model.getActiveWalletAvailableBalance(),
                                true, false) }));
            }
        }

        String walletFilename = controller.getModel().getActiveWalletFilename();
        if (walletFilename == null) {
            setWalletFilename("");
        } else {
            setWalletFilename(walletFilename);
        }

    }

    private JComboBox createActiveWalletComboBox() {
        java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

        Integer[] indexArray = new Integer[perWalletModelDataList.size()];
        int index = 0;
        for (@SuppressWarnings("unused")
        PerWalletModelData loopModelData : perWalletModelDataList) {
            indexArray[index] = index;
            index++;
        }
        activeWalletComboBox = new JComboBox(indexArray);
        activeWalletComboBox.setOpaque(false);
        activeWalletPanel.setBorder(normalBorder);

        ComboBoxRenderer renderer = new ComboBoxRenderer();
        renderer.setMinimumSize(new Dimension(200, 30));

        activeWalletComboBox.setRenderer(renderer);

        String activeWalletFileName = null;
        if (controller.getModel().getActiveWallet() != null) {
            activeWalletFileName = controller.getModel().getActiveWalletFilename();
        }

        if (activeWalletFileName != null) {
            int startingIndex = 0;
            Integer walletIndex = 0;
            for (PerWalletModelData loopWalletData : perWalletModelDataList) {
                if (activeWalletFileName.equals(loopWalletData.getWalletFilename())) {
                    walletIndex = startingIndex;
                    break;
                }
                startingIndex++;
            }
            if (walletIndex != 0) {
                activeWalletComboBox.setSelectedItem(walletIndex.intValue());
            }
        }

        // add change listener
        activeWalletComboBox.addItemListener(new ChangeActiveWalletItemListener());

        return activeWalletComboBox;
    }

  class ChangeActiveWalletItemListener implements ItemListener {
        public ChangeActiveWalletItemListener() {

        }

        public void itemStateChanged(ItemEvent e) {
            JComboBox activeWalletComboBox = (JComboBox) e.getSource();
            int selectedIndex = activeWalletComboBox.getSelectedIndex();
            PerWalletModelData selectedWalletModelData = controller.getModel().getPerWalletModelDataList().get(selectedIndex);
            if (selectedWalletModelData != null
                && !controller.getModel().getActiveWalletFilename().equals(selectedWalletModelData.getWalletFilename())) {
                controller.getModel().setActiveWalletByFilename(selectedWalletModelData.getWalletFilename());
                controller.fireWalletChanged();
                controller.fireDataChanged();
                controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
            }
        }
    }



    // Macify application methods

    @Override
    @Deprecated
    public void handleAbout(ApplicationEvent event) {
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_HELP_ABOUT);
        event.setHandled(true);
    }

    @Override
    @Deprecated
    public void handleOpenApplication(ApplicationEvent event) {
        // Ok, we know our application started
        // Not much to do about that..
    }

    @Override
    @Deprecated
    public void handleOpenFile(ApplicationEvent event) {
        // TODO i18n required
        JOptionPane.showMessageDialog(this, "Sorry, opening of files with double click is not yet implemented.  Wallet was "
            + event.getFilename());
    }

    @Override
    @Deprecated
    public void handlePreferences(ApplicationEvent event) {
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_PREFERENCES);
    }

    @Override
    @Deprecated
    public void handlePrintFile(ApplicationEvent event) {
        // TODO i18n required
        JOptionPane.showMessageDialog(this, "Sorry, printing not implemented");
    }

    @Override
    @Deprecated
    public void handleQuit(ApplicationEvent event) {
        ExitAction exitAction = new ExitAction(controller);
        exitAction.actionPerformed(null);
    }

    @Override
    public void handleReOpenApplication(ApplicationEvent event) {
        setVisible(true);
    }

    public void setActiveWalletTooltip(File walletFile, String walletDescription) {
        // multiline tool tip text
        String toolTipText = "<html><font face=\"sansserif\">";
        if (walletDescription != null && !"".equals(walletDescription)) {
            toolTipText = toolTipText + walletDescription + "<br>";
        }
        toolTipText = toolTipText + walletFile.getAbsolutePath() + "</font></html>";
        activeWalletComboBox.setToolTipText(toolTipText);
    }

    public void setUpdatesStoppedTooltip(JComponent component) {
        // multiline tool tip text
        String toolTipText = "<html><font face=\"sansserif\">";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + "<br>";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2") + "<br>";
        toolTipText = toolTipText + "</font></html>";
        component.setToolTipText(toolTipText);
    }

    class ComboBoxRenderer extends JLabel implements ListCellRenderer {
        private static final long serialVersionUID = -3301957214353702172L;

        public ComboBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }

        /*
         * This method finds the image and text corresponding to the selected
         * value and returns the label, set up to display the text and image.
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {
            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            int selectedIndex = 0;
            if (value != null) {
                selectedIndex = (Integer) value;
            }
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            // Set the icon and text.
            int loopIndex = 0;
            java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
            if (perWalletModelDataList != null) {
                for (PerWalletModelData loopModelData : perWalletModelDataList) {
                    if (selectedIndex == loopIndex) {
                        if (loopModelData.getWalletFilename() != null) {
                            File walletFile = new File(loopModelData.getWalletFilename());
                            if (walletFile != null) {
                                String walletFilenameFull = walletFile.getName();
                                String walletFilenameShort = walletFilenameFull.replaceAll("\\.wallet", "");
                                this.setText(walletFilenameShort);

                                // multiline tool tip text
                                setActiveWalletTooltip(walletFile, loopModelData.getWalletDescription());
                            }
                        }

                        break;
                    }
                    loopIndex++;
                }
            }

            setFont(list.getFont());

            return this;
        }
    }

    @Override
    public void newWalletCreated() {
        activeWalletPanel.remove(activeWalletComboBox);
        activeWalletComboBox = this.createActiveWalletComboBox();
        activeWalletComboBox.setFont(yourWalletsButton.getFont());

        ComboBoxRenderer renderer = new ComboBoxRenderer();
        renderer.setMinimumSize(new Dimension(200, 50));
        activeWalletComboBox.setRenderer(renderer);
        activeWalletPanel.add(activeWalletComboBox, BorderLayout.CENTER);

        recreateAllViews(true);
    }
}
