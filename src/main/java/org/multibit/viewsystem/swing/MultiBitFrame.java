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
package org.multibit.viewsystem.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.math.BigInteger;
import java.util.Timer;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.platform.GenericApplication;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.CreateNewWalletAction;
import org.multibit.viewsystem.swing.action.DeleteWalletAction;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.multibit.viewsystem.swing.action.HelpAboutAction;
import org.multibit.viewsystem.swing.action.MnemonicUtil;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.action.ReceiveBitcoinAction;
import org.multibit.viewsystem.swing.action.ResetTransactionsAction;
import org.multibit.viewsystem.swing.action.SendBitcoinAction;
import org.multibit.viewsystem.swing.action.ShowCreateBulkAddressesAction;
import org.multibit.viewsystem.swing.action.ShowExportPrivateKeysAction;
import org.multibit.viewsystem.swing.action.ShowHelpContentsAction;
import org.multibit.viewsystem.swing.action.ShowImportPrivateKeysAction;
import org.multibit.viewsystem.swing.action.ShowPreferencesAction;
import org.multibit.viewsystem.swing.action.YourWalletsAction;
import org.multibit.viewsystem.swing.view.ViewFactory;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.GradientPanel;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.yourwallets.YourWalletsPanel;
import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.VerificationException;
import com.google.bitcoin.core.Wallet;

/*
 * JFrame displaying Swing version of MultiBit
 *
 */
public class MultiBitFrame extends JFrame implements ViewSystem, ApplicationListener {

    private static final Logger log = LoggerFactory.getLogger(MultiBitFrame.class);

    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.72D;

    public static final String EXAMPLE_LONG_FIELD_TEXT = "TheQuickBrownFoxJumpsOverTheLazyDog";
    public static final int WIDTH_OF_LONG_FIELDS = 320;
    public static final int WIDTH_OF_AMOUNT_FIELD = 160;

    private StatusBar statusBar;
    private boolean online = false;

    private static final long serialVersionUID = 7621813615342923041L;

    private MultiBitController controller;
    private MultiBitModel model;
    private Localiser localiser;

    private BlinkLabel estimatedBalanceTextLabel;

    public BlinkLabel getEstimatedBalanceTextLabel() {
        return estimatedBalanceTextLabel;
    }

    private MultiBitLabel availableBalanceTextLabel;

    /**
     * list of wallets shown in left hand column
     */
    private View yourWalletsView;

    private MultiBitFrame thisFrame;

    private static final int TOOLTIP_DISMISSAL_DELAY = 10000; // millisecs

    /**
     * Provide the Application reference during construction
     */
    private final GenericApplication application;

    /**
     * the tabbed pane containing the views
     * 
     */
    private JTabbedPane viewTabbedPane;

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

        FontSizer.INSTANCE.initialise(controller);
        UIManager.put("ToolTip.font", FontSizer.INSTANCE.getAdjustedDefaultFont());

        setCursor(Cursor.WAIT_CURSOR);
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        setTitle(localiser.getString("multiBitFrame.title"));

        ToolTipManager.sharedInstance().setDismissDelay(TOOLTIP_DISMISSAL_DELAY);

        final MultiBitController finalController = controller;

        // TODO Examine how this fits in with the controller onQuit() event
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent arg0) {
                org.multibit.viewsystem.swing.action.ExitAction exitAction = new org.multibit.viewsystem.swing.action.ExitAction(
                        finalController, thisFrame);
                exitAction.actionPerformed(null);
            }
        });

        getContentPane().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        sizeAndCenter();

        viewFactory = new ViewFactory(controller, this);

        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        recreateAllViews(false);

        // initialise status bar
        statusBar.initialise();

        estimatedBalanceTextLabel.setText(controller.getLocaliser().bitcoinValueToString4(model.getActiveWalletEstimatedBalance(),
                true, false));

        availableBalanceTextLabel.setText(controller.getLocaliser().getString(
                "multiBitFrame.availableToSpend",
                new Object[] { controller.getLocaliser()
                        .bitcoinValueToString4(model.getActiveWalletAvailableBalance(), true, false) }));

        estimatedBalanceTextLabel.setFocusable(true);
        estimatedBalanceTextLabel.requestFocusInWindow();

        yourWalletsView.displayView();

        pack();
        setVisible(true);

        fileChangeTimer = new Timer();
        fileChangeTimer.schedule(new FileChangeTimerTask(controller, this), 0, FileChangeTimerTask.DEFAULT_REPEAT_RATE);
    }

    public GenericApplication getApplication() {
        return application;
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
    }

    private void initUI() {
        Container contentPane = getContentPane();
        contentPane.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        // set the application icon
        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }

        headerPanel = new GradientPanel();
        headerPanel.setLayout(new GridBagLayout());
        headerPanel.setOpaque(true);

        JPanel balancePanel = createBalancePanel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(balancePanel, constraints);

        addMenuBar(constraints, contentPane);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 1.0;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(headerPanel, constraints);

        // create the Your Wallets panel
        yourWalletsView = new YourWalletsPanel(controller, this);

        // Create the tabbedpane that holds the views
        viewTabbedPane = new JTabbedPane();

        // Create a split pane with the two scroll panes in it.
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, (JPanel) yourWalletsView, viewTabbedPane);
        splitPane.setOneTouchExpandable(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1.0;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(splitPane, constraints);

        statusBar = new StatusBar(controller, this);
        statusBar.updateOnlineStatusText(online);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.01;
        constraints.gridwidth = 2;
        contentPane.add(statusBar, constraints);
    }

    private JPanel createBalancePanel() {
        JPanel headerPanel = new JPanel();

        headerPanel.setMinimumSize(new Dimension(700, 55));
        headerPanel.setPreferredSize(new Dimension(700, 55));
        headerPanel.setOpaque(false);
        headerPanel.setBackground(this.getBackground());

        headerPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel();
        filler1.setMinimumSize(new Dimension(20, 20));
        filler1.setMaximumSize(new Dimension(20, 20));
        filler1.setPreferredSize(new Dimension(20, 20));
        filler1.setOpaque(false);
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(filler1, constraints);

        JLabel walletIconLabel = new JLabel();
        if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()).isLeftToRight()) {
            walletIconLabel.setIcon(ImageLoader.createImageIcon(ImageLoader.WALLET_ICON_FILE));
        } else {
            walletIconLabel.setIcon(ImageLoader.createImageIcon(ImageLoader.RTL_WALLET_ICON_FILE));
        }
        walletIconLabel.setOpaque(false);
        walletIconLabel.setMinimumSize(new Dimension(60, 50));
        walletIconLabel.setMaximumSize(new Dimension(60, 50));
        walletIconLabel.setPreferredSize(new Dimension(60, 50));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(walletIconLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setMinimumSize(new Dimension(8, 8));
        filler2.setMaximumSize(new Dimension(8, 8));
        filler2.setPreferredSize(new Dimension(8, 8));
        filler2.setOpaque(false);
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(filler2, constraints);

        estimatedBalanceTextLabel = new BlinkLabel(controller, true);
        estimatedBalanceTextLabel.setHorizontalAlignment(JTextField.LEFT);

        estimatedBalanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip"));

        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(estimatedBalanceTextLabel, constraints);

        availableBalanceTextLabel = new MultiBitLabel("", controller);
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
    private void addMenuBar(GridBagConstraints constraints, Container contentPane) {
        // Create the menu bar
        JMenuBar menuBar = new JMenuBar();

        ComponentOrientation componentOrientation = ComponentOrientation.getOrientation(controller.getLocaliser().getLocale());

        // Create the toolBar
        JPanel toolBarPanel = new JPanel();
        toolBarPanel.setOpaque(false);

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        // Build the File menu.
        JMenu fileMenu = new JMenu(localiser.getString("multiBitFrame.fileMenuText"));
        fileMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        fileMenu.setComponentOrientation(componentOrientation);

        fileMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.fileMenuMnemonic"));
        menuBar.add(fileMenu);

        // Build the Trade menu.
        JMenu tradeMenu = new JMenu(localiser.getString("multiBitFrame.tradeMenuText"));
        tradeMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        tradeMenu.setComponentOrientation(componentOrientation);
        tradeMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.tradeMenuMnemonic"));
        menuBar.add(tradeMenu);

        // Build the View menu.
        JMenu viewMenu = new JMenu(localiser.getString("multiBitFrame.viewMenuText"));
        viewMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        viewMenu.setComponentOrientation(componentOrientation);
        viewMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.viewMenuMnemonic"));
        menuBar.add(viewMenu);

        // Build the Tools menu.
        JMenu toolsMenu = new JMenu(localiser.getString("multiBitFrame.toolsMenuText"));
        toolsMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        toolsMenu.setComponentOrientation(componentOrientation);
        toolsMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.toolsMenuMnemonic"));
        menuBar.add(toolsMenu);

        // Build the Merchant menu.
        // see if it is required
        String showMerchantMenuString = controller.getModel().getUserPreference(MultiBitModel.SHOW_MERCHANT_MENU);
        boolean showMerchantMenu = Boolean.TRUE.toString().equalsIgnoreCase(showMerchantMenuString);
        JMenu merchantMenu = null;
        if (showMerchantMenu) {
            merchantMenu = new JMenu(localiser.getString("multiBitFrame.merchantMenuText"));
            merchantMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            merchantMenu.setComponentOrientation(componentOrientation);
            merchantMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.merchantMenuMnemonic"));
            menuBar.add(merchantMenu);
        }

        // Build the Help menu.
        JMenu helpMenu = new JMenu(localiser.getString("multiBitFrame.helpMenuText"));
        helpMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        helpMenu.setComponentOrientation(componentOrientation);
        helpMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.helpMenuMnemonic"));
        menuBar.add(helpMenu);

        // create new wallet action
        CreateNewWalletAction createNewWalletAction = new CreateNewWalletAction(controller,
                ImageLoader.createImageIcon(ImageLoader.CREATE_NEW_ICON_FILE), this);
        JMenuItem menuItem = new JMenuItem(createNewWalletAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        fileMenu.add(menuItem);

        // open wallet action
        OpenWalletAction openWalletAction = new OpenWalletAction(controller,
                ImageLoader.createImageIcon(ImageLoader.OPEN_WALLET_ICON_FILE), this);
        menuItem = new JMenuItem(openWalletAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        fileMenu.add(menuItem);
 
        DeleteWalletAction deleteWalletAction = new DeleteWalletAction(controller,  ImageLoader.createImageIcon(ImageLoader.DELETE_WALLET_ICON_FILE), this);
        menuItem = new JMenuItem(deleteWalletAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        fileMenu.add(menuItem);
 
        // exit action
        if (!application.isMac()) {
            // non Macs have an Exit Menu item
            fileMenu.addSeparator();

            menuItem = new JMenuItem(new ExitAction(controller, this));
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            fileMenu.add(menuItem);
        }

        // show help contents action
        ShowHelpContentsAction showHelpContentsAction = new ShowHelpContentsAction(controller, localiser,
                ImageLoader.createImageIcon(ImageLoader.HELP_CONTENTS_ICON_FILE));
        menuItem = new JMenuItem(showHelpContentsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        helpMenu.add(menuItem);

        if (!application.isMac()) {
            // non Macs have a Help About menu item
            // help about action
            HelpAboutAction helpAboutAction = new HelpAboutAction(controller,
                    ImageLoader.createImageIcon(ImageLoader.MULTIBIT_SMALL_ICON_FILE), this);
            menuItem = new JMenuItem(helpAboutAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            helpMenu.add(menuItem);
        }

        YourWalletsAction myWalletsAction = new YourWalletsAction(controller,
                ImageLoader.createImageIcon(ImageLoader.YOUR_WALLETS_ICON_FILE));
        menuItem = new JMenuItem(myWalletsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        viewMenu.add(menuItem);

        // send bitcoin action
        SendBitcoinAction sendBitcoinAction = new SendBitcoinAction(controller,
                ImageLoader.createImageIcon(ImageLoader.SEND_BITCOIN_ICON_FILE), this);
        menuItem = new JMenuItem(sendBitcoinAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        tradeMenu.add(menuItem);

        ReceiveBitcoinAction receiveBitcoinAction = new ReceiveBitcoinAction(controller, localiser,
                ImageLoader.createImageIcon(ImageLoader.RECEIVE_BITCOIN_ICON_FILE), this);
        menuItem = new JMenuItem(receiveBitcoinAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        tradeMenu.add(menuItem);

         // show preferences
        if (!application.isMac()) {
            // non Macs have a Preferences menu item
            // help about action
            ShowPreferencesAction showPreferencesAction = new ShowPreferencesAction(controller,
                    ImageLoader.createImageIcon(ImageLoader.PREFERENCES_ICON_FILE));
            menuItem = new JMenuItem(showPreferencesAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            viewMenu.add(menuItem);
        }

        // import private keys
        ShowImportPrivateKeysAction showImportPrivateKeysAction = new ShowImportPrivateKeysAction(controller,
                ImageLoader.createImageIcon(ImageLoader.IMPORT_PRIVATE_KEYS_ICON_FILE));
        menuItem = new JMenuItem(showImportPrivateKeysAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        toolsMenu.add(menuItem);

        // export private keys
        ShowExportPrivateKeysAction showExportPrivateKeysAction = new ShowExportPrivateKeysAction(controller,
                ImageLoader.createImageIcon(ImageLoader.EXPORT_PRIVATE_KEYS_ICON_FILE));
        menuItem = new JMenuItem(showExportPrivateKeysAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        toolsMenu.add(menuItem);

        toolsMenu.addSeparator();

        ResetTransactionsAction resetTransactionsAction = new ResetTransactionsAction(controller,
                ImageLoader.createImageIcon(ImageLoader.RESET_TRANSACTIONS_ICON_FILE));
        menuItem = new JMenuItem(resetTransactionsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        toolsMenu.add(menuItem);

        if (showMerchantMenu) {
            // create bulk addresses action
            ShowCreateBulkAddressesAction createBulkAddressesAction = new ShowCreateBulkAddressesAction(controller, null);
            menuItem = new JMenuItem(createBulkAddressesAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            merchantMenu.add(menuItem);
        }

        setJMenuBar(menuBar);

        return;
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
        // close down current view
        if (currentView != 0) {
            navigateAwayFromView(currentView);
        }

        if (initUI) {
            this.localiser = controller.getLocaliser();
            Container contentPane = getContentPane();
            contentPane.removeAll();
            initUI();
            applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        }

        statusBar.refreshOnlineStatusText();

        updateHeader();

        setTitle(localiser.getString("multiBitFrame.title"));

        viewFactory = new ViewFactory(controller, this);
        
        // tell all the tabs in the tabbedPane to update
        if (viewTabbedPane != null) {
            for (int i =0; i < viewTabbedPane.getTabCount(); i++) {
                JPanel tabComponent = (JPanel)viewTabbedPane.getComponentAt(i);
                Component[] components = tabComponent.getComponents();
                if (components != null && components.length > 0 && components[0] instanceof View) {
                    ((View)components[0]).updateView();
                }
            }
        }

        invalidate();
        validate();
        repaint();
    }

    /**
     * display next view on Swing event dispatch thread
     */
    public void displayView(int viewToDisplay) {
        // open wallet view obselete - show Your wallets
        if (View.OPEN_WALLET_VIEW == viewToDisplay) {
            viewToDisplay = View.YOUR_WALLETS_VIEW;
        }

        // show wallets view always on display
        if (View.YOUR_WALLETS_VIEW == viewToDisplay) {
            yourWalletsView.displayView();
            return;
        }

        currentView = viewToDisplay;

        final View nextViewFinal = viewFactory.getView(viewToDisplay);

        if (nextViewFinal == null) {
            log.debug("Cannot display view " + viewToDisplay);
            return;
        }
        final MultiBitFrame thisFrame = this;

        SwingUtilities.invokeLater(new Runnable() {
            @SuppressWarnings("deprecation")
            public void run() {
                String viewTitle = nextViewFinal.getViewTitle();
                boolean foundTab = false;
                if (viewTabbedPane.getTabCount() > 0) {
                    for (int i = 0; i < viewTabbedPane.getTabCount(); i++) {
                        if (viewTitle != null && viewTitle.equals(viewTabbedPane.getTitleAt(i))) {
                            foundTab = true;
                            log.debug("Tab panel = " + System.identityHashCode(viewTabbedPane.getTabComponentAt(i))  + ", nextPanel = " + System.identityHashCode(nextViewFinal));
                            ((JPanel)viewTabbedPane.getComponentAt(i)).removeAll();
                            ((JPanel)viewTabbedPane.getComponentAt(i)).add((JPanel) nextViewFinal);
                        }
                    }
                }

                if (!foundTab) {
                    JPanel tabOutlinePanel = new JPanel(new BorderLayout());
                    tabOutlinePanel.add( (JPanel) nextViewFinal, BorderLayout.CENTER);
                    viewTabbedPane.addTab(nextViewFinal.getViewTitle(), nextViewFinal.getViewIcon(),tabOutlinePanel);
                    viewTabbedPane.setSelectedComponent(tabOutlinePanel);
                }

                nextViewFinal.displayView();

                if (nextViewFinal instanceof JPanel) {
                    ((JPanel) nextViewFinal).invalidate();
                    ((JPanel) nextViewFinal).validate();
                    ((JPanel) nextViewFinal).repaint();
                }

                thisFrame.setCursor(Cursor.DEFAULT_CURSOR);
            }
        });
    }

    /**
     * navigate away from view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    public void navigateAwayFromView(int viewToNavigateAwayFrom) {
        if (View.YOUR_WALLETS_VIEW == viewToNavigateAwayFrom) {
            // do nothing
            return;
        }

        final View viewToNavigateAwayFromFinal = viewFactory.getView(viewToNavigateAwayFrom);

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                viewToNavigateAwayFromFinal.navigateAwayFromView();
            }
        });
    }

    public void nowOnline() {
        online = true;
        if (statusBar != null) {
            statusBar.updateOnlineStatusText(true);
        }
    }

    public void nowOffline() {
        online = false;
        if (statusBar != null) {
            statusBar.updateOnlineStatusText(false);
        }
    }

    public void updateStatusLabel(String newStatusLabel, boolean clearAutomatically) {
        if (statusBar != null) {
            statusBar.updateStatusLabel(newStatusLabel, clearAutomatically);
        }
    }

    public void updateStatusLabel(String newStatusLabel, double percentComplete) {
        if (statusBar != null) {
            if (percentComplete == 0) {
                statusBar.startSync();
            }
            statusBar.updateSync((int) percentComplete, newStatusLabel);

            if (percentComplete == 100) {
                statusBar.finishSync();
            }
        }
    }

    public void blockDownloaded() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // update screen in case status bars have changed
                thisFrame.fireDataChanged();
            }
        });
    }

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        processNewCoin(wallet, transaction);
    }

    private void processNewCoin(Wallet wallet, Transaction transaction) {
        // loop through all the wallets, updating them as required with the new
        // transaction
        log.debug("processNewCoin is processing transaction " + transaction.toString());
        try {
            java.util.List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    try {
                        Wallet loopWallet = perWalletModelData.getWallet();
                        if (loopWallet.isTransactionRelevant(transaction, true)) {
                            // the perWalletModelData is marked as dirty

                            // check to see if the transaction is already in the
                            // wallet
                            if (loopWallet.getTransaction(transaction.getHash()) == null) {
                                log.debug("processNewCoin is receivingPending");
                                loopWallet.receivePending(transaction);
                            }
                            perWalletModelData.setDirty(true);
                            log.debug("Marking wallet '" + perWalletModelData.getWalletFilename() + "' as dirty.");
                            fireDataChanged();
                        }
                    } catch (VerificationException e) {
                        e.printStackTrace();
                    }
                }
            }
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just log it
            log.error(e.getMessage(), e);
        }
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        processNewCoin(wallet, transaction);
    }

    /**
     * one of the wallets has been reorganised due to a block chain reorganise
     */
    public void onReorganize(Wallet wallet) {
        log.info("Wallet has been reorganised.");
        recreateAllViews(true);
    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
        log.debug("Transaction confidence changed for tx " + transaction.toString());
    }

    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        if (controller.getModel().getActiveWalletFilename() != null
                && controller.getModel().getActiveWalletFilename().equals(perWalletModelData.getWalletFilename())) {
            updateStatusLabel(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + " "
                    + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2"), true);
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

                viewTabbedPane.invalidate();
                thisFrame.invalidate();
                viewTabbedPane.validate();
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
        ExitAction exitAction = new ExitAction(controller, this);
        exitAction.actionPerformed(null);
    }

    @Override
    public void handleReOpenApplication(ApplicationEvent event) {
        setVisible(true);
    }

    public void setUpdatesStoppedTooltip(JComponent component) {
        // multiline tool tip text
        String toolTipText = "<html><font face=\"sansserif\">";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + "<br>";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2") + "<br>";
        toolTipText = toolTipText + "</font></html>";
        component.setToolTipText(toolTipText);
    }

    @Override
    /**
     * a new wallet has been created or opened
     * (this may be called from a SwingWorker thread 
     */
    public void newWalletCreated() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                recreateAllViews(true);
                displayView(currentView);

                viewTabbedPane.invalidate();
                thisFrame.invalidate();
                viewTabbedPane.validate();
                thisFrame.validate();
                thisFrame.repaint();
            }
        });
    }

    public void bringToFront() {
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                toFront();
                repaint();
            }
        });
    }

    @Override
    public void updateCurrentView() {
        final View currentViewFinal = viewFactory.getView(currentView);

        if (currentViewFinal == null) {
            log.debug("Cannot update view " + currentView);
            return;
        }
        final MultiBitFrame thisFrame = this;

        SwingUtilities.invokeLater(new Runnable() {
            @SuppressWarnings("deprecation")
            public void run() {
                log.debug("Updating view '" + currentViewFinal.getViewTitle() + "'");
                currentViewFinal.updateView();

                if (currentViewFinal instanceof JPanel) {
                    ((JPanel) currentViewFinal).invalidate();
                    ((JPanel) currentViewFinal).repaint();
                }

                thisFrame.setCursor(Cursor.DEFAULT_CURSOR);
            }
        });
     }
}
