package org.multibit.viewsystem.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Timer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import org.apache.log4j.Logger;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.multibit.viewsystem.swing.action.HelpAboutAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.action.ReceiveBitcoinAction;
import org.multibit.viewsystem.swing.action.SaveWalletAsAction;
import org.multibit.viewsystem.swing.action.SendBitcoinAction;
import org.multibit.viewsystem.swing.action.ShowHelpContentsAction;
import org.multibit.viewsystem.swing.action.ShowPreferencesAction;
import org.multibit.viewsystem.swing.action.ShowTransactionsAction;
import org.multibit.viewsystem.swing.view.HeaderPanel;
import org.multibit.viewsystem.swing.view.MultiBitButton;
import org.multibit.viewsystem.swing.view.ReceiveBitcoinPanel;
import org.multibit.viewsystem.swing.view.SendBitcoinPanel;
import org.multibit.viewsystem.swing.view.ShowTransactionsPanel;
import org.multibit.viewsystem.swing.view.ViewFactory;

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
    private static final String PREFERENCES_ICON_FILE = "/images/preferences.jpg";
    private static final String HELP_CONTENTS_ICON_FILE = "/images/helpContents.jpg";
    private static final String MULTIBIT_SMALL_ICON_FILE = "/images/multibit-small.jpg";
    private static final String MULTIBIT_ICON_FILE = "/images/multibit.gif";
    private static final String TRANSACTIONS_ICON_FILE = "/images/information.jpg";
    private static final String WALLET_ICON_FILE = "/images/wallet.png";

    public static final String MULTIBIT_FONT_NAME = "Dialog";
    public static final int MULTIBIT_FONT_STYLE = Font.PLAIN;
    public static final int MULTIBIT_LARGE_FONT_SIZE = 14;

    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.75D;

    private static final long serialVersionUID = 7621813615342923041L;

    private MultiBitController controller;
    private MultiBitModel model;
    private Localiser localiser;

    private JLabel balanceTextLabel;

    private JLabel walletNameLabel;

    private JLabel onlineLabel;
    private JLabel statusLabel;
    private boolean isOnline;

    private MultiBitFrame thisFrame;

    private MultiBitButton sendBitcoinButton;
    private MultiBitButton receiveBitcoinButton;
    private MultiBitButton showTransactionsButton;
    private MultiBitButton openWalletButton;

    /**
     * the panel containing the main view
     */
    private JPanel viewPanel;

    public Logger logger = Logger.getLogger(MultiBitFrame.class.getName());

    /**
     * the view that the controller is telling us to display an int - one of the
     * View constants
     * 
     */
    private int currentView;

    private ViewFactory viewFactory;

    private Timer refreshTimer;

    @SuppressWarnings("deprecation")
    public MultiBitFrame(MultiBitController controller) {
        this.controller = controller;
        this.model = controller.getModel();
        this.localiser = controller.getLocaliser();
        this.thisFrame = this;

        setCursor(Cursor.WAIT_CURSOR);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        setTitle(localiser.getString("multiBitFrame.title"));

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
        updateStatusLabel("");

        balanceTextLabel.setText(Localiser.bitcoinValueToFriendlyString3(model.getBalance(), true, false));
        balanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.text",
                new Object[] { Localiser.bitcoinValueToString(model.getBalance(), true, false) }));
        balanceTextLabel.setFocusable(true);
        balanceTextLabel.requestFocusInWindow();
        
        pack();
        setVisible(true);

        refreshTimer = new Timer();
        refreshTimer.schedule(new RefreshTimerTask(this), 0, 60000); // fires
                                                                     // once a
                                                                     // minute
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

        JPanel headerPanel = new HeaderPanel();
        headerPanel.setLayout(new GridBagLayout());

        JPanel balancePanel = createBalancePanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.15;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(balancePanel, constraints);

        JToolBar toolBar = addMenuBarAndCreateToolBar(constraints, contentPane);
        toolBar.setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, TOOLBAR_HEIGHT));
        toolBar.setOpaque(false);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.85;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(toolBar, constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.15;
        constraints.weighty = 0.01;
        constraints.anchor = GridBagConstraints.LINE_START;

        contentPane.add(headerPanel, constraints);

        viewPanel = new JPanel(new BorderLayout()); // initally blank
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(viewPanel, constraints);

        StatusBar statusBar = new StatusBar();
        statusBar.setMaximumSize(new Dimension(A_LARGE_NUMBER_OF_PIXELS, STATUSBAR_HEIGHT));
        statusBar.setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, STATUSBAR_HEIGHT));
        onlineLabel = new JLabel();
        onlineLabel.setHorizontalAlignment(SwingConstants.CENTER);
        statusLabel = new JLabel();
        statusBar.setZones(new String[] { "online", "network" }, new JComponent[] { onlineLabel, statusLabel }, new String[] {
                "12%", "*" });
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 1;
        constraints.weighty = 0.01;
        contentPane.add(statusBar, constraints);
    }

    private JPanel createBalancePanel() {
        JPanel balancePanel = new JPanel();

        balancePanel.setMinimumSize(new Dimension(600, 60));
        balancePanel.setPreferredSize(new Dimension(600, 60));
        balancePanel.setOpaque(false);
        balancePanel.setBackground(this.getBackground());

        balancePanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        balancePanel.add(filler1, constraints);

        JLabel walletIconLabel = new JLabel();
        walletIconLabel.setIcon(createImageIcon(WALLET_ICON_FILE));
        walletIconLabel.setOpaque(false);
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.2;
        constraints.anchor = GridBagConstraints.LINE_START;

        balancePanel.add(walletIconLabel, constraints);

        balanceTextLabel = new JLabel();
        balanceTextLabel.setHorizontalAlignment(JTextField.LEFT);
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 3);
        balanceTextLabel.setFont(font);

        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.2;
        constraints.anchor = GridBagConstraints.LINE_START;
        balancePanel.add(balanceTextLabel, constraints);
        walletNameLabel = new JLabel();
        walletNameLabel.setFont(walletNameLabel.getFont().deriveFont(12.0F));

        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 1.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        balancePanel.add(walletNameLabel, constraints);

        return balancePanel;
    }

    private JToolBar addMenuBarAndCreateToolBar(GridBagConstraints constraints, Container contentPane) {
        // Create the menu bar
        JMenuBar menuBar = new JMenuBar();

        // Create the toolBar
        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);

        // Enable rollover mode
        toolBar.setRollover(true);

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
        openWalletPanel.setBorder(BorderFactory.createEmptyBorder(0, 4, 3, 4));
        openWalletPanel.setOpaque(false);

        OpenWalletAction openWalletAction = new OpenWalletAction(controller, createImageIcon(OPEN_WALLET_ICON_FILE));
        JMenuItem menuItem = new JMenuItem(openWalletAction);
        fileMenu.add(menuItem);
        openWalletButton = new MultiBitButton(openWalletAction);
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

        // show transactions action
        JPanel showTransactionsPanel = new JPanel(new BorderLayout());
        showTransactionsPanel.setBorder(BorderFactory.createEmptyBorder(0, 4, 3, 4));
        showTransactionsPanel.setOpaque(false);
        ShowTransactionsAction showTransactionsAction = new ShowTransactionsAction(controller,
                createImageIcon(TRANSACTIONS_ICON_FILE));
        menuItem = new JMenuItem(showTransactionsAction);
        viewMenu.add(menuItem);
        showTransactionsButton = new MultiBitButton(showTransactionsAction);
        showTransactionsPanel.add(showTransactionsButton);

        // receive bitcoin action
        JPanel receiveBitcoinPanel = new JPanel(new BorderLayout());
        receiveBitcoinPanel.setBorder(BorderFactory.createEmptyBorder(0, 4, 3, 4));
        receiveBitcoinPanel.setOpaque(false);

        ReceiveBitcoinAction receiveBitcoinAction = new ReceiveBitcoinAction(controller, localiser,
                createImageIcon(RECEIVE_BITCOIN_ICON_FILE), this);
        tradeMenu.add(receiveBitcoinAction);
        receiveBitcoinButton = new MultiBitButton(receiveBitcoinAction);

        receiveBitcoinPanel.add(receiveBitcoinButton);
        toolBar.add(receiveBitcoinPanel);

        // send bitcoin action
        SendBitcoinAction sendBitcoinAction = new SendBitcoinAction(controller, createImageIcon(SEND_BITCOIN_ICON_FILE), this);
        menuItem = new JMenuItem(sendBitcoinAction);
        tradeMenu.add(menuItem);

        JPanel sendBitcoinPanel = new JPanel(new BorderLayout());
        sendBitcoinPanel.setBorder(BorderFactory.createEmptyBorder(0, 4, 3, 4));
        sendBitcoinPanel.setOpaque(false);
        sendBitcoinButton = new MultiBitButton(sendBitcoinAction);
        sendBitcoinPanel.add(sendBitcoinButton);
        toolBar.add(sendBitcoinPanel);

        toolBar.add(showTransactionsPanel);

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
            navigateAwayFromView(currentView, View.TRANSACTIONS_VIEW, ViewSystem.NEW_VIEW_IS_PARENT_OF_PREVIOUS); // home
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
        balanceTextLabel.setText(Localiser.bitcoinValueToFriendlyString3(model.getBalance(), true, false));
        balanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.text",
                new Object[] { Localiser.bitcoinValueToString(model.getBalance(), true, false) }));

        String walletFilename = model.getWalletFilename();
        if (walletFilename == null) {
            setWalletFilename("");
        } else {
            setWalletFilename(walletFilename);
        }

        setTitle(localiser.getString("multiBitFrame.title"));

        invalidate();
        validate();
        repaint();

        // recreate the views
        viewFactory = new ViewFactory(controller, this);
    }

    public void setWalletFilename(String walletFilename) {
        walletNameLabel.setText(localiser.getString("multiBitFrame.walletNameLabel.text", new Object[] { new File(
                walletFilename).getName() }));
        walletNameLabel.setToolTipText(walletFilename);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        if (currentView != 0) {
            View view = viewFactory.getView(currentView);
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
     * display next view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    public void displayView(int viewToDisplay) {
        currentView = viewToDisplay;

        updateStatusLabel("");

        final View nextViewFinal = viewFactory.getView(viewToDisplay);
        final MultiBitFrame thisFrame = this;

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {

                if (nextViewFinal instanceof SendBitcoinPanel) {
                    if (sendBitcoinButton != null) {
                        sendBitcoinButton.requestFocusInWindow();
                    }
                } else {
                    if (nextViewFinal instanceof ReceiveBitcoinPanel) {
                        if (receiveBitcoinButton != null) {
                            receiveBitcoinButton.requestFocusInWindow();
                        }
                    } else {
                        if (nextViewFinal instanceof ShowTransactionsPanel) {
                            if (showTransactionsButton != null) {
                                showTransactionsButton.requestFocusInWindow();
                            }
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
                } else {
                    onlineLabel.setForeground(new Color(180, 0, 0));
                }
                onlineLabel.setText(onlineStatus);
            }
        });
    }

    public void updateStatusLabel(String newStatusLabel) {
        final String finalUpdateDownloadStatus = newStatusLabel;
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                statusLabel.setText(finalUpdateDownloadStatus);
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
        // print out transaction details
        try {
            TransactionInput input = transaction.getInputs().get(0);
            Address from = input.getFromAddress();
            BigInteger value = transaction.getValueSentToMe(wallet);
            logger.debug("Received " + Localiser.bitcoinValueToString(value, true, false) + " from " + from.toString());
            wallet.saveToFile(new File(controller.getModel().getWalletFilename()));
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just crash.
            e.printStackTrace();
            throw new RuntimeException(e);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }

        // logger.debug("MultiBitFrame#onCoinsReceived - wallet is currently:\n"
        // + wallet.toString());
        fireDataChanged();
    }

    public void onPendingCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        // print out transaction details
        try {
            TransactionInput input = transaction.getInputs().get(0);
            Address from = input.getFromAddress();
            BigInteger value = transaction.getValueSentToMe(wallet);
            logger.debug("Received " + Localiser.bitcoinValueToString(value, true, false) + " from " + from.toString());
            wallet.saveToFile(new File(controller.getModel().getWalletFilename()));
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just crash.
            e.printStackTrace();
            throw new RuntimeException(e);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }

        // logger.debug("MultiBitFrame#onPendingCoinsReceived - wallet is currently:\n"
        // + wallet.toString());
        fireDataChanged();
    }

    /**
     * update the UI after the model data has changed
     */
    public void fireDataChanged() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                balanceTextLabel.setText(Localiser
                        .bitcoinValueToFriendlyString3(controller.getModel().getBalance(), true, false));
                balanceTextLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.text",
                        new Object[] { Localiser.bitcoinValueToString(model.getBalance(), true, false) }));

                viewPanel.invalidate();
                viewPanel.validate();
                viewPanel.repaint();
                thisFrame.invalidate();
                thisFrame.validate();
                thisFrame.repaint();
            }
        });
    }
}