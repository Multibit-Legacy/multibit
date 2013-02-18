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
import java.awt.EventQueue;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.SystemColor;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.math.BigInteger;
import java.util.Timer;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.text.DefaultEditorKit;

import org.multibit.Localiser;
import org.multibit.controller.Controller;
import org.multibit.controller.core.CoreController;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterListener;
import org.multibit.exchange.ExchangeRate;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.StatusEnum;
import org.multibit.platform.GenericApplication;
import org.multibit.platform.listener.GenericQuitEventListener;
import org.multibit.platform.listener.GenericQuitResponse;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.action.AbstractExitAction;
import org.multibit.viewsystem.swing.action.CreateWalletSubmitAction;
import org.multibit.viewsystem.swing.action.DeleteWalletAction;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.action.MnemonicUtil;
import org.multibit.viewsystem.swing.action.MultiBitAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.view.ViewFactory;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.dialogs.SendBitcoinConfirmDialog;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;
import org.multibit.viewsystem.swing.view.ticker.TickerTablePanel;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanel;
import org.multibit.viewsystem.swing.view.walletlist.WalletListPanel;

import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.joda.money.Money;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Sha256Hash;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;

/*
 * JFrame displaying Swing version of MultiBit
 */
public class MultiBitFrame extends JFrame implements ViewSystem, ApplicationListener, CurrencyConverterListener {

    private static final Logger log = LoggerFactory.getLogger(MultiBitFrame.class);

    private static final double PROPORTION_OF_VERTICAL_SCREEN_TO_FILL = 0.75D;
    private static final double PROPORTION_OF_HORIZONTAL_SCREEN_TO_FILL = 0.82D;

    public static final String EXAMPLE_LONG_FIELD_TEXT = "1JiM1UyTGqpLqgayxTPbWbcdVeoepmY6pK++++";
    public static final String EXAMPLE_MEDIUM_FIELD_TEXT = "Typical text 00.12345678 BTC (000.01 XYZ)";
    
    public static final int WIDTH_OF_LONG_FIELDS = 300;
    public static final int WIDTH_OF_AMOUNT_FIELD = 150;
    public static final int WALLET_WIDTH_DELTA = 25;

    public static final int SCROLL_BAR_DELTA = 20;

    public static final int HEIGHT_OF_HEADER = 70;

    public static final int WIDTH_OF_SPLIT_PANE_DIVIDER = 9;

    private StatusBar statusBar;
    private StatusEnum online = StatusEnum.CONNECTING;
    public static final String SEPARATOR = " - ";

    private static final long serialVersionUID = 7621813615342923041L;

    private final Controller controller;
    private final CoreController coreController;
    private final BitcoinController bitcoinController;
    
    private MultiBitModel model;
    private Localiser localiser;

    private String helpContext;

    public String getHelpContext() {
        return helpContext;
    }

    @Override
    public void setHelpContext(String helpContext) {
        this.helpContext = helpContext;
    }

    private MultiBitLabel estimatedBalanceLabelLabel;
    private BlinkLabel estimatedBalanceBTCLabel;
    private BlinkLabel estimatedBalanceFiatLabel;

    public BlinkLabel getEstimatedBalanceBTCLabel() {
        return estimatedBalanceBTCLabel;
    }

    public BlinkLabel getEstimatedBalanceFiatLabel() {
        return estimatedBalanceFiatLabel;
    }

    private HelpButton availableBalanceLabelButton;
    private HelpButton availableBalanceBTCButton;
    private HelpButton availableBalanceFiatButton;

    /**
     * list of wallets shown in left hand column
     */
    private WalletListPanel walletsView;

    private MultiBitFrame thisFrame;

    private JSplitPane splitPane;

    private static final int TOOLTIP_DISMISSAL_DELAY = 12000; // millisecs

    /**
     * Provide the Application reference during construction
     */
    private final GenericApplication application;
    
    private final GenericQuitResponse multiBitFrameQuitResponse = new GenericQuitResponse() {
        @Override
        public void cancelQuit() {
            log.debug("Quit Cancled");
        }

        @Override
        public void performQuit() {
            log.debug("Preformed Quit");
        }
    };
    
    final private GenericQuitEventListener quitEventListener;

    /**
     * the tabbed pane containing the views
     * 
     */
    private MultiBitTabbedPane viewTabbedPane;

    public Logger logger = LoggerFactory.getLogger(MultiBitFrame.class.getName());

    private ViewFactory viewFactory;

    private Timer fileChangeTimer;

    private Timer tickerTimer1;
    private Timer tickerTimer2;
    private TickerTimerTask tickerTimerTask1;
    private TickerTimerTask tickerTimerTask2;

    private JPanel headerPanel;

    private TickerTablePanel tickerTablePanel;

    /**
     * For events coming from Peers condense the events into regular updates.
     * This is to prevent the UI thrashing with hundreds of events per second.
     */
    public static final int FIRE_DATA_CHANGED_UPDATE_LATER_DELAY_TIME = 1000; // milliseconds
 
    /**
     * Timer used to condense multiple updates
     */
    private static Timer fireDataChangedTimer;

    private static FireDataChangedTimerTask fireDataChangedTimerTask;

    @SuppressWarnings("deprecation")
    public MultiBitFrame(CoreController coreController, BitcoinController bitcoinController, GenericApplication application, View initialView) {
        this.coreController = coreController;
        this.bitcoinController = bitcoinController;
        
        this.controller = this.coreController;
        this.quitEventListener = this.coreController;
        
        this.model = controller.getModel();
        this.localiser = controller.getLocaliser();
        this.thisFrame = this;
        this.application = application;
        
        // Remap to command v and C on a Mac
        if (application != null && application.isMac()) {
            InputMap im = (InputMap) UIManager.get("TextField.focusInputMap");
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.META_DOWN_MASK), DefaultEditorKit.copyAction);
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, KeyEvent.META_DOWN_MASK), DefaultEditorKit.pasteAction);
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, KeyEvent.META_DOWN_MASK), DefaultEditorKit.cutAction);
        }

        ColorAndFontConstants.init();

        FontSizer.INSTANCE.initialise(controller);
        UIManager.put("ToolTip.font", FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        setCursor(Cursor.WAIT_CURSOR);
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        String titleText = localiser.getString("multiBitFrame.title");
        if (controller.getModel().getActiveWallet() != null) {
            titleText = titleText + SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletDescription()
                    + SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletFilename();
        }
        setTitle(titleText);

        ToolTipManager.sharedInstance().setDismissDelay(TOOLTIP_DISMISSAL_DELAY);

        
        // TODO Examine how this fits in with the controller onQuit() event
        // Cam: I've moved this to the quit event.
        
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent arg0) {
                quitEventListener.onQuitEvent(null, multiBitFrameQuitResponse);
            }
        });

        getContentPane().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        sizeAndCenter();

        viewFactory = new ViewFactory(this.bitcoinController, this);

        initUI();

        // Initialise the tickers.
        tickerTimer1 = new Timer();
        tickerTimerTask1 = new TickerTimerTask(controller, this, true);
        tickerTimer1.schedule(tickerTimerTask1, TickerTimerTask.INITIAL_DELAY, TickerTimerTask.DEFAULT_REPEAT_RATE);
        tickerTimer2 = new Timer();
        tickerTimerTask2 = new TickerTimerTask(controller, this, false);
        tickerTimer2.schedule(tickerTimerTask2, TickerTimerTask.INITIAL_DELAY + TickerTimerTask.TASK_SEPARATION, TickerTimerTask.DEFAULT_REPEAT_RATE);

        // Initialise status bar.
        statusBar.initialise();

        // Initialise the file change timer.
        fileChangeTimer = new Timer();
        fileChangeTimer.schedule(new FileChangeTimerTask(this.bitcoinController), FileChangeTimerTask.INITIAL_DELAY, FileChangeTimerTask.DEFAULT_REPEAT_RATE);

        estimatedBalanceLabelLabel.setFocusable(false);
        estimatedBalanceBTCLabel.setFocusable(false);
        estimatedBalanceFiatLabel.setFocusable(false);

        availableBalanceLabelButton.setFocusable(false);
        availableBalanceBTCButton.setFocusable(false);
        availableBalanceFiatButton.setFocusable(false);
               
        updateHeader();

        calculateDividerPosition();
 
        MultiBitTabbedPane.setEnableUpdates(true);
        
        CurrencyConverter.INSTANCE.addCurrencyConverterListener(this);
        
        displayView(null != initialView ? initialView : View.DEFAULT_VIEW());

        pack();

        setVisible(true);
        
        fireDataChangedTimerTask = new FireDataChangedTimerTask(this);
        fireDataChangedTimer = new Timer();
        fireDataChangedTimer.scheduleAtFixedRate(fireDataChangedTimerTask, FIRE_DATA_CHANGED_UPDATE_LATER_DELAY_TIME, FIRE_DATA_CHANGED_UPDATE_LATER_DELAY_TIME);
    }

    public GenericApplication getApplication() {
        return application;
    }

    private void sizeAndCenter() {
        // Get the screen size as a java dimension.
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        int height = (int) (screenSize.height * PROPORTION_OF_VERTICAL_SCREEN_TO_FILL);
        int width = (int) (screenSize.width * PROPORTION_OF_HORIZONTAL_SCREEN_TO_FILL);

        // Set the jframe height and width.
        setPreferredSize(new Dimension(width, height));
        double startVerticalPositionRatio = (1 - PROPORTION_OF_VERTICAL_SCREEN_TO_FILL) / 2;
        double startHorizontalPositionRatio = (1 - PROPORTION_OF_HORIZONTAL_SCREEN_TO_FILL) / 2;
        setLocation((int) (width * startHorizontalPositionRatio), (int) (height * startVerticalPositionRatio));
    }

    private void initUI() {
        Container contentPane = getContentPane();
        contentPane.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        GridBagConstraints constraints2 = new GridBagConstraints();

        // Set the application icon.
        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }

        headerPanel = new JPanel();
        headerPanel.setOpaque(false);
        headerPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        headerPanel.setLayout(new GridBagLayout());
        headerPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        JPanel balancePanel = createBalancePanel();
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.weightx = 1.0;
        constraints2.weighty = 1.0;
        constraints2.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(balancePanel, constraints2);

        addMenuBar(constraints, contentPane);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(headerPanel, constraints);

        // Create the wallet list panel.
        walletsView = new WalletListPanel(this.bitcoinController, this);

        // Create the tabbedpane that holds the views.
        viewTabbedPane = new MultiBitTabbedPane(controller);
        viewTabbedPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        // Create a split pane with the two scroll panes in it.
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, (JPanel) walletsView, viewTabbedPane);
        } else {
            splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, viewTabbedPane, (JPanel) walletsView);
            splitPane.setResizeWeight(1.0);
        }

        splitPane.setOneTouchExpandable(false);
        splitPane.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, SystemColor.windowBorder));
        splitPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        BasicSplitPaneDivider divider = ( ( javax.swing.plaf.basic.BasicSplitPaneUI)splitPane.getUI()).getDivider();
        divider.setDividerSize(WIDTH_OF_SPLIT_PANE_DIVIDER);
        
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.gridheight = 1;
        constraints.weightx = 1.0;
        constraints.weighty = 1000.0;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(splitPane, constraints);

        calculateDividerPosition();
        
        // Cannot get the RTL wallets drawing nicely so switch off adjustment.
        splitPane.setEnabled(ComponentOrientation.LEFT_TO_RIGHT.equals(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())));

        // Add the send bitcoin tab.
        JPanel sendBitcoinOutlinePanel = new JPanel(new BorderLayout());
        Viewable sendBitcoinView = viewFactory.getView(View.SEND_BITCOIN_VIEW);
        sendBitcoinOutlinePanel.add((JPanel) sendBitcoinView, BorderLayout.CENTER);
        viewTabbedPane.addTab(sendBitcoinView.getViewTitle(), sendBitcoinView.getViewIcon(), sendBitcoinView.getViewTooltip(),
                sendBitcoinOutlinePanel);

        // Add the receive bitcoin tab.
        JPanel receiveBitcoinOutlinePanel = new JPanel(new BorderLayout());
        Viewable receiveBitcoinView = viewFactory.getView(View.RECEIVE_BITCOIN_VIEW);
        receiveBitcoinOutlinePanel.add((JPanel) receiveBitcoinView, BorderLayout.CENTER);
        viewTabbedPane.addTab(receiveBitcoinView.getViewTitle(), receiveBitcoinView.getViewIcon(),
                receiveBitcoinView.getViewTooltip(), receiveBitcoinOutlinePanel);

        // Add the transactions tab.
        JPanel transactionsOutlinePanel = new JPanel(new BorderLayout());
        Viewable transactionsView = viewFactory.getView(View.TRANSACTIONS_VIEW);
        transactionsOutlinePanel.add((JPanel) transactionsView, BorderLayout.CENTER);
        viewTabbedPane.addTab(transactionsView.getViewTitle(), transactionsView.getViewIcon(), transactionsView.getViewTooltip(),
                transactionsOutlinePanel);

        statusBar = new StatusBar(this.bitcoinController, this);
        statusBar.updateOnlineStatusText(online);
        MessageManager.INSTANCE.addMessageListener(statusBar);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 2;
        contentPane.add(statusBar, constraints);
    }

    private JPanel createBalancePanel() {
        JPanel headerPanel = new JPanel();

        headerPanel.setMinimumSize(new Dimension(700, HEIGHT_OF_HEADER));
        headerPanel.setPreferredSize(new Dimension(700, HEIGHT_OF_HEADER));
        headerPanel.setOpaque(false);
        headerPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        headerPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(MultiBitTitledPanel.createStent(8, 8), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        String[] keys = new String[] { "multiBitFrame.balanceLabel", "multiBitFrame.availableToSpend2"};

        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, headerPanel);

        headerPanel.add(MultiBitTitledPanel.createStent(stentWidth, 1), constraints);

        FontMetrics fontMetrics = this.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int availableToSpendWidth = fontMetrics.stringWidth(controller.getLocaliser().getString("multiBitFrame.availableToSpend2"));
        int availableToSpendHeight = fontMetrics.getHeight();

        estimatedBalanceLabelLabel = new MultiBitLabel(controller.getLocaliser().getString("multiBitFrame.balanceLabel"), JTextField.RIGHT);
        estimatedBalanceLabelLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip")));
        estimatedBalanceLabelLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(3 * ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));

        constraints.gridx = 3;
        constraints.gridy = 1;
        constraints.weightx = 0.6;
        constraints.weighty = 0.4;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        headerPanel.add(estimatedBalanceLabelLabel, constraints);
        headerPanel.add(MultiBitTitledPanel.createStent(availableToSpendWidth, availableToSpendHeight), constraints);

        constraints.gridx = 4;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(MultiBitTitledPanel.createStent(12), constraints);

        estimatedBalanceBTCLabel = new BlinkLabel(controller, true);
        estimatedBalanceBTCLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip")));
        estimatedBalanceBTCLabel.setBorder(BorderFactory.createEmptyBorder());
        //estimatedBalanceBTCLabel.setBorder(BorderFactory.createLineBorder(Color.RED));

        constraints.gridx = 5;
        constraints.gridy = 1;
        constraints.weightx = 0.6;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(estimatedBalanceBTCLabel, constraints);

        estimatedBalanceFiatLabel = new BlinkLabel(controller, true);
        estimatedBalanceFiatLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip")));
        estimatedBalanceFiatLabel.setBorder(BorderFactory.createEmptyBorder());
        //estimatedBalanceFiatLabel.setBorder(BorderFactory.createLineBorder(Color.RED));
        
        constraints.gridx = 6;
        constraints.gridy = 0;
        constraints.weightx = 0.01;
        constraints.weighty = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(MultiBitTitledPanel.createStent(12), constraints);

        constraints.gridx = 7;
        constraints.gridy = 1;
        constraints.weightx = 0.6;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(estimatedBalanceFiatLabel, constraints);

        Action availableBalanceHelpAction = new HelpContextAction(controller, null, "multiBitFrame.availableToSpend2",
                "multiBitFrame.availableToSpend.tooltip", "multiBitFrame.helpMenuText", HelpContentsPanel.HELP_AVAILABLE_TO_SPEND_URL);
        availableBalanceLabelButton = new HelpButton(availableBalanceHelpAction, controller);
        availableBalanceLabelButton.setHorizontalAlignment(JLabel.RIGHT);
        availableBalanceLabelButton.setBorder(BorderFactory.createEmptyBorder());
        
        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("multiBitFrame.availableToSpend.tooltip"), "\n",
                controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        availableBalanceLabelButton.setToolTipText(tooltipText);
        availableBalanceLabelButton.setBorder(BorderFactory.createEmptyBorder());

        constraints.gridx = 3;
        constraints.gridy = 2;
        constraints.weightx = 0.6;
        constraints.weighty = 0.4;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;

        constraints.anchor = GridBagConstraints.LINE_END;
        headerPanel.add(availableBalanceLabelButton, constraints);
        headerPanel.add(MultiBitTitledPanel.createStent(availableToSpendWidth, availableToSpendHeight), constraints);

        constraints.gridx = 5;
        constraints.gridy = 2;
        constraints.weightx = 0.6;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        availableBalanceBTCButton = new HelpButton(availableBalanceHelpAction, controller);
        availableBalanceBTCButton.setBorder(BorderFactory.createEmptyBorder());
        availableBalanceBTCButton.setToolTipText(tooltipText);

        headerPanel.add(availableBalanceBTCButton, constraints);

        constraints.gridx = 7;
        constraints.gridy = 2;
        constraints.weightx = 0.6;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;

        constraints.anchor = GridBagConstraints.LINE_START;
        availableBalanceFiatButton = new HelpButton(availableBalanceHelpAction, controller);
        availableBalanceFiatButton.setBorder(BorderFactory.createEmptyBorder());
        availableBalanceFiatButton.setToolTipText(tooltipText);

        //availableBalanceFiatButton.setBorder(BorderFactory.createLineBorder(Color.RED));

        headerPanel.add(availableBalanceFiatButton, constraints);

        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.01;
        constraints.weighty = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(MultiBitTitledPanel.createStent(8, 8), constraints);
 
        JPanel forcer1 = new JPanel();
        forcer1.setOpaque(false);
        //forcer1.setBorder(BorderFactory.createLineBorder(Color.CYAN));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 8;
        constraints.gridy = 2;
        constraints.weightx = 10000;
        constraints.weighty = 10000;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        headerPanel.add(forcer1, constraints);

        JPanel forcer2 = new JPanel();
        forcer2.setOpaque(false);
        //forcer2.setBorder(BorderFactory.createLineBorder(Color.YELLOW));

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 8;
        constraints.gridy = 1;
        constraints.weightx = 10000;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        headerPanel.add(forcer2, constraints);

        // Initially invisible.
        availableBalanceLabelButton.setVisible(false);
        availableBalanceLabelButton.setEnabled(false);
        availableBalanceBTCButton.setVisible(false);
        availableBalanceBTCButton.setEnabled(false);
        availableBalanceFiatButton.setVisible(false);
        availableBalanceFiatButton.setEnabled(false);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 8;
        constraints.gridy = 0;
        constraints.weightx = 1000;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 2;

        constraints.anchor = GridBagConstraints.LINE_START;
        headerPanel.add(filler3, constraints);

        // Add ticker panel.
        tickerTablePanel = new TickerTablePanel(this, controller);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 9;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.gridheight = 3;

        constraints.anchor = GridBagConstraints.CENTER;
        headerPanel.add(tickerTablePanel, constraints);

        // Add a little stent to keep it off the right hand edge.
        int stent = 6; // A reasonable default.
        Insets tabAreaInsets = UIManager.getInsets("TabbedPane.tabAreaInsets");
        if (tabAreaInsets != null) {
            stent = tabAreaInsets.right;
        }
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 10;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.BASELINE_TRAILING;
        headerPanel.add(MultiBitTitledPanel.createStent(stent), constraints);

        return headerPanel;
    }

    /**
     * @param constraints
     * @param contentPane
     */
    private void addMenuBar(GridBagConstraints constraints, Container contentPane) {
        ComponentOrientation componentOrientation = ComponentOrientation.getOrientation(controller.getLocaliser().getLocale());

        // Create the menu bar.
        JMenuBar menuBar = new JMenuBar();
        menuBar.setComponentOrientation(componentOrientation);

        // Create the toolBar.
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

        // Build the Help menu.
        JMenu helpMenu = new JMenu(localiser.getString("multiBitFrame.helpMenuText"));
        helpMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        helpMenu.setComponentOrientation(componentOrientation);
        helpMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.helpMenuMnemonic"));
        menuBar.add(helpMenu);

        // Create new wallet action.
        CreateWalletSubmitAction createNewWalletAction = new CreateWalletSubmitAction(this.bitcoinController,
                ImageLoader.createImageIcon(ImageLoader.CREATE_NEW_ICON_FILE), this);
        JMenuItem menuItem = new JMenuItem(createNewWalletAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        fileMenu.add(menuItem);

        // Open wallet action.
        OpenWalletAction openWalletAction = new OpenWalletAction(this.bitcoinController,
                ImageLoader.createImageIcon(ImageLoader.OPEN_WALLET_ICON_FILE), this);
        menuItem = new JMenuItem(openWalletAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        fileMenu.add(menuItem);

        DeleteWalletAction deleteWalletAction = new DeleteWalletAction(this.bitcoinController,
                ImageLoader.createImageIcon(ImageLoader.DELETE_WALLET_ICON_FILE), this);
        menuItem = new JMenuItem(deleteWalletAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        fileMenu.add(menuItem);

        // Exit action.
        if (application != null && !application.isMac()) {
            // non Macs have an Exit Menu item
            fileMenu.addSeparator();
            {
                AbstractAction exitAction = new AbstractExitAction(this.controller) {
                    
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        quitEventListener.onQuitEvent(null, multiBitFrameQuitResponse);
                    }
                };
                menuItem = new JMenuItem(exitAction);
            }
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            fileMenu.add(menuItem);
        }

        // Show welcome action.
        MultiBitAction showWelcomeAction = new MultiBitAction(controller, ImageLoader.WELCOME_ICON_FILE, "welcomePanel.text",
                "welcomePanel.title", "welcomePanel.mnemonic", View.WELCOME_VIEW);
        showWelcomeAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("welcomePanel.title")));
        menuItem = new JMenuItem(showWelcomeAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        helpMenu.add(menuItem);

        // Show help contents action.
        MultiBitAction showHelpContentsAction;
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            showHelpContentsAction = new MultiBitAction(controller, ImageLoader.HELP_CONTENTS_ICON_FILE,
                "showHelpContentsAction.text", "showHelpContentsAction.tooltip", "showHelpContentsAction.mnemonic",
                View.HELP_CONTENTS_VIEW);
        } else {
            showHelpContentsAction = new MultiBitAction(controller, ImageLoader.HELP_CONTENTS_RTL_ICON_FILE,
                    "showHelpContentsAction.text", "showHelpContentsAction.tooltip", "showHelpContentsAction.mnemonic",
                    View.HELP_CONTENTS_VIEW);
        }
        showHelpContentsAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("showHelpContentsAction.tooltip")));

        menuItem = new JMenuItem(showHelpContentsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        helpMenu.add(menuItem);

        if (application != null && !application.isMac()) {
            // Non Macs have a Help About menu item
            MultiBitAction helpAboutAction = new MultiBitAction(controller, ImageLoader.MULTIBIT_SMALL_ICON_FILE,
                    "helpAboutAction.text", "helpAboutAction.tooltip", "helpAboutAction.mnemonic", View.HELP_ABOUT_VIEW);
            helpAboutAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("helpAboutAction.tooltip")));
            menuItem = new JMenuItem(helpAboutAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            helpMenu.add(menuItem);
        }

        // ViewTransactions action.
        MultiBitAction showTransactionsAction = new MultiBitAction(controller, ImageLoader.TRANSACTIONS_ICON_FILE,
                "showTransactionsAction.text", "showTransactionsAction.tooltip", "showTransactionsAction.mnemonic",
                View.TRANSACTIONS_VIEW);
        showTransactionsAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("showTransactionsAction.tooltip")));
        menuItem = new JMenuItem(showTransactionsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        viewMenu.add(menuItem);

        // View Charts action.
        MultiBitAction showChartsAction = new MultiBitAction(controller, ImageLoader.CHART_LINE_ICON_FILE,
                "chartsPanelAction.text", "chartsPanelAction.tooltip", "chartsPanelAction.mnemonic",
                View.CHARTS_VIEW);
        showChartsAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("chartsPanelAction.tooltip")));
        menuItem = new JMenuItem(showChartsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        viewMenu.add(menuItem);

        // show messages action
        MultiBitAction showMessagesAction = new MultiBitAction(controller, ImageLoader.MESSAGES_ICON_FILE, "messagesPanel.text",
                "messagesPanel.tooltip", "messagesPanel.mnemonic", View.MESSAGES_VIEW);
        showMessagesAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("messagesPanel.tooltip")));
        menuItem = new JMenuItem(showMessagesAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        viewMenu.add(menuItem);

        // send bitcoin action
        MultiBitAction sendBitcoinAction = new MultiBitAction(controller, ImageLoader.SEND_BITCOIN_ICON_FILE,
                "sendBitcoinAction.text", "sendBitcoinAction.tooltip", "sendBitcoinAction.mnemonic", View.SEND_BITCOIN_VIEW);
        sendBitcoinAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("sendBitcoinAction.tooltip")));
        menuItem = new JMenuItem(sendBitcoinAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        tradeMenu.add(menuItem);

        MultiBitAction receiveBitcoinAction = new MultiBitAction(controller, ImageLoader.RECEIVE_BITCOIN_ICON_FILE,
                "receiveBitcoinAction.text", "receiveBitcoinAction.tooltip", "receiveBitcoinAction.mnemonic",
                View.RECEIVE_BITCOIN_VIEW);
        receiveBitcoinAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("receiveBitcoinAction.tooltip")));
        menuItem = new JMenuItem(receiveBitcoinAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        tradeMenu.add(menuItem);

        // show preferences
        if (application != null && !application.isMac()) {
            // non Macs have a Preferences menu item
            MultiBitAction showPreferencesAction = new MultiBitAction(controller, ImageLoader.PREFERENCES_ICON_FILE,
                    "showPreferencesAction.text", "showPreferencesAction.tooltip", "showPreferencesAction.mnemonic",
                    View.PREFERENCES_VIEW);
            showPreferencesAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("showPreferencesAction.tooltip")));
            menuItem = new JMenuItem(showPreferencesAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            viewMenu.add(menuItem);
        }

        viewMenu.addSeparator();

        // show ticker
        String viewTicker = controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW);
        boolean isTickerVisible = !Boolean.FALSE.toString().equals(viewTicker);

        String tickerKey;
        if (isTickerVisible) {
            tickerKey = "multiBitFrame.ticker.hide.text";
        } else {
            tickerKey = "multiBitFrame.ticker.show.text";

        }
        final JMenuItem showTicker = new JMenuItem(controller.getLocaliser().getString(tickerKey));
        showTicker.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        showTicker.setComponentOrientation(componentOrientation);
        showTicker.setIcon(ImageLoader.createImageIcon(ImageLoader.MONEY_ICON_FILE));

        if (tickerTablePanel != null) {
            tickerTablePanel.setVisible(isTickerVisible);
        }

        showTicker.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                if (tickerTablePanel != null) {
                    if (tickerTablePanel.isVisible()) {
                        tickerTablePanel.setVisible(false);
                        controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW, Boolean.FALSE.toString());
                        showTicker.setText(controller.getLocaliser().getString("multiBitFrame.ticker.show.text"));
                    } else {
                        tickerTablePanel.setVisible(true);
                        controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW, Boolean.TRUE.toString());
                        showTicker.setText(controller.getLocaliser().getString("multiBitFrame.ticker.hide.text"));
                        if (tickerTimer1 != null) {
                            tickerTimer1.cancel();
                        }                        
                        if (tickerTimer2 != null) {
                            tickerTimer2.cancel();
                        }
                        // Start ticker timer.
                        tickerTimer1 = new Timer();
                        tickerTimer1.schedule(new TickerTimerTask(controller, thisFrame, true), 0, TickerTimerTask.DEFAULT_REPEAT_RATE);
                        tickerTimer2 = new Timer();
                        tickerTimer2.schedule(new TickerTimerTask(controller, thisFrame, false), TickerTimerTask.TASK_SEPARATION, TickerTimerTask.DEFAULT_REPEAT_RATE);
                    }
                }
            }
        });

        viewMenu.add(showTicker);

        // import private keys.
        MultiBitAction showImportPrivateKeysAction = new MultiBitAction(controller, ImageLoader.IMPORT_PRIVATE_KEYS_ICON_FILE,
                "showImportPrivateKeysAction.text", "showImportPrivateKeysAction.tooltip", "showImportPrivateKeysAction.mnemonic",
                View.SHOW_IMPORT_PRIVATE_KEYS_VIEW);
        showImportPrivateKeysAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("showImportPrivateKeysAction.tooltip")));
        menuItem = new JMenuItem(showImportPrivateKeysAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        toolsMenu.add(menuItem);

        // Export private keys.
        MultiBitAction showExportPrivateKeysAction = new MultiBitAction(controller, ImageLoader.EXPORT_PRIVATE_KEYS_ICON_FILE,
                "showExportPrivateKeysAction.text", "showExportPrivateKeysAction.tooltip", "showExportPrivateKeysAction.mnemonic",
                View.SHOW_EXPORT_PRIVATE_KEYS_VIEW);
        showExportPrivateKeysAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip")));
        menuItem = new JMenuItem(showExportPrivateKeysAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        toolsMenu.add(menuItem);

        toolsMenu.addSeparator();

        MultiBitAction resetTransactionsAction = new MultiBitAction(controller, ImageLoader.RESET_TRANSACTIONS_ICON_FILE,
                "resetTransactionsAction.text", "resetTransactionsAction.tooltip", "resetTransactionsAction.mnemonic",
                View.RESET_TRANSACTIONS_VIEW);
        resetTransactionsAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("resetTransactionsAction.tooltip")));
        menuItem = new JMenuItem(resetTransactionsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        toolsMenu.add(menuItem);

        setJMenuBar(menuBar);

        return;
    }

    /**
     * Recreate all views.
     */
    @Override
    public void recreateAllViews(final boolean initUI, final View initialView) {
        // if initUI set, do an invokeLater or else it can sometimes leave the menu items in the Mac header row.
        if (EventQueue.isDispatchThread() && !initUI) {
            recreateAllViewsOnSwingThread(initUI, initialView);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    recreateAllViewsOnSwingThread(initUI, initialView);
                }
            });
        }
    }
    
    private void recreateAllViewsOnSwingThread(final boolean initUI, View initialView) {
        ColorAndFontConstants.init();

        // Close down current view.
        if (controller.getCurrentView() != View.UNKNOWN_VIEW) {
            navigateAwayFromView(controller.getCurrentView());
        }

        if (initUI) {
            thisFrame.localiser = controller.getLocaliser();
            Container contentPane = getContentPane();
            viewFactory.initialise();
            contentPane.removeAll();
            viewTabbedPane.removeAllTabs();
            initUI();
            
            if (initialView != null && !initialView.toString().equals(View.TRANSACTIONS_VIEW.toString()) && !initialView.toString().equals(View.SEND_BITCOIN_VIEW.toString())
                    && !initialView.toString().equals(View.RECEIVE_BITCOIN_VIEW)) {
                JPanel currentTabPanel = new JPanel(new BorderLayout());
                Viewable currentView = viewFactory.getView(initialView);
                currentTabPanel.add((JPanel) currentView, BorderLayout.CENTER);
                viewTabbedPane.addTab(currentView.getViewTitle(), currentView.getViewIcon(), currentView.getViewTooltip(),
                        currentTabPanel, true);
            }
 
            try {
                applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            } catch (ClassCastException cce) {
                // Look and feel exception - ignore.
            }
        }

        statusBar.refreshOnlineStatusText();

        updateHeader();

        // Tell the wallets list to display.
        if (walletsView != null) {
            walletsView.displayView(DisplayHint.COMPLETE_REDRAW);
        }

        if (tickerTablePanel != null) {
            tickerTablePanel.update();
        }

        // Tell all the tabs in the tabbedPane to update.
        if (viewTabbedPane != null) {
            for (int i = 0; i < viewTabbedPane.getTabCount(); i++) {
                JPanel tabComponent = (JPanel) viewTabbedPane.getComponentAt(i);
                Component[] components = tabComponent.getComponents();
                if (components != null && components.length > 0 && components[0] instanceof Viewable) {
                    Viewable loopView = ((Viewable) components[0]);
                    loopView.displayView(DisplayHint.COMPLETE_REDRAW);
                    if (initialView != null && loopView.getViewId().toString().equals(initialView.toString())) {
                        viewTabbedPane.setSelectedIndex(i);
                    }
                }
            }
        }
    }

    /**
     * Display next view
     */
    @Override
    public void displayView(View viewToDisplay) {

        //log.debug("Displaying view '" + viewToDisplay + "'");
        // Open wallet view obselete - show transactions
        if (View.OPEN_WALLET_VIEW == viewToDisplay) {
            viewToDisplay = View.YOUR_WALLETS_VIEW;
        }
        
        // Create Bulk addreses obselete - show transactions
        if (View.CREATE_BULK_ADDRESSES_VIEW == viewToDisplay) {
            viewToDisplay = View.TRANSACTIONS_VIEW;
        }
        // show wallets view always on display
        if (View.YOUR_WALLETS_VIEW == viewToDisplay) {
            walletsView.displayView(DisplayHint.COMPLETE_REDRAW);
            return;
        }

        controller.setCurrentView(viewToDisplay);

        final Viewable nextViewFinal = viewFactory.getView(viewToDisplay);

        if (nextViewFinal == null) {
            log.debug("Cannot display view " + viewToDisplay);
            return;
        }

        if (EventQueue.isDispatchThread()) {
            displayViewOnSwingThread(nextViewFinal);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    displayViewOnSwingThread(nextViewFinal);
                }
            });
        }
    }
    
    private void displayViewOnSwingThread(final Viewable nextViewFinal) {
        String viewName = nextViewFinal.getViewId().toString();
        boolean foundTab = false;
        if (viewTabbedPane.getTabCount() > 0) {
            //log.debug("viewTabbedPane " + System.identityHashCode(viewTabbedPane) + " initally has " + viewTabbedPane.getTabCount() + " tabs.");
            for (int i = 0; i < viewTabbedPane.getTabCount(); i++) {
                JPanel tabComponent = (JPanel) viewTabbedPane.getComponentAt(i);
                if (tabComponent != null) {
                    Component[] childComponents = tabComponent.getComponents();
                    String tabName = null;
                    if (childComponents != null && childComponents.length > 0 && childComponents[0] instanceof Viewable) {
                        tabName= ((Viewable) childComponents[0]).getViewId().toString();
                    }
                    if (viewName != null && viewName.equals(tabName)) {
                        foundTab = true;
                        ((JPanel) viewTabbedPane.getComponentAt(i)).removeAll();
                        ((JPanel) viewTabbedPane.getComponentAt(i)).add((JPanel) nextViewFinal);
                        viewTabbedPane.setSelectedIndex(i);
                    }
                }
            }
        }

        if (!foundTab && nextViewFinal instanceof JPanel) {
            JPanel tabOutlinePanel = new JPanel(new BorderLayout());
            tabOutlinePanel.add((JPanel) nextViewFinal, BorderLayout.CENTER);
            viewTabbedPane.addTab(nextViewFinal.getViewTitle(), nextViewFinal.getViewIcon(),
                    nextViewFinal.getViewTooltip(), tabOutlinePanel, true);
            viewTabbedPane.setSelectedComponent(tabOutlinePanel);
        }

        nextViewFinal.displayView(DisplayHint.COMPLETE_REDRAW);

        //log.debug("viewTabbedPane " + System.identityHashCode(viewTabbedPane) + " finally has " + viewTabbedPane.getTabCount() + " tabs.");
        thisFrame.setCursor(Cursor.getDefaultCursor());
    }
    
    /**
     * navigate away from view - this may be on another thread hence the
     * SwingUtilities.invokeLater
     */
    @Override
    public void navigateAwayFromView(View viewToNavigateAwayFrom) {
        if (View.YOUR_WALLETS_VIEW == viewToNavigateAwayFrom) {
            // Do nothing.
            return;
        }

        final Viewable viewToNavigateAwayFromFinal = viewFactory.getView(viewToNavigateAwayFrom);

        if (viewToNavigateAwayFromFinal != null) {
            if (EventQueue.isDispatchThread()) {
                viewToNavigateAwayFromFinal.navigateAwayFromView();
            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        viewToNavigateAwayFromFinal.navigateAwayFromView();
                    }
                });
            }
        }
    }

    @Override
    public void setOnlineStatus(StatusEnum statusEnum) {
        online = statusEnum;
        if (statusBar != null) {
            statusBar.updateOnlineStatusText(statusEnum);
        }    
    }

    @Override
    /**
     * Update due to a block being downloaded
     */
    public void blockDownloaded() {
        // Update transaction screen in case status bars have changed.
        if (View.TRANSACTIONS_VIEW == controller.getCurrentView()) {
            ShowTransactionsPanel.updateTransactions();
        }
    }

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        fireDataChangedUpdateLater(DisplayHint.WALLET_TRANSACTIONS_HAVE_CHANGED);
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        fireDataChangedUpdateLater(DisplayHint.WALLET_TRANSACTIONS_HAVE_CHANGED);
    }

    /**
     * One of the wallets has been reorganised due to a block chain reorganise
     */
    @Override
    public void onReorganize(Wallet wallet) {
        log.info("Wallet has been reorganised.");
        recreateAllViews(false, controller.getCurrentView());
    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
         if (controller.getCurrentView() == View.TRANSACTIONS_VIEW) {
            ShowTransactionsPanel.updateTransactions(); 
        } else if (controller.getCurrentView() == View.SEND_BITCOIN_VIEW) {
            final int numberOfPeers = (transaction == null || transaction.getConfidence() == null) ? 0 : transaction.getConfidence().getBroadcastByCount();
            final Sha256Hash transactionHash = (transaction == null) ? null : transaction.getHash();
            SendBitcoinConfirmDialog.updateDialogDueToTransactionConfidenceChange(transactionHash, numberOfPeers); 
        }
    }
    
    @Override
    public void onKeyAdded(ECKey key) {
        log.debug("Key added " + key.toString());
    }

    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        if (controller.getModel().getActiveWalletFilename() != null
                && controller.getModel().getActiveWalletFilename().equals(perWalletModelData.getWalletFilename())) {
            Message message = new Message(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + " "
                    + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2"), true);
            MessageManager.INSTANCE.addMessage(message);
        }
        fireDataChangedUpdateNow(DisplayHint.COMPLETE_REDRAW);
    }

    /**
     * Mark that the UI needs to be updated as soon as possible.
     */
    @Override
    public void fireDataChangedUpdateNow(final DisplayHint displayHint) {
        if (EventQueue.isDispatchThread()) {
            fireDataChangedOnSwingThread(displayHint);   
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    fireDataChangedOnSwingThread(displayHint); 
                }
            });
        }
    }  

    /**
     * Mark that the UI needs updating the next time the fireDataChangedTimer fires.
     */
    @Override
    public void fireDataChangedUpdateLater(DisplayHint displayHint) {
        if (fireDataChangedTimerTask != null) {
            fireDataChangedTimerTask.setFireDataChanged(true);
        }    
    }
    
    /**
     * Actually update the UI.
     * (Called back from the FireDataChangedTimerTask).
     */
    private void fireDataChangedOnSwingThread(DisplayHint displayHint) {
        updateHeader();
       
        // Tell the wallets list to display.
        if (walletsView != null) {
            walletsView.displayView(displayHint);
        }

        // Tell the current view to update itself.
        Viewable currentViewView = viewFactory.getView(controller.getCurrentView());
        if (currentViewView != null) {
            currentViewView.displayView(displayHint);
        }

        // Tell the tab to refresh (gets round bug on replay for transactions panel)
        Viewable tabbedPaneCurrentView = viewTabbedPane.getCurrentlyShownView();
        if (tabbedPaneCurrentView != null && System.identityHashCode(tabbedPaneCurrentView) != System.identityHashCode(currentViewView)) {
            log.debug("Tabbed pane is showing " + System.identityHashCode(tabbedPaneCurrentView) + ", ViewFactory has " + System.identityHashCode(currentViewView));
            tabbedPaneCurrentView.displayView(displayHint);
        }
    }


    /**
     * Update the Ticker Panel after the exchange data has changed.
     */
    public void fireExchangeDataChanged() {
        updateHeader();

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                tickerTablePanel.update();
            }
        });
    }

    public void updateHeader() {
        final BigInteger finalEstimatedBalance = controller.getModel().getActiveWalletEstimatedBalance();
        final BigInteger finalAvailableToSpend = model.getActiveWalletAvailableBalanceWithBoomerangChange();
        final boolean filesHaveBeenChangeByAnotherProcess = controller.getModel().getActivePerWalletModelData() != null && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess();

        //log.debug("updateHeader finalEstimatedBalance = " + finalEstimatedBalance + ", finalAvailableToSpend = " + finalAvailableToSpend);
        if (EventQueue.isDispatchThread()) {
            updateHeaderOnSwingThread(filesHaveBeenChangeByAnotherProcess, finalEstimatedBalance, finalAvailableToSpend);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    updateHeaderOnSwingThread(filesHaveBeenChangeByAnotherProcess, finalEstimatedBalance, finalAvailableToSpend);
                }
            });
        }
    }
        
    private void updateHeaderOnSwingThread(final boolean filesHaveBeenChangedByAnotherProcess, final BigInteger estimatedBalance, final BigInteger availableToSpend) {
        if (filesHaveBeenChangedByAnotherProcess) {
            // Files have been changed by another process - blank totals
            // and put 'Updates stopped' message.
            estimatedBalanceLabelLabel.setText(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.text"));
            estimatedBalanceBTCLabel.setText(" ");
            estimatedBalanceFiatLabel.setText(" ");
            setUpdatesStoppedTooltip(estimatedBalanceLabelLabel);
            availableBalanceLabelButton.setText(" ");
            availableBalanceBTCButton.setText(" ");
            availableBalanceFiatButton.setText(" ");
        } else {
            estimatedBalanceLabelLabel.setText(controller.getLocaliser().getString("multiBitFrame.balanceLabel"));
            estimatedBalanceBTCLabel.setText(controller.getLocaliser().bitcoinValueToString(estimatedBalance, true, false));
            if (CurrencyConverter.INSTANCE.getRate() != null && CurrencyConverter.INSTANCE.isShowingFiat()) {
                Money fiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(estimatedBalance);
                estimatedBalanceFiatLabel.setText("(" + CurrencyConverter.INSTANCE.getFiatAsLocalisedString(fiat) + ")");
            }

            if (availableToSpend != null
                    && availableToSpend.equals(estimatedBalance)) {
                availableBalanceBTCButton.setText(" ");
                availableBalanceFiatButton.setText(" ");
                availableBalanceLabelButton.setEnabled(false);
                availableBalanceBTCButton.setEnabled(false);
                availableBalanceFiatButton.setEnabled(false);
                availableBalanceLabelButton.setVisible(false);
                availableBalanceBTCButton.setVisible(false);
                availableBalanceFiatButton.setVisible(false);
            } else {
                availableBalanceBTCButton.setText(controller.getLocaliser().bitcoinValueToString(availableToSpend, true, false));
                if (CurrencyConverter.INSTANCE.getRate() != null && CurrencyConverter.INSTANCE.isShowingFiat()) {
                    Money fiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(availableToSpend);
                    availableBalanceFiatButton.setText("(" + CurrencyConverter.INSTANCE.getFiatAsLocalisedString(fiat) + ")");
                } else {
                    availableBalanceFiatButton.setText(" ");
                }
                availableBalanceLabelButton.setEnabled(true);
                availableBalanceBTCButton.setEnabled(true);
                availableBalanceFiatButton.setEnabled(true);
                availableBalanceLabelButton.setVisible(true);
                availableBalanceBTCButton.setVisible(true);
                availableBalanceFiatButton.setVisible(true);
            }

            String titleText = localiser.getString("multiBitFrame.title");
            if (controller.getModel().getActiveWallet() != null) {
                titleText = titleText + SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletDescription()
                        + SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletFilename();
            }
            setTitle(titleText);
            
            headerPanel.invalidate();
            headerPanel.validate();
            headerPanel.repaint();
        }
    }
 
    // Macify application methods

    @Override
    @Deprecated
    public void handleAbout(ApplicationEvent event) {
        controller.displayView(View.HELP_ABOUT_VIEW);
        event.setHandled(true);
    }

    @Override
    @Deprecated
    public void handleOpenApplication(ApplicationEvent event) {
        // Ok, we know our application started.
        // Not much to do about that..
    }

    @Override
    @Deprecated
    public void handleOpenFile(ApplicationEvent event) {
        // TODO i18n required.
        JOptionPane.showMessageDialog(this, "Sorry, opening of files with double click is not yet implemented.  Wallet was "
                + event.getFilename());
    }

    @Override
    @Deprecated
    public void handlePreferences(ApplicationEvent event) {
        controller.displayView(View.PREFERENCES_VIEW);
    }

    @Override
    @Deprecated
    public void handlePrintFile(ApplicationEvent event) {
        // TODO i18n required.
        JOptionPane.showMessageDialog(this, "Sorry, printing not implemented");
    }

    @Override
    @Deprecated
    public void handleQuit(ApplicationEvent event) {
        throw new UnsupportedOperationException("Deprecated.");
    }

    @Override
    public void handleReOpenApplication(ApplicationEvent event) {
        setVisible(true);
    }

    public void setUpdatesStoppedTooltip(JComponent component) {
        // Multiline tool tip text.
        String toolTipText = "<html><font face=\"sansserif\">";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + "<br>";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2") + "<br>";
        toolTipText = toolTipText + "</font></html>";
        component.setToolTipText(toolTipText);
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
    
    public void calculateDividerPosition() {
        int dividerPosition = SingleWalletPanel.calculateNormalWidth((JComponent) (walletsView)) + WALLET_WIDTH_DELTA;
        if (((WalletListPanel) walletsView).getScrollPane().getVerticalScrollBar().isVisible()) {
            dividerPosition += SCROLL_BAR_DELTA;
        }
        if (walletsView != null && walletsView.getPreferredSize() != null && walletsView.getPreferredSize().width > dividerPosition) {
            dividerPosition = walletsView.getPreferredSize().width;
        }
        
        if (ComponentOrientation.RIGHT_TO_LEFT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            int width = getWidth();
            if (width == 0) {
                width = (int) this.getPreferredSize().getWidth();
            }
            dividerPosition = width - dividerPosition; // - WalletListPanel.LEFT_BORDER - WalletListPanel.RIGHT_BORDER - 2;
        } 
        splitPane.setEnabled(true);
        splitPane.setDividerLocation(dividerPosition);
        splitPane.setEnabled(ComponentOrientation.LEFT_TO_RIGHT.equals(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())));
    }

    public WalletListPanel getWalletsView() {
        return walletsView;
    }

    public void onDeadTransaction(Wallet wallet, Transaction deadTx, Transaction replacementTx) {
    }

    public JPanel getHeaderPanel() {
        return headerPanel;
    }

    public TickerTablePanel getTickerTablePanel() {
        return tickerTablePanel;
    }

    public JSplitPane getSplitPane() {
        return splitPane;
    }

    @Override
    public void lostExchangeRate(ExchangeRate exchangeRate) {
        updateHeader();
    }

    @Override
    public void foundExchangeRate(ExchangeRate exchangeRate) {
        updateHeader();
    }

    @Override
    public void updatedExchangeRate(ExchangeRate exchangeRate) {
        updateHeader();
    }

    public Timer getTickerTimer1() {
        return tickerTimer1;
    }

    public void setTickerTimer1(Timer tickerTimer1) {
        this.tickerTimer1 = tickerTimer1;
    }
    
    public Timer getTickerTimer2() {
        return tickerTimer2;
    }

    public void setTickerTimer2(Timer tickerTimer2) {
        this.tickerTimer2 = tickerTimer2;
    }
    
    public TickerTimerTask getTickerTimerTask1() {
        return tickerTimerTask1;
    }
    public TickerTimerTask getTickerTimerTask2() {
        return tickerTimerTask2;
    }
}
