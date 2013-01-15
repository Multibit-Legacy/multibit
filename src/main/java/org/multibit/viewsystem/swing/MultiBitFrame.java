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
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.math.BigInteger;
import java.util.Timer;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.joda.money.Money;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterListener;
import org.multibit.exchange.ExchangeRate;
import org.multibit.exchange.TickerTimerTask;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.StatusEnum;
import org.multibit.model.WalletBusyListener;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.core.MultiBitView;
import org.multibit.viewsystem.swing.action.CreateWalletSubmitAction;
import org.multibit.viewsystem.swing.action.DeleteWalletAction;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.action.MnemonicUtil;
import org.multibit.viewsystem.swing.action.MultiBitAction;
import org.multibit.viewsystem.swing.action.MultiBitWalletBusyAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;
import org.multibit.viewsystem.core.MultiBitViewFactory;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.ticker.TickerTablePanel;
import org.multibit.viewsystem.swing.view.walletlist.WalletListPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.EncryptionType;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletVersion;
import org.multibit.viewsystem.MultiBitViewSystem;
import org.multibit.viewsystem.core.View;

/*
 * JFrame displaying Swing version of MultiBit
 */
public class MultiBitFrame implements MultiBitViewSystem, WalletBusyListener, CurrencyConverterListener, FramePlugin {

    private static final Logger log = LoggerFactory.getLogger(MultiBitFrame.class);

    private MultiBitController controller;
    private CoreFrame coreFrame;


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

    private MultiBitViewFactory viewFactory;

    private Timer fileChangeTimer;

    private Timer tickerTimer;
    private TickerTimerTask tickerTimerTask;

    private TickerTablePanel tickerTablePanel;
    
    private MultiBitWalletBusyAction addPasswordAction;
    private MultiBitWalletBusyAction changePasswordAction;
    private MultiBitWalletBusyAction removePasswordAction;
    
    private MultiBitWalletBusyAction showImportPrivateKeysAction;
    private MultiBitWalletBusyAction showExportPrivateKeysAction;

    @SuppressWarnings("deprecation")
    public MultiBitFrame(MultiBitController controller, CoreFrame coreFrame) {
        this.controller = controller;
        this.coreFrame = coreFrame;
        
        
        viewFactory = new MultiBitViewFactory(controller, this);

        walletsView = new WalletListPanel(controller, this);
        
    }

    
    @Override
    public CoreFrame getCoreFrame() {
        return this.coreFrame;
    }

    @Override
    public void fireFinishInitUI()
    {
        controller.registerWalletBusyListener(this);

        // Initialise the file change timer.
        fileChangeTimer = new Timer();
        fileChangeTimer.schedule(new FileChangeTimerTask(controller), FileChangeTimerTask.INITIAL_DELAY, FileChangeTimerTask.DEFAULT_REPEAT_RATE);

         // Initialise the ticker.
        tickerTimer = new Timer();
        tickerTimerTask = new TickerTimerTask(controller, this);
        tickerTimer.schedule(tickerTimerTask, TickerTimerTask.INITIAL_DELAY, TickerTimerTask.DEFAULT_REPEAT_RATE);

        estimatedBalanceLabelLabel.setFocusable(false);
        estimatedBalanceBTCLabel.setFocusable(false);
        estimatedBalanceFiatLabel.setFocusable(false);

        availableBalanceLabelButton.setFocusable(false);
        availableBalanceBTCButton.setFocusable(false);
        availableBalanceFiatButton.setFocusable(false);

        CurrencyConverter.INSTANCE.addCurrencyConverterListener(this);

    }
    
    
    @Override
    public void addToHeaderPanel(JPanel headerPanel)
    {
        JPanel balancePanel = createBalancePanel();
        GridBagConstraints constraints2 = new GridBagConstraints();
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.weightx = 1.0;
        constraints2.weighty = 1.0;
        constraints2.anchor = GridBagConstraints.LINE_START;

        headerPanel.add(balancePanel, constraints2);
    }
    
    @Override
    public void addToViewTabbedPane(MultiBitTabbedPane viewTabbedPane)
    {
        // Add the send bitcoin tab.
        JPanel sendBitcoinOutlinePanel = new JPanel(new BorderLayout());
        View sendBitcoinView = viewFactory.getView(MultiBitView.SEND_BITCOIN_VIEW);
        sendBitcoinOutlinePanel.add((JPanel) sendBitcoinView, BorderLayout.CENTER);
        viewTabbedPane.addTab(sendBitcoinView.getViewTitle(), sendBitcoinView.getViewIcon(), sendBitcoinView.getViewTooltip(),
                sendBitcoinOutlinePanel);

        // Add the receive bitcoin tab.
        JPanel receiveBitcoinOutlinePanel = new JPanel(new BorderLayout());
        View receiveBitcoinView = viewFactory.getView(MultiBitView.RECEIVE_BITCOIN_VIEW);
        receiveBitcoinOutlinePanel.add((JPanel) receiveBitcoinView, BorderLayout.CENTER);
        viewTabbedPane.addTab(receiveBitcoinView.getViewTitle(), receiveBitcoinView.getViewIcon(),
                receiveBitcoinView.getViewTooltip(), receiveBitcoinOutlinePanel);

        // Add the transactions tab.
        JPanel transactionsOutlinePanel = new JPanel(new BorderLayout());
        View transactionsView = viewFactory.getView(MultiBitView.TRANSACTIONS_VIEW);
        transactionsOutlinePanel.add((JPanel) transactionsView, BorderLayout.CENTER);
        viewTabbedPane.addTab(transactionsView.getViewTitle(), transactionsView.getViewIcon(), transactionsView.getViewTooltip(),
                transactionsOutlinePanel);

    }
    
    @Override
    public void addToLeftTabbedPane(MultiBitTabbedPane viewLeftTabbedPane)
    {
        viewLeftTabbedPane.addTab("Wallets", walletsView);
    }

    private JPanel createBalancePanel() {
        JPanel headerPanel = new JPanel();

        headerPanel.setMinimumSize(new Dimension(700, CoreFrame.HEIGHT_OF_HEADER));
        headerPanel.setPreferredSize(new Dimension(700, CoreFrame.HEIGHT_OF_HEADER));
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
        
        
        FontMetrics fontMetrics = this.coreFrame.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int availableToSpendWidth = fontMetrics.stringWidth(controller.getLocaliser().getString("multiBitFrame.availableToSpend2"));
        int availableToSpendHeight = fontMetrics.getHeight();

        estimatedBalanceLabelLabel = new MultiBitLabel(controller.getLocaliser().getString("multiBitFrame.balanceLabel"), JTextField.RIGHT);
        estimatedBalanceLabelLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip"));
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
        estimatedBalanceBTCLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip"));
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
        estimatedBalanceFiatLabel.setToolTipText(controller.getLocaliser().getString("multiBitFrame.balanceLabel.tooltip"));
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

    
    
    @Override
    public void addToMenuBar(JMenuBar menuBar) {
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        {
            // Build the Trade menu.
            JMenu tradeMenu = new JMenu(this.controller.getLocaliser().getString("multiBitFrame.tradeMenuText"));
            tradeMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            tradeMenu.setComponentOrientation(this.coreFrame.getComponentOrientation());
            tradeMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.tradeMenuMnemonic"));
            
            this.addToTradeMenu(tradeMenu);
            
            menuBar.add(tradeMenu);
        }
        {
            // Build the Tools menu.
            JMenu toolsMenu = new JMenu(this.controller.getLocaliser().getString("multiBitFrame.toolsMenuText"));
            toolsMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            toolsMenu.setComponentOrientation(this.coreFrame.getComponentOrientation());
            toolsMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.toolsMenuMnemonic"));
            
            this.addToToolsMenu(toolsMenu);
            
            menuBar.add(toolsMenu);
        }
    }
    
    @Override
    public void addToFileMenu(JMenu fileMenu) {
        {
            // Create new wallet action.
            CreateWalletSubmitAction createNewWalletAction = new CreateWalletSubmitAction(controller,
                    ImageLoader.createImageIcon(ImageLoader.CREATE_NEW_ICON_FILE), this);
            JMenuItem menuItem = new JMenuItem(createNewWalletAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            fileMenu.add(menuItem);
        }
        {
            // Open wallet action.
            OpenWalletAction openWalletAction = new OpenWalletAction(controller,
                    ImageLoader.createImageIcon(ImageLoader.OPEN_WALLET_ICON_FILE), this);
            JMenuItem menuItem = new JMenuItem(openWalletAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            fileMenu.add(menuItem);
        }
        {
            DeleteWalletAction deleteWalletAction = new DeleteWalletAction(controller,
                    ImageLoader.createImageIcon(ImageLoader.DELETE_WALLET_ICON_FILE), this);
            JMenuItem menuItem = new JMenuItem(deleteWalletAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            fileMenu.add(menuItem);
        }

        fileMenu.addSeparator();

        {
            // Add password action.
            addPasswordAction = new MultiBitWalletBusyAction(controller, ImageLoader.ADD_PASSWORD_ICON_FILE, "addPasswordAction.text",
                    "addPasswordAction.tooltip", "addPasswordAction.mnemonic", MultiBitView.ADD_PASSWORD_VIEW);
            JMenuItem menuItem = new JMenuItem(addPasswordAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            fileMenu.add(menuItem);
        }
        {
            // Change password action.
            changePasswordAction = new MultiBitWalletBusyAction(controller, ImageLoader.CHANGE_PASSWORD_ICON_FILE, "changePasswordAction.text",
                    "changePasswordAction.tooltip", "changePasswordAction.mnemonic", MultiBitView.CHANGE_PASSWORD_VIEW);
            JMenuItem menuItem = new JMenuItem(changePasswordAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            fileMenu.add(menuItem);
        }
        {
            // Remove password action.
            removePasswordAction = new MultiBitWalletBusyAction(controller, ImageLoader.REMOVE_PASSWORD_ICON_FILE, "removePasswordAction.text",
                    "removePasswordAction.tooltip", "removePasswordAction.mnemonic", MultiBitView.REMOVE_PASSWORD_VIEW);
            JMenuItem menuItem = new JMenuItem(removePasswordAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            fileMenu.add(menuItem);
        }

    }
    
    @Override
    public void addToViewMenu(JMenu viewMenu) {
        {
            // ViewTransactions action.
            MultiBitAction showTransactionsAction = new MultiBitAction(controller, ImageLoader.TRANSACTIONS_ICON_FILE,
                    "showTransactionsAction.text", "showTransactionsAction.tooltip", "showTransactionsAction.mnemonic",
                    MultiBitView.TRANSACTIONS_VIEW);
            JMenuItem menuItem = new JMenuItem(showTransactionsAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            viewMenu.add(menuItem);
        }
        {
            // View Charts action.
            MultiBitAction showChartsAction = new MultiBitAction(controller, ImageLoader.CHART_LINE_ICON_FILE,
                    "chartsPanelAction.text", "chartsPanelAction.tooltip", "chartsPanelAction.mnemonic",
                    MultiBitView.CHARTS_VIEW);
            JMenuItem menuItem = new JMenuItem(showChartsAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            viewMenu.add(menuItem);
        }
        {
            // Show messages action.
            MultiBitAction showMessagesAction = new MultiBitAction(controller, ImageLoader.MESSAGES_ICON_FILE, "messagesPanel.text",
                    "messagesPanel.tooltip", "messagesPanel.mnemonic", MultiBitView.MESSAGES_VIEW);
            JMenuItem menuItem = new JMenuItem(showMessagesAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            viewMenu.add(menuItem);
        }

        {
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
            showTicker.setComponentOrientation(this.coreFrame.getComponentOrientation());
            showTicker.setIcon(ImageLoader.createImageIcon(ImageLoader.MONEY_ICON_FILE));

            if (tickerTablePanel != null) {
                tickerTablePanel.setVisible(isTickerVisible);
            }

            final MultiBitFrame finalFrame = this;

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
                            // Cancel any existing timer.
                            if (tickerTimer != null) {
                                tickerTimer.cancel();
                            }
                            // Start ticker timer.
                            tickerTimer = new Timer();
                            tickerTimer.schedule(new TickerTimerTask(controller, finalFrame), 0, TickerTimerTask.DEFAULT_REPEAT_RATE);
                        }
                    }
                }
            });

            viewMenu.add(showTicker);
        }
    }
    
    private void addToTradeMenu(JMenu tradeMenu) {
        {
            // Send bitcoin action.
            MultiBitAction sendBitcoinAction = new MultiBitAction(controller, ImageLoader.SEND_BITCOIN_ICON_FILE,
                    "sendBitcoinAction.text", "sendBitcoinAction.tooltip", "sendBitcoinAction.mnemonic", MultiBitView.SEND_BITCOIN_VIEW);
            JMenuItem menuItem = new JMenuItem(sendBitcoinAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            tradeMenu.add(menuItem);
        }
        {
            MultiBitAction receiveBitcoinAction = new MultiBitAction(controller, ImageLoader.RECEIVE_BITCOIN_ICON_FILE,
                    "receiveBitcoinAction.text", "receiveBitcoinAction.tooltip", "receiveBitcoinAction.mnemonic",
                    MultiBitView.RECEIVE_BITCOIN_VIEW);
            JMenuItem menuItem = new JMenuItem(receiveBitcoinAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
            tradeMenu.add(menuItem);
        }
    }
    
  private void addToToolsMenu(JMenu toolsMenu)
  {
      {
        // Import private keys.
        showImportPrivateKeysAction = new MultiBitWalletBusyAction(controller, ImageLoader.IMPORT_PRIVATE_KEYS_ICON_FILE,
                "showImportPrivateKeysAction.text", "showImportPrivateKeysAction.tooltip", "showImportPrivateKeysAction.mnemonic",
                MultiBitView.SHOW_IMPORT_PRIVATE_KEYS_VIEW);
        JMenuItem menuItem = new JMenuItem(showImportPrivateKeysAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
        toolsMenu.add(menuItem);
      }
      {
        // Export private keys.
        showExportPrivateKeysAction = new MultiBitWalletBusyAction(controller, ImageLoader.EXPORT_PRIVATE_KEYS_ICON_FILE,
                "showExportPrivateKeysAction.text", "showExportPrivateKeysAction.tooltip", "showExportPrivateKeysAction.mnemonic",
                MultiBitView.SHOW_EXPORT_PRIVATE_KEYS_VIEW);
        JMenuItem menuItem = new JMenuItem(showExportPrivateKeysAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
        toolsMenu.add(menuItem);
      }
      {
        toolsMenu.addSeparator();

        MultiBitAction resetTransactionsAction = new MultiBitAction(controller, ImageLoader.RESET_TRANSACTIONS_ICON_FILE,
                "resetTransactionsAction.text", "resetTransactionsAction.tooltip", "resetTransactionsAction.mnemonic",
                MultiBitView.RESET_TRANSACTIONS_VIEW);
        JMenuItem menuItem = new JMenuItem(resetTransactionsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(this.coreFrame.getComponentOrientation());
        toolsMenu.add(menuItem);
      }
  }
    


    /**
     * Recreate all views.
     */
    @Override
    public void recreateAllViews(final boolean initUI) {

        // Tell the wallets list to display.
        if (walletsView != null) {
            walletsView.initUI();
            walletsView.displayView();
        }
    }

    /**
     * Display next view on Swing event dispatch thread.
     */
    @Override
    public void displayView(int viewToDisplay) {
        //log.debug("Displaying view '" + viewToDisplay + "'");
        // Open wallet view obselete - show transactions
        if (MultiBitView.OPEN_WALLET_VIEW == viewToDisplay) {
            viewToDisplay = MultiBitView.TRANSACTIONS_VIEW;
        }
        // Create Bulk addreses obselete - show transactions
        if (MultiBitView.CREATE_BULK_ADDRESSES_VIEW == viewToDisplay) {
            viewToDisplay = MultiBitView.TRANSACTIONS_VIEW;
        }

        // Show wallets view always on display.
        if (MultiBitView.YOUR_WALLETS_VIEW == viewToDisplay) {
            walletsView.displayView();
            return;
        }

        controller.setCurrentView(viewToDisplay);

    }

    /**
     * Navigate away from view - this may be on another thread hence the
     * SwingUtilities.invokeLater.
     */
    @Override
    public void navigateAwayFromView(int viewToNavigateAwayFrom) {
        if (MultiBitView.YOUR_WALLETS_VIEW == viewToNavigateAwayFrom) {
            // Do nothing
            return;
        }

        final View viewToNavigateAwayFromFinal = viewFactory.getView(viewToNavigateAwayFrom);

        if (viewToNavigateAwayFromFinal != null) {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    viewToNavigateAwayFromFinal.navigateAwayFromView();
                }
            });
        }
    }

    @Override
    public void setOnlineStatus(StatusEnum statusEnum) {
        // TODO: Fix Status Bar for MultiBit
//        online = statusEnum;
//        if (statusBar != null) {
//            statusBar.updateOnlineStatusText(statusEnum);
//        }    
    }
    

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
        updateMenuItemsOnWalletChange();
    }
    
    public void updateMenuItemsOnWalletChange() {
        showImportPrivateKeysAction.setEnabled(!controller.getModel().getActivePerWalletModelData().isBusy());
        showExportPrivateKeysAction.setEnabled(!controller.getModel().getActivePerWalletModelData().isBusy());

        if (controller.getModel().getActiveWallet() == null) {
            // Cannot do anything password related.
            addPasswordAction.setEnabled(false);
            changePasswordAction.setEnabled(false);
            removePasswordAction.setEnabled(false);
        } else {
            if (controller.getModel().getActivePerWalletModelData().isBusy()) {
                addPasswordAction.setEnabled(false);
                changePasswordAction.setEnabled(false);
                removePasswordAction.setEnabled(false);
            } else {
                if (controller.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
                    addPasswordAction.setEnabled(false);
                    changePasswordAction.setEnabled(true);
                    removePasswordAction.setEnabled(true);
                } else {
                    if (controller.getModel().getActiveWalletWalletInfo() == null ||
                            controller.getModel().getActiveWalletWalletInfo().getWalletVersion() == WalletVersion.SERIALIZED) {
                        addPasswordAction.setEnabled(false);
                    } else {
                        addPasswordAction.setEnabled(true);
                    }
                    changePasswordAction.setEnabled(false);
                    removePasswordAction.setEnabled(false);
                }
            }
        }
    }

    @Override
    /**
     * Update due to a block being downloaded
     * This typically comes in from a Peer so is 'SwingUtilitied' to get the request on the Swing event thread
     */
    public void blockDownloaded() {
        // Update transaction screen in case status bars have changed.
        if (MultiBitView.TRANSACTIONS_VIEW == controller.getCurrentView()) {
            ShowTransactionsPanel.updateTransactions();
        }
    }

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        fireDataChanged();
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        fireDataChanged();
    }

    /**
     * One of the wallets has been reorganised due to a block chain reorganise
     */
    @Override
    public void onReorganize(Wallet wallet) {
        log.info("Wallet has been reorganised.");
        recreateAllViews(false);
    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, final Transaction transaction) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                ShowTransactionsPanel.updateTransactions();
                SendBitcoinConfirmPanel.updatePanel(transaction);
            }
        });
    }

    @Override
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        if (controller.getModel().getActiveWalletFilename() != null
                && controller.getModel().getActiveWalletFilename().equals(perWalletModelData.getWalletFilename())) {
            Message message = new Message(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + " "
                    + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2"), true);
            MessageManager.INSTANCE.addMessage(message);
        }
        fireDataChanged();
    }

    /**
     * Update the UI after the model data has changed.
     */
    @Override
    public void fireDataChanged() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                // Update the password related menu items.
                updateMenuItemsOnWalletChange();

                // Tell the wallets list to display.
                if (walletsView != null) {
                    walletsView.displayView();
                }
            }
        });
    }

    /**
     * Update the Ticker Panel after the exchange data has changed.
     */
    public void fireExchangeDataChanged() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                updateHeader();
                tickerTablePanel.update();
            }
        });
    }

    @Override
    public void updateHeader() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                updateHeaderOnSwingThread();
            }
        });
    }
        
    void updateHeaderOnSwingThread() {
        if (controller.getModel().getActivePerWalletModelData() != null
                && controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
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
            BigInteger estimatedBalance = controller.getModel().getActiveWalletEstimatedBalance();
            estimatedBalanceBTCLabel.setText(controller.getLocaliser().bitcoinValueToString(estimatedBalance, true, false));
            if (CurrencyConverter.INSTANCE.getRate() != null && CurrencyConverter.INSTANCE.isShowingFiat()) {
                Money fiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(estimatedBalance);
                estimatedBalanceFiatLabel.setText("(" + CurrencyConverter.INSTANCE.getFiatAsLocalisedString(fiat) + ")");
            }

            if (this.controller.getModel().getActiveWalletAvailableBalanceWithBoomerangChange() != null
                    && this.controller.getModel().getActiveWalletAvailableBalanceWithBoomerangChange().equals(
                            controller.getModel().getActiveWalletEstimatedBalance())) {
                availableBalanceBTCButton.setText(" ");
                availableBalanceFiatButton.setText(" ");
                availableBalanceLabelButton.setEnabled(false);
                availableBalanceBTCButton.setEnabled(false);
                availableBalanceFiatButton.setEnabled(false);
                availableBalanceLabelButton.setVisible(false);
                availableBalanceBTCButton.setVisible(false);
                availableBalanceFiatButton.setVisible(false);
            } else {
                BigInteger availableToSpend = this.controller.getModel().getActiveWalletAvailableBalanceWithBoomerangChange();
                availableBalanceBTCButton.setText(controller.getLocaliser().bitcoinValueToString(availableToSpend, true, false));
                if (CurrencyConverter.INSTANCE.getRate() != null && CurrencyConverter.INSTANCE.isShowingFiat()) {
                    Money fiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(this.controller.getModel()
                            .getActiveWalletAvailableBalanceWithBoomerangChange());
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

            String titleText = this.controller.getLocaliser().getString("multiBitFrame.title");
            if (controller.getModel().getActiveWallet() != null) {
                titleText = titleText + CoreFrame.SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletDescription()
                        + CoreFrame.SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletFilename();
            }
            this.coreFrame.setTitle(titleText);
        }
    }



    public void setUpdatesStoppedTooltip(JComponent component) {
        // Multiline tool tip text.
        String toolTipText = "<html><font face=\"sansserif\">";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.1") + "<br>";
        toolTipText = toolTipText + controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip.2") + "<br>";
        toolTipText = toolTipText + "</font></html>";
        component.setToolTipText(toolTipText);
    }

   

    public WalletListPanel getWalletsView() {
        return walletsView;
    }

    public void onDeadTransaction(Wallet wallet, Transaction deadTx, Transaction replacementTx) {
    }

    @Override
    public void onKeyAdded(ECKey key) {        
    }

    public TickerTablePanel getTickerTablePanel() {
        return tickerTablePanel;
    }

    @Override
    public void onWalletChanged(Wallet wallet) {
        // TODO 
    }

    @Override
    public void lostExchangeRate(ExchangeRate exchangeRate) {
        // TODO Auto-generated method stub    
    }

    @Override
    public void foundExchangeRate(ExchangeRate exchangeRate) {
        updateHeader();
    }

    @Override
    public void updatedExchangeRate(ExchangeRate exchangeRate) {
        updateHeader();
    }

    public Timer getTickerTimer() {
        return tickerTimer;
    }

    public void setTickerTimer(Timer tickerTimer) {
        this.tickerTimer = tickerTimer;
    }
    
    public TickerTimerTask getTickerTimerTask() {
        return tickerTimerTask;
    }

    @Override
    public void setHelpContext(String helpContextToDisplay) {
        throw new UnsupportedOperationException("Not supported yet.");
    }


}
