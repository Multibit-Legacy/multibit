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
import java.awt.SystemColor;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashSet;
import java.util.Set;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.text.DefaultEditorKit;

import org.multibit.message.MessageManager;
import org.multibit.model.StatusEnum;
import org.multibit.platform.GenericApplication;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.multibit.viewsystem.swing.action.MnemonicUtil;
import org.multibit.viewsystem.swing.action.MultiBitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.core.CoreViewFactory;
import org.multibit.viewsystem.core.View;

/*
 * JFrame displaying Swing version of MultiBit
 */
public class CoreFrame extends JFrame implements CommonFrame {

    private static final Logger log = LoggerFactory.getLogger(CoreFrame.class);

    private static final double PROPORTION_OF_VERTICAL_SCREEN_TO_FILL = 0.75D;
    private static final double PROPORTION_OF_HORIZONTAL_SCREEN_TO_FILL = 0.82D;
    
    private StatusBar statusBar;
    private StatusEnum online = StatusEnum.CONNECTING;

    private static final long serialVersionUID = 7621813615342923041L;

    private Controller controller;
    
    private Set<FramePlugin> framePlugins;

    private String helpContext;

    public String getHelpContext() {
        return helpContext;
    }

    @Override
    public void setHelpContext(String helpContext) {
        this.helpContext = helpContext;
    }

    private JSplitPane splitPane;

    private static final int TOOLTIP_DISMISSAL_DELAY = 12000; // millisecs

    /**
     * Provide the Application reference during construction
     */
    private final GenericApplication application;

    /**
     * the tabbed pane containing the views
     * 
     */
    private MultiBitTabbedPane viewTabbedPane;
    private MultiBitTabbedPane viewLeftTabbedPane;

    public Logger logger = LoggerFactory.getLogger(CoreFrame.class.getName());

    private CoreViewFactory viewFactory;

    private JPanel headerPanel;

    @SuppressWarnings("deprecation")
    public CoreFrame(Controller controller, GenericApplication application) {
        this.controller = controller;
        this.application = application;
        
        this.framePlugins = new HashSet<FramePlugin>();
        
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

        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        String titleText = this.controller.getLocaliser().getString("multiBitFrame.title");
        if (controller.getModel().getActiveWallet() != null) {
            titleText = titleText + SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletDescription()
                    + SEPARATOR + controller.getModel().getActivePerWalletModelData().getWalletFilename();
        }
        setTitle(titleText);

        ToolTipManager.sharedInstance().setDismissDelay(TOOLTIP_DISMISSAL_DELAY);

        final Controller finalController = controller;
        final CoreFrame finalCoreFrame = this;

        // TODO Examine how this fits in with the controller onQuit() event
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent arg0) {
                org.multibit.viewsystem.swing.action.ExitAction exitAction = new org.multibit.viewsystem.swing.action.ExitAction(
                        finalController, finalCoreFrame);
                exitAction.actionPerformed(null);
            }
        });

        getContentPane().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        sizeAndCenter();

        viewFactory = new CoreViewFactory(controller, this);
    }
    
    public void Load(int initialView)
    {
        initUI();

        // Initialise status bar.
        statusBar.initialise();

               
        this.updateHeader();

        this.calculateDividerPosition();
 
        MultiBitTabbedPane.setEnableUpdates(true);
        
        this.displayView(initialView);

        pack();

        setVisible(true);
    }
    
    
    public void addFramePlugin(FramePlugin framePlugin)
    {
        this.framePlugins.add(framePlugin);
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

        
        // add pugins
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.addToHeaderPanel(headerPanel);
        }


        addMenuBar(constraints, contentPane);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(headerPanel, constraints);


        // Create the tabbedpane that holds the views.
        viewTabbedPane = new MultiBitTabbedPane(controller);
        viewTabbedPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        // add pugins
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.addToViewTabbedPane(viewTabbedPane);
        }
        
        
        // Create the tabbedpane that holds the views.
        viewLeftTabbedPane = new MultiBitTabbedPane(controller);
        viewLeftTabbedPane.setBackground(ColorAndFontConstants.DARK_BACKGROUND_COLOR);
        
        // add pugins
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.addToLeftTabbedPane(viewLeftTabbedPane);
        }
        
        

        // Create a split pane with the two scroll panes in it.
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, viewLeftTabbedPane, viewTabbedPane);
        } else {
            splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, viewTabbedPane, viewLeftTabbedPane);
            splitPane.setResizeWeight(1.0);
        }

        splitPane.setOneTouchExpandable(false);
        splitPane.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, SystemColor.windowBorder));
        splitPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        
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

        statusBar = new StatusBar(controller, this);
        statusBar.updateOnlineStatusText(online);
        MessageManager.INSTANCE.addMessageListener(statusBar);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 2;
        contentPane.add(statusBar, constraints);
        
        
        // add pugins
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.fireFinishInitUI();
        }
        
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
        JMenu fileMenu = new JMenu(this.controller.getLocaliser().getString("multiBitFrame.fileMenuText"));
        fileMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        fileMenu.setComponentOrientation(componentOrientation);

        fileMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.fileMenuMnemonic"));
        menuBar.add(fileMenu);


        // Build the View menu.
        JMenu viewMenu = new JMenu(this.controller.getLocaliser().getString("multiBitFrame.viewMenuText"));
        viewMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        viewMenu.setComponentOrientation(componentOrientation);
        viewMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.viewMenuMnemonic"));
        menuBar.add(viewMenu);


        // Build the Help menu.
        JMenu helpMenu = new JMenu(this.controller.getLocaliser().getString("multiBitFrame.helpMenuText"));
        helpMenu.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        helpMenu.setComponentOrientation(componentOrientation);
        helpMenu.setMnemonic(mnemonicUtil.getMnemonic("multiBitFrame.helpMenuMnemonic"));
        menuBar.add(helpMenu);
        
        // Add Plugin Menus
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.addToMenuBar(menuBar);
        }
        
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.addToFileMenu(fileMenu);
        }
        
        
        // Exit action.
        if (application != null && !application.isMac()) {
            // Non Macs have an Exit Menu item.
            fileMenu.addSeparator();

            JMenuItem menuItem = new JMenuItem(new ExitAction(controller, this));
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            fileMenu.add(menuItem);
        }

        // Show welcome action.
        MultiBitAction showWelcomeAction = new MultiBitAction(controller, ImageLoader.WELCOME_ICON_FILE, "welcomePanel.text",
                "welcomePanel.title", "welcomePanel.mnemonic", View.WELCOME_VIEW);
        JMenuItem menuItem = new JMenuItem(showWelcomeAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        helpMenu.add(menuItem);

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
        menuItem = new JMenuItem(showHelpContentsAction);
        menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        menuItem.setComponentOrientation(componentOrientation);
        helpMenu.add(menuItem);

        if (application != null && !application.isMac()) {
            // Non Macs have a Help About menu item.
            MultiBitAction helpAboutAction = new MultiBitAction(controller, ImageLoader.MULTIBIT_SMALL_ICON_FILE,
                    "helpAboutAction.text", "helpAboutAction.tooltip", "helpAboutAction.mnemonic", View.HELP_ABOUT_VIEW);
            menuItem = new JMenuItem(helpAboutAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            helpMenu.add(menuItem);
        }
        
        // Show preferences.
        if (application != null && !application.isMac()) {
            // Non Macs have a Preferences menu item.
            MultiBitAction showPreferencesAction = new MultiBitAction(controller, ImageLoader.PREFERENCES_ICON_FILE,
                    "showPreferencesAction.text", "showPreferencesAction.tooltip", "showPreferencesAction.mnemonic",
                    View.PREFERENCES_VIEW);
            menuItem = new JMenuItem(showPreferencesAction);
            menuItem.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            menuItem.setComponentOrientation(componentOrientation);
            viewMenu.add(menuItem);
        }

        viewMenu.addSeparator();
        
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.addToViewMenu(viewMenu);
        }

        setJMenuBar(menuBar);

        return;
    }

    /**
     * Recreate all views.
     */
    @Override
    public void recreateAllViews(final boolean initUI) {
        ColorAndFontConstants.init();

        // Close down current view.
        if (controller.getCurrentView() != 0) {
            navigateAwayFromView(controller.getCurrentView());
        }

        if (initUI) {
            Container contentPane = getContentPane();
            contentPane.removeAll();
            viewTabbedPane.removeAll();
            viewFactory.initialise();
            initUI();
            try {
                applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            } catch (ClassCastException cce) {
                // Look and feel exception - ignore.
            }
        }

        statusBar.refreshOnlineStatusText();

        updateHeader();

        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.recreateAllViews(initUI);
        }

        // Tell all the tabs in the tabbedPane to update.
        if (viewTabbedPane != null) {
            for (int i = 0; i < viewTabbedPane.getTabCount(); i++) {
                JPanel tabComponent = (JPanel) viewTabbedPane.getComponentAt(i);
                Component[] components = tabComponent.getComponents();
                if (components != null && components.length > 0 && components[0] instanceof View) {
                    View loopView = ((View) components[0]);
                    loopView.displayView();
                    if (loopView.getViewId() == controller.getCurrentView()) {
                        viewTabbedPane.setSelectedIndex(i);
                    }
                }
            }
        }
    }

    /**
     * Display next view on Swing event dispatch thread.
     */
    @Override
    public void displayView(int viewToDisplay) {

        controller.setCurrentView(viewToDisplay);

        final View nextViewFinal = viewFactory.getView(viewToDisplay);

        if (nextViewFinal == null) {
            log.debug("Cannot display view " + viewToDisplay);
            return;
        }
        final CoreFrame thisFrame = this;

        SwingUtilities.invokeLater(new Runnable() {
            @SuppressWarnings("deprecation")
            @Override
            public void run() {
                String viewTitle = nextViewFinal.getViewTitle();
                boolean foundTab = false;
                if (viewTabbedPane.getTabCount() > 0) {
                    for (int i = 0; i < viewTabbedPane.getTabCount(); i++) {
                        JPanel tabComponent = (JPanel) viewTabbedPane.getComponentAt(i);
                        if (tabComponent != null) {
                            Component[] childComponents = tabComponent.getComponents();
                            String tabTitle = null;
                            if (childComponents != null && childComponents.length > 0 && childComponents[0] instanceof View) {
                                tabTitle = ((View) childComponents[0]).getViewTitle();
                            }
                            if (viewTitle != null && viewTitle.equals(tabTitle)) {
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

                nextViewFinal.displayView();

                thisFrame.setCursor(Cursor.DEFAULT_CURSOR);
            }
        });
    }

    /**
     * Navigate away from view - this may be on another thread hence the
     * SwingUtilities.invokeLater.
     */
    @Override
    public void navigateAwayFromView(int viewToNavigateAwayFrom) {

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


    /**
     * Update the UI after the model data has changed.
     */
    @Override
    public void fireDataChanged() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                // Update the password related menu items.
                //updateMenuItemsOnWalletChange();
                
                // update the header
                updateHeader();

                // TODO: Put this back into the MultiBit Class
                // Tell the wallets list to display.
//                if (walletsView != null) {
//                    walletsView.displayView();
//                }

                // Tell the current view to update itself.
                View currentViewView = viewFactory.getView(controller.getCurrentView());
                if (currentViewView != null) {
                    currentViewView.displayView();
                }

                // Tell the tab to refresh (gets round bug on replay for transactions panel)
                View tabbedPaneCurrentView = viewTabbedPane.getCurrentlyShownView();
                if (tabbedPaneCurrentView != null && System.identityHashCode(tabbedPaneCurrentView) != System.identityHashCode(currentViewView)) {
                    //log.debug("Tabbed pane is showing " + System.identityHashCode(tabbedPaneCurrentView) + ", ViewFactory has " + System.identityHashCode(currentViewView));
                    tabbedPaneCurrentView.displayView();
                }
            }
        });
        
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.fireDataChanged();
        }
        
    }


    @Override
    public void updateHeader() {
        for (FramePlugin framePlugin : this.framePlugins)
        {
            framePlugin.updateHeader();
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
        int dividerPosition = SingleWalletPanel.calculateNormalWidth(viewLeftTabbedPane) + WALLET_WIDTH_DELTA;
        if ((viewLeftTabbedPane).isVisible()) {
            dividerPosition += SCROLL_BAR_DELTA;
        }
        if (viewLeftTabbedPane != null && viewLeftTabbedPane.getPreferredSize() != null && viewLeftTabbedPane.getPreferredSize().width > dividerPosition) {
            dividerPosition = viewLeftTabbedPane.getPreferredSize().width;
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

    public MultiBitTabbedPane getLeftTabbedPane() {
        return viewLeftTabbedPane;
    }

    public JPanel getHeaderPanel() {
        return headerPanel;
    }
    
    public JSplitPane getSplitPane() {
        return splitPane;
    }
}
