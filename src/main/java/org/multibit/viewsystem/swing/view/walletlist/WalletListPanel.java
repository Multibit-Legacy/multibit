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
package org.multibit.viewsystem.swing.view.walletlist;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.SystemColor;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterListener;
import org.multibit.exchange.ExchangeRate;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.MultiBitTabbedPane;
import org.multibit.viewsystem.swing.action.CreateWalletSubmitAction;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;

/**
 * The wallet list view.
 */
public class WalletListPanel extends JPanel implements Viewable, WalletBusyListener, ComponentListener, CurrencyConverterListener {

    private static final long serialVersionUID = 191352298245057705L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private MultiBitFrame mainFrame;

    private MultiBitTabbedPane tabbedPane;
    private JPanel tabPanel;
    private JPanel walletListPanel;
    private ArrayList<SingleWalletPanel> walletPanels;
    private JPanel buttonPanel;

    private JScrollPane scrollPane;
    
    private static final int TOP_BORDER = 4;
    public static final int LEFT_BORDER = 3;
    public static final int RIGHT_BORDER = 3;

    public JScrollPane getScrollPane() {
        return scrollPane;
    }

    /**
     * Creates a new {@link WalletListPanel}.
     */
    public WalletListPanel(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;

        walletPanels = new ArrayList<SingleWalletPanel>();

        setOpaque(false);
        setFocusable(true);

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        initUI();
        
        this.bitcoinController.registerWalletBusyListener(this);
        
        CurrencyConverter.INSTANCE.addCurrencyConverterListener(this);
    }

    @Override
    public void displayView(DisplayHint displayHint) {
        displayView(displayHint, true);
    }
    
    private void displayView(DisplayHint displayHint, boolean blinkEnabled) {
        if (walletPanels != null) {
            synchronized(walletPanels) {
                int amountFiatLabelSize = 0;
                
                for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                    if (buttonPanel != null) {
                        int buttonPanelPreferredWidth = buttonPanel.getPreferredSize().width;
                        int loopPanelPreferredWidth = loopSingleWalletPanel.getPreferredSize().width;
                        if (buttonPanelPreferredWidth > loopPanelPreferredWidth) {
                            loopSingleWalletPanel.setPreferredSize(new Dimension(buttonPanelPreferredWidth, loopSingleWalletPanel
                                    .getPreferredSize().height));
                        }
                    }
                    // Make sure the totals displayed and encryption status are correct.
                    boolean modelBlinkEnabled = this.bitcoinController.getModel().isBlinkEnabled();
                    loopSingleWalletPanel.updateFromModel(blinkEnabled && modelBlinkEnabled, true);
                     
                    amountFiatLabelSize = Math.max(amountFiatLabelSize, loopSingleWalletPanel.getFiatLabelWidth());
                }
                
                for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                    loopSingleWalletPanel.setFiatLabelWidth(amountFiatLabelSize + 20);
                }
            }
        }
        
        // Get the wallets from the model.
        String activeWalletFilename = this.bitcoinController.getModel().getActiveWalletFilename();
        WalletData activePerModelData = this.bitcoinController.getModel().getPerWalletModelDataByWalletFilename(activeWalletFilename);

        if (activePerModelData != null && DisplayHint.COMPLETE_REDRAW == displayHint) {
            selectWalletPanelByFilename(activePerModelData.getWalletFilename());
        }
        
        invalidate();
        revalidate();
        repaint();
    }

    private void selectWalletPanelByFilename(String filename) {
        if (walletPanels != null) {
            synchronized(walletPanels) {
                for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                    loopSingleWalletPanel.updateFromModel(false, true);
                    if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename() != null) {
                        if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename().equals(filename)) {
                            loopSingleWalletPanel.setSelected(true);
                            Rectangle bounds = loopSingleWalletPanel.getParent().getBounds();
                            walletListPanel.scrollRectToVisible(bounds);
                        } else {
                            loopSingleWalletPanel.setSelected(false);
                        }
                    }
                }
            }
        }
    }

    public SingleWalletPanel findWalletPanelByFilename(String filename) {
        if (walletPanels != null) {
            synchronized(walletPanels) {
                for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                     if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename() != null) {
                        if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename().equals(filename)) {
                            return loopSingleWalletPanel;
                        }
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }
    @Override
    public void navigateAwayFromView() {
    }

    public void initUI() {
        setLayout(new BorderLayout());

        tabbedPane = new MultiBitTabbedPane(controller);
        tabPanel = new JPanel(new BorderLayout());

        buttonPanel = createButtonPanel();

        createWalletListPanel();
        scrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setViewportView(walletListPanel);
        scrollPane.setViewportBorder(BorderFactory.createEmptyBorder());
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        scrollPane.getViewport().setOpaque(true);
        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        tabPanel.add(scrollPane, BorderLayout.CENTER);

        JPanel buttonPanel = createButtonPanel();
        tabPanel.add(buttonPanel, BorderLayout.SOUTH);

        tabbedPane.addTab(controller.getLocaliser().getString("showYourWalletsAction.text"), ImageLoader.createImageIcon(ImageLoader.YOUR_WALLETS_ICON_FILE),
                tabPanel);
 
        removeAll();
        add(tabbedPane, BorderLayout.CENTER);
    }

    private JPanel createWalletListPanel() {
        walletListPanel = new JPanel();
        walletListPanel.setLayout(new GridBagLayout());
        walletListPanel.setOpaque(false);
        walletListPanel.setFocusable(true);
        walletListPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        walletListPanel.setBorder(BorderFactory.createEmptyBorder());
        walletListPanel.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        // Get the wallets from the model.
        List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();

        GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 10.0;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;

        if (perWalletModelDataList != null) {
            synchronized (walletPanels) {
                for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                    if (loopPerWalletModelData.getWallet() != null) {
                        JPanel outerPanel = new JPanel();
                        outerPanel.setOpaque(false);
                        outerPanel.setBorder(BorderFactory.createEmptyBorder(TOP_BORDER, LEFT_BORDER, 0, RIGHT_BORDER));
                        outerPanel.setLayout(new GridBagLayout());

                        GridBagConstraints constraints2 = new GridBagConstraints();
                        constraints2.fill = GridBagConstraints.BOTH;
                        constraints2.gridx = 0;
                        constraints2.gridy = 0;
                        constraints2.weightx = 1.0;
                        constraints2.weighty = 1.0;
                        constraints2.gridwidth = 1;
                        constraints2.gridheight = 1;
                        constraints2.anchor = GridBagConstraints.CENTER;

                        SingleWalletPanel loopPanel = new SingleWalletPanel(loopPerWalletModelData, this.bitcoinController, mainFrame, this);
                        loopPanel.setComponentOrientation(ComponentOrientation
                                .getOrientation(controller.getLocaliser().getLocale()));

                        int buttonPanelPreferredWidth = buttonPanel.getPreferredSize().width;
                        int loopPanelPreferredWidth = loopPanel.getPreferredSize().width;
                        if (buttonPanelPreferredWidth > loopPanelPreferredWidth) {
                            loopPanel
                                    .setPreferredSize(new Dimension(buttonPanelPreferredWidth, loopPanel.getPreferredSize().height));
                        }

                        outerPanel.add(loopPanel, constraints2);
                        loopPanel.addMouseListener(new WalletMouseListener());

                        walletListPanel.add(outerPanel, constraints);
                        walletPanels.add(loopPanel);
                        constraints.gridy = constraints.gridy + 1;
                    }
                }
            }
        }

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.weightx = 1.0;
        constraints.weighty = 10000.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        walletListPanel.add(fill1, constraints);

        return walletListPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();
        
        buttonPanel.setBorder(BorderFactory.createCompoundBorder( BorderFactory.createMatteBorder(1, 0, 0, 0, SystemColor.windowBorder),
                BorderFactory.createEmptyBorder(2, 0, 2, 0)
               ));
        buttonPanel.setOpaque(true);
        buttonPanel.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
        buttonPanel.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));       

        CreateWalletSubmitAction createNewWalletAction = new CreateWalletSubmitAction(this.bitcoinController, ImageLoader.createImageIcon(ImageLoader.CREATE_NEW_ICON_FILE), mainFrame);
        MultiBitButton createNewWalletButton = new MultiBitButton(createNewWalletAction, controller);
        createNewWalletButton.setText(controller.getLocaliser().getString("createNewWalletAction.text"));
        createNewWalletButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.33;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        buttonPanel.add(createNewWalletButton, constraints);

        return buttonPanel;
    }

    public final void selectAdjacentWallet(KeyEvent e, String keyStatus) {
        int keyCode = e.getKeyCode();
        int modifiersEx = e.getModifiersEx();

        boolean moveToNextWallet = ((keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_KP_DOWN) && modifiersEx == KeyEvent.SHIFT_DOWN_MASK);
        boolean moveToPreviousWallet = ((keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_KP_UP) && modifiersEx == KeyEvent.SHIFT_DOWN_MASK);

        if (walletPanels != null) {
            synchronized (walletPanels) {
                int currentlySelectedWalletIndex = 0;
                int nextSelectedWalletIndex = -1;
                for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                    if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename() != null) {
                        if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename()
                                .equals(this.bitcoinController.getModel().getActiveWalletFilename())) {
                            // Found the currently selected panel.
                            if (moveToNextWallet && currentlySelectedWalletIndex < walletPanels.size() - 1) {
                                nextSelectedWalletIndex = currentlySelectedWalletIndex + 1;
                                break;
                            } else {
                                if (moveToPreviousWallet && currentlySelectedWalletIndex > 0) {
                                    nextSelectedWalletIndex = currentlySelectedWalletIndex - 1;
                                    break;
                                }   
                            }
                        }
                    }
                    currentlySelectedWalletIndex++;
                }
                if (nextSelectedWalletIndex > -1) {
                    this.bitcoinController.getModel().setActiveWalletByFilename(walletPanels.get(nextSelectedWalletIndex).getPerWalletModelData().getWalletFilename());
                    selectWalletPanelByFilename(walletPanels.get(nextSelectedWalletIndex).getPerWalletModelData().getWalletFilename());
                    controller.fireDataChangedUpdateNow();
                }
            }
        }
    }
    
    class WalletMouseListener extends MouseAdapter implements MouseListener {
        public WalletMouseListener() {
            super();
        }

        @Override
        public void mouseClicked(MouseEvent e) {
            SingleWalletPanel selectedWalletPanel = null;
            JComponent source = (JComponent)e.getSource();
            
            if (source == null) {
                return;
            }
            if (source instanceof SingleWalletPanel) {
                selectedWalletPanel = (SingleWalletPanel)source;
            } 
            
            if (source.getParent() != null) {
                if (source.getParent() instanceof SingleWalletPanel) {
                    selectedWalletPanel = (SingleWalletPanel) source.getParent();
                } else {
                    if (source instanceof RoundedPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent();
                    }   
                }
                if (source.getParent().getParent() != null) {
                    if (source.getParent().getParent() instanceof SingleWalletPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent().getParent();
                    } else if (source.getParent() instanceof RoundedPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent().getParent();
                    }
                }
                if (source.getParent().getParent().getParent() != null) {
                    if (source.getParent().getParent().getParent() instanceof SingleWalletPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent().getParent().getParent();
                    } else if (source.getParent().getParent() instanceof RoundedPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent().getParent().getParent();
                    }
                }
                if (source.getParent().getParent().getParent().getParent() != null) {
                    if (source.getParent().getParent().getParent().getParent() instanceof SingleWalletPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent().getParent().getParent().getParent();
                    } else if (source.getParent().getParent().getParent() instanceof RoundedPanel) {
                        selectedWalletPanel = (SingleWalletPanel) source.getParent().getParent().getParent().getParent();
                    }
                }
            }
                        
            if (selectedWalletPanel != null) {
                boolean originallySelected = selectedWalletPanel.isSelectedInternal();

                if (!selectedWalletPanel.getPerWalletModelData().getWalletFilename()
                        .equals(bitcoinController.getModel().getActiveWalletFilename())) {
                    bitcoinController.getModel()
                            .setActiveWalletByFilename(selectedWalletPanel.getPerWalletModelData().getWalletFilename());
                    selectWalletPanelByFilename(selectedWalletPanel.getPerWalletModelData().getWalletFilename());

                    controller.fireDataChangedUpdateNow();
                }
                
                if (!originallySelected) {
                    // If this is a new wallet selection, request the focus.
                    selectedWalletPanel.requestFocusInWindow();
                } else {
                    if (source instanceof JTextField) {
                        // give the text field focus.
                        source.requestFocusInWindow();
                    } else {
                        // Select the wallet (this makes the Shift up and down subsequently work.
                        selectedWalletPanel.requestFocusInWindow();
                    }
                }
            }

        }

        @Override
        public void mouseEntered(MouseEvent e) {
        }

        @Override
        public void mouseExited(MouseEvent e) {
        }

        @Override
        public void mousePressed(MouseEvent e) {
        }

        @Override
        public void mouseReleased(MouseEvent e) {
        }
    }
    
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.YOUR_WALLETS_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showYourWalletsAction.text");
    }
    
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showYourWalletsAction.tooltip");
    }

    @Override
    public View getViewId() {
        return View.YOUR_WALLETS_VIEW;
    }

    @Override
    public void componentHidden(ComponentEvent arg0) {        
    }

    @Override
    public void componentMoved(ComponentEvent arg0) {   
    }

    @Override
    public void componentResized(ComponentEvent arg0) {
        int preferredWalletWidth = SingleWalletPanel.calculateNormalWidth(this) + LEFT_BORDER + RIGHT_BORDER + 4;
        if (scrollPane.getVerticalScrollBar().isVisible()) {
            preferredWalletWidth -= MultiBitFrame.SCROLL_BAR_DELTA;
        }         

        walletListPanel.setPreferredSize(new Dimension(preferredWalletWidth, walletListPanel.getPreferredSize().height));
        mainFrame.calculateDividerPosition();
    }

    @Override
    public void componentShown(ComponentEvent arg0) {
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
        for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
            // Update the visibility of the hourglass if wallet is busy.
            loopSingleWalletPanel.setBusyIconStatus(loopSingleWalletPanel.getPerWalletModelData().isBusy());
            loopSingleWalletPanel.invalidate();
            loopSingleWalletPanel.revalidate();
            loopSingleWalletPanel.repaint();
        }
    }

    @Override
    public void lostExchangeRate(ExchangeRate exchangeRate) {      
    }

    @Override
    public void foundExchangeRate(ExchangeRate exchangeRate) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                displayView(DisplayHint.COMPLETE_REDRAW, false);
            }
        });       
    }

    @Override
    public void updatedExchangeRate(ExchangeRate exchangeRate) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                displayView(DisplayHint.COMPLETE_REDRAW, false);
            }
        });     
    }
}