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
package org.multibit.viewsystem.swing.view.yourwallets;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.action.CreateNewWalletAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.view.ShowTransactionsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.GradientPanel;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;

/**
 * The your wallets view
 */
public class YourWalletsPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private JPanel walletListPanel;
    private ArrayList<SingleWalletPanel> walletPanels;

    private JLabel transactionsTitleLabel;
    private ShowTransactionsPanel transactionsPanel;

    private JScrollPane scrollPane;
    
    private boolean initialised = false;

    /**
     * Creates a new {@link YourWalletsPanel}.
     */
    public YourWalletsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));
        setBackground(Color.WHITE);

        this.controller = controller;

        walletPanels = new ArrayList<SingleWalletPanel>();

        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * show your wallets view
     */
    public void displayView() {
        if (!initialised) {
            initUI();
        }
        initialised = true;

        // get the wallets from the model
        String activeWalletFilename = controller.getModel().getActiveWalletFilename();
        PerWalletModelData activePerModelData = controller.getModel().getPerWalletModelDataByWalletFilename(activeWalletFilename);

        if (walletPanels != null) {
            for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                loopSingleWalletPanel.updateFromModel();
                if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename() != null) {
                    if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename()
                            .equals(activePerModelData.getWalletFilename())) {
                        loopSingleWalletPanel.setSelected(true);
                        transactionsPanel.displayView();
                    } else {
                        loopSingleWalletPanel.setSelected(false);
                    }
                }
            }
        }

        if (controller.getModel().getActivePerWalletModelData() != null) {
            if (controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                transactionsTitleLabel.setText(controller.getLocaliser().getString("showTransactionsAction.mayBeOutOfDate.text"));
                mainFrame.setUpdatesStoppedTooltip(transactionsTitleLabel);
            } else {
                transactionsTitleLabel.setText(controller.getLocaliser().getString("showTransactionsAction.text"));
                transactionsTitleLabel.setToolTipText(null);
            }
            transactionsTitleLabel.invalidate();
            transactionsTitleLabel.revalidate();
            transactionsTitleLabel.repaint();
        }
        
        String grabFocus = controller.getModel().getUserPreference(MultiBitModel.GRAB_FOCUS_FOR_ACTIVE_WALLET);
        if (Boolean.TRUE.toString().equals(grabFocus)) {
            grabFocusByWalletFilename(activeWalletFilename);
        }
        controller.getModel().setUserPreference(MultiBitModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "false");
        invalidate();
        revalidate();
        repaint();

    }
    
    public void grabFocusByWalletFilename(String walletFilename) {
        if (walletPanels != null) {
            for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename() != null) {
                    if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename()
                            .equals(walletFilename)) {
                        loopSingleWalletPanel.requestWalletDescriptionFocus();
                    } 
                }
            }
        }
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        // save any changes to any of the wallets (except those that other
        // processes are dealing with)
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopModelData : perWalletModelDataList) {
                if (loopModelData != null) {
                    if (loopModelData.isDirty() && !loopModelData.isFilesHaveBeenChangedByAnotherProcess()) {
                        controller.getFileHandler().savePerWalletModelData(loopModelData, false);
                    }
                }
            }
        }
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));

        this.removeAll();

        setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JPanel headerPanel = createHeaderPanel();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 0.01;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(headerPanel, constraints);

        createWalletListPanel();
        scrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setViewportView(walletListPanel);
        scrollPane.setViewportBorder(BorderFactory.createEmptyBorder());
        scrollPane.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker()));
        scrollPane.getViewport().setBackground(Color.WHITE);
        scrollPane.getViewport().setOpaque(true);
        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.49;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(scrollPane, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.03;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createAddressesHeaderPanel(), constraints);

        transactionsPanel = new ShowTransactionsPanel(mainFrame, controller);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 1;
        constraints.weighty = 0.45;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(transactionsPanel, constraints);
    }

    private JPanel createAddressesHeaderPanel() {
        JPanel addressesHeaderPanel = new GradientPanel();

        addressesHeaderPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        transactionsTitleLabel = new JLabel();
        transactionsTitleLabel.setHorizontalTextPosition(JLabel.CENTER);
        transactionsTitleLabel.setText(controller.getLocaliser().getString("showTransactionsAction.text"));
        transactionsTitleLabel.setFont(FontSizer.INSTANCE
                .getAdjustedDefaultFontWithDelta(2 * ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        addressesHeaderPanel.add(transactionsTitleLabel, constraints);

        return addressesHeaderPanel;
    }

    private JPanel createWalletListPanel() {
        WrapLayout layout = new WrapLayout(FlowLayout.LEADING, 1, 1);
        walletListPanel = new JPanel(layout);
        walletListPanel.setOpaque(true);
        walletListPanel.setBackground(Color.WHITE);
        walletListPanel.setBorder(BorderFactory.createEmptyBorder());
        walletListPanel.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        // get the wallets from the model
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (loopPerWalletModelData.getWallet() != null) {
                    JPanel outerPanel = new JPanel();
                    outerPanel.setOpaque(false);
                    outerPanel.setBorder(BorderFactory.createEmptyBorder(0, 9, 18, 9));
                    outerPanel.setLayout(new BorderLayout());
                    SingleWalletPanel loopPanel = new SingleWalletPanel(loopPerWalletModelData, controller, mainFrame);
                    outerPanel.add(loopPanel, BorderLayout.CENTER);
                    loopPanel.addMouseListener(new WalletMouseListener());

                    walletListPanel.add(outerPanel);
                    walletPanels.add(loopPanel);
                }
            }
        }

        return walletListPanel;
    }

    private JPanel createHeaderPanel() {
        JPanel headerPanel = new JPanel();
        headerPanel.setBorder(BorderFactory.createEmptyBorder(8, 2, 0, 2));
        headerPanel.setOpaque(false);
        headerPanel.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        headerPanel.setLayout(new FlowLayout(FlowLayout.LEADING));

        OpenWalletAction openWalletAction = new OpenWalletAction(controller, null, mainFrame);
        MultiBitButton openWalletButton = new MultiBitButton(openWalletAction, controller);
        headerPanel.add(openWalletButton);

        CreateNewWalletAction createNewWalletAction = new CreateNewWalletAction(controller, null, mainFrame);
        MultiBitButton createNewWalletButton = new MultiBitButton(createNewWalletAction, controller);
        headerPanel.add(createNewWalletButton);

        return headerPanel;
    }

    class WalletMouseListener extends MouseAdapter implements MouseListener {
        public WalletMouseListener() {
            super();
        }

        @Override
        public void mouseClicked(MouseEvent e) {
            SingleWalletPanel selectedWalletPanel = null;
            if (e.getSource() instanceof SingleWalletPanel) {
                selectedWalletPanel = (SingleWalletPanel) e.getSource();
            } else if (((JComponent) e.getSource()).getParent() instanceof SingleWalletPanel) {
                selectedWalletPanel = (SingleWalletPanel) (((JComponent) e.getSource()).getParent());
            }
            if (selectedWalletPanel != null) {
                if (!selectedWalletPanel.getPerWalletModelData().getWalletFilename()
                        .equals(controller.getModel().getActiveWalletFilename())) {
                    controller.getModel()
                            .setActiveWalletByFilename(selectedWalletPanel.getPerWalletModelData().getWalletFilename());

                    controller.fireWalletChanged();
                    controller.fireDataChanged();
                    controller.setActionForwardToSibling(ActionForward.FORWARD_TO_YOUR_WALLETS);
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

    public Data getData() {
        return null;
    }

    public void updateViewForNewWallet() {
        initialised = false;
        displayView();
    }

    @Override
    public void updateView() {       
        if (walletPanels != null) {
            for (SingleWalletPanel loopSingleWalletPanel : walletPanels) {
                // make sure the totals displayed are correct
                loopSingleWalletPanel.updateFromModel();
                loopSingleWalletPanel.invalidate();
                loopSingleWalletPanel.revalidate();
                loopSingleWalletPanel.repaint();
            }
        }

        // recreate the wallet data backing the ShowTransactionsPanel
        if (transactionsPanel != null) {
            WalletTableModel walletTableModel = transactionsPanel.getWalletTableModel();
            if (walletTableModel != null) {
                walletTableModel.recreateWalletData();
            }
        }
        displayView();
    }
}