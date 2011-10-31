package org.multibit.viewsystem.swing.view.yourwallets;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.action.CreateNewWalletAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.view.AddressesPanel;
import org.multibit.viewsystem.swing.view.ShowTransactionsPanel;

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
        PerWalletModelData activePerModelData = controller.getModel().getPerWalletModelDataByWalletFilename(
                activeWalletFilename);

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
                transactionsTitleLabel.setText(controller.getLocaliser()
                        .getString("showTransactionsAction.mayBeOutOfDate.text"));
                transactionsTitleLabel.setToolTipText(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.tooltip"));            
            } else {
                transactionsTitleLabel.setText(controller.getLocaliser().getString("showTransactionsAction.text"));
                transactionsTitleLabel.setToolTipText(null);
            }
            transactionsTitleLabel.invalidate();
            transactionsTitleLabel.revalidate();
            transactionsTitleLabel.repaint();
        }

        invalidate();
        revalidate();
        repaint();

    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
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
        JScrollPane scrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setViewportView(walletListPanel);
        scrollPane.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker()));
        scrollPane.getViewport().setBackground(MultiBitFrame.BACKGROUND_COLOR);
        scrollPane.getViewport().setOpaque(true);
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
        JPanel addressesHeaderPanel = new AddressesPanel();

        addressesHeaderPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        transactionsTitleLabel = new JLabel();
        transactionsTitleLabel.setHorizontalTextPosition(JLabel.CENTER);
        transactionsTitleLabel.setText(controller.getLocaliser().getString("showTransactionsAction.text"));
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        transactionsTitleLabel.setFont(font);

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

        headerPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

        OpenWalletAction openWalletAction = new OpenWalletAction(controller, null);
        JButton openWalletButton = new JButton(openWalletAction);
        headerPanel.add(openWalletButton);

        CreateNewWalletAction createNewWalletAction = new CreateNewWalletAction(controller, null, mainFrame);
        JButton createNewWalletButton = new JButton(createNewWalletAction);
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
                    controller.getModel().setActiveWalletByFilename(
                            selectedWalletPanel.getPerWalletModelData().getWalletFilename());

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