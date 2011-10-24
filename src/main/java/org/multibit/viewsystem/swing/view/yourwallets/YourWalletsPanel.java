package org.multibit.viewsystem.swing.view.yourwallets;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CreateNewWalletAction;
import org.multibit.viewsystem.swing.action.OpenWalletAction;

/**
 * The your wallets view
 */
public class YourWalletsPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    private Data data;

    private JPanel walletListPanel;
    private ArrayList<SingleWalletPanel> walletPanels;

    private boolean initialised = false;

    /**
     * Creates a new {@link YourWalletsPanel}.
     */
    public YourWalletsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker())));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        this.controller = controller;

        walletPanels = new ArrayList<SingleWalletPanel>();
        data = new Data();

        initUI();
    }

    public String getDescription() {
        return controller.getLocaliser().getString("showYourWalletsAction.text");
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
                if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename() != null) {                  
                    if (loopSingleWalletPanel.getPerWalletModelData().getWalletFilename().equals(activePerModelData.getWalletFilename())) {
                        loopSingleWalletPanel.setSelected(true);
                    } else {
                        loopSingleWalletPanel.setSelected(false);
                    }
                }
            }
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

        setLayout(new BorderLayout());

        add(createHeaderPanel(), BorderLayout.NORTH);

        createWalletListPanel();
        JScrollPane scrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setViewportView(walletListPanel);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getViewport().setBackground(MultiBitFrame.BACKGROUND_COLOR);
        scrollPane.getViewport().setOpaque(true);
        add(scrollPane, BorderLayout.CENTER);  
    }

    private JPanel createWalletListPanel() {
        WrapLayout layout = new WrapLayout(FlowLayout.LEADING, 10, 10);
        walletListPanel = new JPanel(layout);
        walletListPanel.setOpaque(false);

        // get the wallets from the model
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (loopPerWalletModelData.getWallet() != null) {
                    SingleWalletPanel loopPanel = new SingleWalletPanel(loopPerWalletModelData, mainFrame);
                    loopPanel.addMouseListener(new WalletMouseListener());
                    
                    walletListPanel.add(loopPanel);
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
            // TODO Auto-generated method stub

        }
    }

    public Data getData() {
        return null;
    }
}