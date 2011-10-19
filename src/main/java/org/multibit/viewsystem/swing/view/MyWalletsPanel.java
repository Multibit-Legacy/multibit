package org.multibit.viewsystem.swing.view;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.ListCellRenderer;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.OpenWalletAction;
import org.multibit.viewsystem.swing.view.ShowPreferencesPanel.LanguageData;

import com.google.bitcoin.core.Wallet.BalanceType;

/**
 * The my wallets view
 */
public class MyWalletsPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;

    private Data data;

    private ButtonGroup walletButtonGroup;
    private ArrayList<JRadioButton> walletButtons;

    /**
     * Creates a new {@link MyWalletsPanel}.
     */
    public MyWalletsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker())));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        initUI();
    }

    public String getDescription() {
        return controller.getLocaliser().getString("showMyWalletsAction.text");
    }

    /**
     * show my wallets view
     */
    public void displayView() {
        initUI();
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));

        this.removeAll();

        GridBagConstraints constraints = new GridBagConstraints();
        setLayout(new GridBagLayout());

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel1 = new JPanel();
        fillerPanel1.setOpaque(false);
        add(fillerPanel1, constraints);

        JLabel titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.LEFT);
        titleLabel.setText(getDescription());
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        titleLabel.setFont(font);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1.8;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(createWalletListPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(filler1, constraints);
    }

    private JPanel createWalletListPanel() {
        // wallet radios
        JPanel walletListPanel = new JPanel(new GridBagLayout());
        // walletListPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0,
        // 2, 0, 0),
        // BorderFactory.createTitledBorder(controller.getLocaliser().getString("showPreferencesPanel.languageTitle"))));
        walletListPanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        walletButtonGroup = new ButtonGroup();
        walletButtons = new ArrayList<JRadioButton>();

        ItemListener itemListener = new ChangeActiveWalletListener();

        // get the wallets from the model
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
        int walletCount = 0;

        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                JRadioButton loopButton = new JRadioButton();
                String buttonText = loopPerWalletModelData.getWalletFilename();
                if (loopPerWalletModelData.getWallet() != null) {
                    buttonText = buttonText
                            + "  "
                            + Localiser.bitcoinValueToString4(
                                    loopPerWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED), true, false);
                    loopButton.setText(buttonText);
                    if (loopPerWalletModelData.getWallet().equals(controller.getModel().getWallet())) {
                        loopButton.setSelected(true);
                    } else {
                        loopButton.setSelected(false);
                    }
                }
                loopButton.setOpaque(false);
                loopButton.addItemListener(itemListener);
                walletButtonGroup.add(loopButton);
                walletButtons.add(loopButton);

                constraints.fill = GridBagConstraints.NONE;
                constraints.gridx = 0;
                constraints.gridy = walletCount;
                constraints.weightx = 0.2;
                constraints.weighty = 0.3;
                constraints.gridwidth = 1;
                constraints.anchor = GridBagConstraints.LINE_START;
                walletListPanel.add(loopButton, constraints);

                walletCount++;
            }
        }

        return walletListPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        OpenWalletAction openWalletAction = new OpenWalletAction(controller, null);
        JButton openWalletButton = new JButton(openWalletAction);
        buttonPanel.add(openWalletButton);

        return buttonPanel;
    }

    class ChangeActiveWalletListener implements ItemListener {
        public ChangeActiveWalletListener() {

        }

        public void itemStateChanged(ItemEvent e) {
            JRadioButton selectedButton = (JRadioButton) e.getSource();
            int selectedIndex = walletButtons.indexOf(selectedButton);
            if (selectedIndex > -1) {
                PerWalletModelData selectedWalletModelData = controller.getModel().getPerWalletModelDataList()
                        .get(selectedIndex);
                controller.getModel().setActiveWallet(selectedWalletModelData.getWallet());

                controller.fireWalletChanged();
                controller.fireDataChanged();
                controller.setActionForwardToSibling(ActionForward.FORWARD_TO_MY_WALLETS);
            }
        }
    }

    public Data getData() {
        return null;
    }
}