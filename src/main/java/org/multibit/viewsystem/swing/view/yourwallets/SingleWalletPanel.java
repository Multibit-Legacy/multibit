package org.multibit.viewsystem.swing.view.yourwallets;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import org.multibit.Localiser;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;

import com.google.bitcoin.core.Wallet.BalanceType;

public class SingleWalletPanel extends RoundedPanel implements ActionListener, FocusListener {

    private static final long serialVersionUID = -7110340338285836548L;

    public static final int MINIMUM_WALLET_WIDTH = 240;
    public static final int MINIMUM_WALLET_HEIGHT = 100;

    private PerWalletModelData perWalletModelData;

    private JLabel walletFilenameLabel;
    private JTextField walletDescriptionTextField;
    private Border walletDescriptionTextFieldBorder;

    private JLabel amountLabel;
    private MultiBitFrame mainFrame;

    public SingleWalletPanel(PerWalletModelData perWalletModelData, MultiBitFrame mainFrame) {
        this.perWalletModelData = perWalletModelData;
        this.mainFrame = mainFrame;
        setLayout(new GridBagLayout());
        setMinimumSize(new Dimension(MINIMUM_WALLET_WIDTH, MINIMUM_WALLET_HEIGHT));
        setPreferredSize(new Dimension(MINIMUM_WALLET_WIDTH, MINIMUM_WALLET_HEIGHT));
        setOpaque(false);
        setFocusable(true);
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEmptyBorder());

        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.015;
        constraints.weighty = 0.015;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler1, constraints);

        JLabel singleWalletIcon = new JLabel();
        singleWalletIcon.setIcon(MultiBitFrame.createImageIcon(MultiBitFrame.SINGLE_WALLET_ICON_FILE));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.02;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(singleWalletIcon, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.015;
        constraints.weighty = 0.015;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler2, constraints);

        walletFilenameLabel = new JLabel();
        String walletFilename = perWalletModelData.getWalletFilename();

        File walletFile = new File(walletFilename);
        if (walletFile != null) {
            String walletFilenameFull = walletFile.getName();
            String walletFilenameShort = walletFilenameFull.replaceAll(".wallet", "");
            walletFilenameLabel.setText(walletFilenameShort);
            walletFilenameLabel.setToolTipText(walletFilename);
        }
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 2;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(walletFilenameLabel, constraints);

        walletDescriptionTextField = new JTextField(perWalletModelData.getWalletDescription());
        walletDescriptionTextField.setFocusable(true);
        walletDescriptionTextField.addActionListener(this);
        walletDescriptionTextField.addFocusListener(this);
        walletDescriptionTextFieldBorder = walletDescriptionTextField.getBorder();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(walletDescriptionTextField, constraints);

        amountLabel = new JLabel();
        amountLabel.setText(Localiser.bitcoinValueToString4(perWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED),
                true, false));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(amountLabel, constraints);

        JPanel filler4 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 1;
        constraints.weightx = 0.03;
        constraints.weighty = 0.03;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler4, constraints);

        setSelected(false);
    }

    @Override
    public void addMouseListener(MouseListener mouseListener) {
        super.addMouseListener(mouseListener);
        walletDescriptionTextField.addMouseListener(mouseListener);
    }

    public void setSelected(boolean selected) {
        super.setSelected(selected);

        if (selected) {
            walletDescriptionTextField.setEditable(true);
            walletDescriptionTextField.setBorder(walletDescriptionTextFieldBorder);
            walletDescriptionTextField.setSelectedTextColor(MultiBitFrame.SELECTION_FOREGROUND_COLOR);
            walletDescriptionTextField.setSelectionColor(MultiBitFrame.SELECTION_BACKGROUND_COLOR);
            walletDescriptionTextField.requestFocusInWindow();
        } else {
            walletDescriptionTextField.setEditable(false);
            walletDescriptionTextField.setBorder(BorderFactory.createEmptyBorder());
            walletDescriptionTextField.setBackground(Color.WHITE);
        }
    }

    public void actionPerformed(ActionEvent evt) {
        walletDescriptionTextField.setBackground(Color.WHITE);
        walletDescriptionTextField.setForeground(Color.BLACK);
        walletDescriptionTextField.select(0, 0);
        String text = walletDescriptionTextField.getText();
        perWalletModelData.setWalletDescription(text);
        mainFrame.setActiveWalletTooltip(new File(perWalletModelData.getWalletFilename()), text);
    }

    public PerWalletModelData getPerWalletModelData() {
        return perWalletModelData;
    }

    public JLabel getWalletFilenameLabel() {
        return walletFilenameLabel;
    }

    @Override
    public void focusGained(FocusEvent arg0) {
        walletDescriptionTextField.setSelectedTextColor(MultiBitFrame.SELECTION_FOREGROUND_COLOR);
        walletDescriptionTextField.setSelectionColor(MultiBitFrame.SELECTION_BACKGROUND_COLOR);
        String text = walletDescriptionTextField.getText();
        perWalletModelData.setWalletDescription(text);
        mainFrame.setActiveWalletTooltip(new File(perWalletModelData.getWalletFilename()), text);
    }

    @Override
    public void focusLost(FocusEvent arg0) {
        walletDescriptionTextField.setBackground(Color.WHITE);
        walletDescriptionTextField.setForeground(Color.BLACK);
        walletDescriptionTextField.select(0, 0);
        String text = walletDescriptionTextField.getText();
        perWalletModelData.setWalletDescription(text);
        mainFrame.setActiveWalletTooltip(new File(perWalletModelData.getWalletFilename()), text);
    }
}
