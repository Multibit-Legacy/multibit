package org.multibit.viewsystem.swing.view;

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
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.Border;

import org.multibit.Localiser;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;

import com.google.bitcoin.core.Wallet.BalanceType;

public class SingleWalletPanel extends JPanel implements ActionListener, FocusListener {

    private static final long serialVersionUID = -7110340338285836548L;

    private static final int MINIMUM_WALLET_WIDTH = 200;
    private static final int MINIMUM_WALLET_HEIGHT = 120;

    private PerWalletModelData perWalletModelData;

    private boolean selected;

    private static JTable COLOR_TABLE = new JTable();
    private static Color HIGHLIGHT_COLOR = COLOR_TABLE.getSelectionBackground();

    private JLabel walletFilenameLabel;
    private JTextField walletDescriptionTextField;
    private Border walletDescriptionTextFieldBorder;

    private JLabel amountLabel;

    public SingleWalletPanel(PerWalletModelData perWalletModelData) {
        this.perWalletModelData = perWalletModelData;
        setLayout(new GridBagLayout());
        setMinimumSize(new Dimension(MINIMUM_WALLET_WIDTH, MINIMUM_WALLET_HEIGHT));
        setPreferredSize(new Dimension(MINIMUM_WALLET_WIDTH, MINIMUM_WALLET_HEIGHT));
        setOpaque(true);
        setFocusable(true);
        setBackground(Color.WHITE);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel singleWalletIcon = new JLabel();
        singleWalletIcon.setIcon(MultiBitFrame.createImageIcon(MultiBitFrame.SINGLE_WALLET_ICON_FILE));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.02;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(singleWalletIcon, constraints);

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
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.48;
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
        constraints.gridx = 1;
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
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(amountLabel, constraints);

        setSelected(false);
    }

    public boolean isSelected() {
        return selected;
    }

    @Override
    public void addMouseListener(MouseListener mouseListener) {
        super.addMouseListener(mouseListener);
        walletDescriptionTextField.addMouseListener(mouseListener);
    }

    public void setSelected(boolean selected) {
        this.selected = selected;

        if (selected) {
            Border outsideBorder = BorderFactory.createMatteBorder(2, 2, 2, 2, HIGHLIGHT_COLOR);
            Border insideBorder = BorderFactory.createEmptyBorder(2, 2, 2, 2);
            setBorder(BorderFactory.createCompoundBorder(outsideBorder, insideBorder));

            walletDescriptionTextField.setEditable(true);
            walletDescriptionTextField.setBorder(walletDescriptionTextFieldBorder);
            walletDescriptionTextField.requestFocusInWindow();
        } else {
            Border outsideOutsideBorder = BorderFactory.createEmptyBorder(1, 1, 1, 1);
            Border outsideBorder = BorderFactory.createMatteBorder(1, 1, 1, 1, MultiBitFrame.DARK_BACKGROUND_COLOR.darker());
            Border insideBorder = BorderFactory.createEmptyBorder(2, 2, 2, 2);
            setBorder(BorderFactory.createCompoundBorder(outsideOutsideBorder,
                    BorderFactory.createCompoundBorder(outsideBorder, insideBorder)));

            walletDescriptionTextField.setEditable(false);
            walletDescriptionTextField.setBorder(BorderFactory.createEmptyBorder());
        }
    }

    public void actionPerformed(ActionEvent evt) {
        String text = walletDescriptionTextField.getText();
        perWalletModelData.setWalletDescription(text);
    }

    public PerWalletModelData getPerWalletModelData() {
        return perWalletModelData;
    }

    public JLabel getWalletFilenameLabel() {
        return walletFilenameLabel;
    }

    @Override
    public void focusGained(FocusEvent arg0) {
    }

    @Override
    public void focusLost(FocusEvent arg0) {
        String text = walletDescriptionTextField.getText();
        perWalletModelData.setWalletDescription(text);
    }
}
