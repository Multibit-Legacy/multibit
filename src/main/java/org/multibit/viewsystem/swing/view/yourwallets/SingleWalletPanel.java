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

    public static final int MINIMUM_WALLET_WIDTH = 220;
    public static final int MINIMUM_WALLET_HEIGHT = 90;
    
    private static final Dimension TOP_LEFT_CORNER_PADDING = new Dimension(5, 12);
    private static final Dimension BOTTOM_RIGHT_CORNER_PADDING = new Dimension(9, 12);

    private PerWalletModelData perWalletModelData;

    private static final Color BACKGROUND_COLOR = new Color(0xfB, 0xeD, 0xb3);   // kitten's ear + 8 (for better readibility)
    
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
        setBackground(BACKGROUND_COLOR);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel();
        filler1.setMinimumSize(TOP_LEFT_CORNER_PADDING);
        filler1.setPreferredSize(TOP_LEFT_CORNER_PADDING);
        filler1.setMaximumSize(TOP_LEFT_CORNER_PADDING);

        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.04;
        constraints.weighty = 0.04;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(filler1, constraints);

        walletFilenameLabel = new JLabel();
        walletFilenameLabel.setBorder(BorderFactory.createEmptyBorder(0, 7, 0, 0));
        
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
        constraints.gridy = 1;
        constraints.weightx = 0.92;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(walletFilenameLabel, constraints);

        walletDescriptionTextField = new JTextField(perWalletModelData.getWalletDescription());
        walletDescriptionTextField.setFocusable(true);
        walletDescriptionTextField.addActionListener(this);
        walletDescriptionTextField.addFocusListener(this);
        walletDescriptionTextFieldBorder = walletDescriptionTextField.getBorder();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.92;
        constraints.weighty = 4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(walletDescriptionTextField, constraints);

        amountLabel = new JLabel();
        amountLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 3));
        amountLabel.setText(Localiser.bitcoinValueToString4(perWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED),
                true, false));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.92;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.SOUTHEAST;
        add(amountLabel, constraints);

        JPanel filler4 = new JPanel();
        filler4.setMinimumSize(BOTTOM_RIGHT_CORNER_PADDING);
        filler4.setPreferredSize(BOTTOM_RIGHT_CORNER_PADDING);
        filler4.setMaximumSize(BOTTOM_RIGHT_CORNER_PADDING);
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.02;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.SOUTHEAST;
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
            walletDescriptionTextField.setBorder(BorderFactory.createEmptyBorder(5, 7, 5, 5));
            walletDescriptionTextField.setBackground(BACKGROUND_COLOR);
        }
    }

    public void actionPerformed(ActionEvent evt) {
        walletDescriptionTextField.setBackground(BACKGROUND_COLOR);
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
        walletDescriptionTextField.setCaretPosition(text == null ? 0 : text.length());
        perWalletModelData.setWalletDescription(text);
        mainFrame.setActiveWalletTooltip(new File(perWalletModelData.getWalletFilename()), text);
    }

    @Override
    public void focusLost(FocusEvent arg0) {
        walletDescriptionTextField.setBackground(BACKGROUND_COLOR);
        walletDescriptionTextField.setForeground(Color.BLACK);
        walletDescriptionTextField.select(0, 0);
        String text = walletDescriptionTextField.getText();
        perWalletModelData.setWalletDescription(text);
        mainFrame.setActiveWalletTooltip(new File(perWalletModelData.getWalletFilename()), text);
    }
}
