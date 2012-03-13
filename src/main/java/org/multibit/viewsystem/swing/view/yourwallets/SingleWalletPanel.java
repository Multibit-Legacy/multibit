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

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
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
import javax.swing.UIManager;
import javax.swing.border.Border;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;

import com.google.bitcoin.core.Wallet.BalanceType;

public class SingleWalletPanel extends RoundedPanel implements ActionListener, FocusListener {

    private static final long serialVersionUID = -7110340338285836548L;

    private static final String TYPICAL_DESCRIPTION = "One Quick Brown Fox";
    private static final int WALLET_WIDTH_DELTA = 20;
    private static final int WALLET_HEIGHT_DELTA = 30;

    private static final Dimension ABOVE_BASELINE_LEADING_CORNER_PADDING = new Dimension(5, 6);
    private static final Dimension BELOW_BASELINE_TRAILING_CORNER_PADDING = new Dimension(7, 8);

    private PerWalletModelData perWalletModelData;

    private static final Color BACKGROUND_COLOR_NORMAL = (Color)UIManager.get("Button.background");    
    private static final Color BACKGROUND_COLOR_DATA_HAS_CHANGED = new Color(0xff, 0xff, 0xff);

    private MultiBitLabel walletFilenameLabel;
    private MultiBitTextField walletDescriptionTextField;
    private Border walletDescriptionTextFieldBorder;

    private BlinkLabel amountLabel;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private Dimension largePreferredSize;
    private Dimension smallPreferredSize;
    
    public SingleWalletPanel(PerWalletModelData perWalletModelData, MultiBitController controller, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getLocale());
        this.perWalletModelData = perWalletModelData;
        this.controller = controller;
        this.mainFrame = mainFrame;
        setLayout(new GridBagLayout());

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        largePreferredSize = new Dimension(fontMetrics.stringWidth(TYPICAL_DESCRIPTION) + WALLET_WIDTH_DELTA,
                fontMetrics.getHeight() * 3 + WALLET_HEIGHT_DELTA);
        smallPreferredSize = new Dimension(fontMetrics.stringWidth(TYPICAL_DESCRIPTION) + WALLET_WIDTH_DELTA,
                fontMetrics.getHeight() * 2 + WALLET_HEIGHT_DELTA);
        //setMinimumSize(largePreferredSize);
        //setPreferredSize(largePreferredSize);

        setOpaque(false);
        setFocusable(true);
        setBackground(BACKGROUND_COLOR_NORMAL);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel();
        filler1.setMinimumSize(ABOVE_BASELINE_LEADING_CORNER_PADDING);
        filler1.setPreferredSize(ABOVE_BASELINE_LEADING_CORNER_PADDING);
        filler1.setMaximumSize(ABOVE_BASELINE_LEADING_CORNER_PADDING);

        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.04;
        constraints.weighty = 0.04;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(filler1, constraints);

        walletDescriptionTextField = new MultiBitTextField(perWalletModelData.getWalletDescription(), 20, controller);
        walletDescriptionTextField.setFocusable(true);
        walletDescriptionTextField.addActionListener(this);
        walletDescriptionTextField.addFocusListener(this);
        walletDescriptionTextFieldBorder = walletDescriptionTextField.getBorder();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.92;
        constraints.weighty = 4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(walletDescriptionTextField, constraints);

        amountLabel = new BlinkLabel(controller, false);
        amountLabel.setBackground(BACKGROUND_COLOR_NORMAL);
        amountLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 3));
        amountLabel.setText(controller.getLocaliser().bitcoinValueToString4(
                perWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED), true, false));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.92;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        add(amountLabel, constraints);


        walletFilenameLabel = new MultiBitLabel("", controller);
        walletFilenameLabel.setBorder(BorderFactory.createEmptyBorder(0, 7, 0, 0));

        String walletFilename = perWalletModelData.getWalletFilename();

        File walletFile = new File(walletFilename);
        if (walletFile != null) {
            String walletFilenameFull = walletFile.getName();
            String walletFilenameShort = walletFilenameFull.replaceAll("\\.wallet", "");
            walletFilenameLabel.setText(walletFilenameShort);
            walletFilenameLabel.setToolTipText(walletFilename);
        }
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.92;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(walletFilenameLabel, constraints);

        JPanel filler4 = new JPanel();
        filler4.setMinimumSize(BELOW_BASELINE_TRAILING_CORNER_PADDING);
        filler4.setPreferredSize(BELOW_BASELINE_TRAILING_CORNER_PADDING);
        filler4.setMaximumSize(BELOW_BASELINE_TRAILING_CORNER_PADDING);
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.02;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        add(filler4, constraints);

        setSelected(false);
        amountLabel.setBlinkEnabled(true);

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    }

    @Override
    public void addMouseListener(MouseListener mouseListener) {
        super.addMouseListener(mouseListener);
        walletDescriptionTextField.addMouseListener(mouseListener);
        amountLabel.addMouseListener(mouseListener);
        walletFilenameLabel.addMouseListener(mouseListener);
    }

    public void setSelected(boolean selected) {
        super.setSelected(selected);
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {

            if (selected) {
                walletFilenameLabel.setVisible(true);
                walletDescriptionTextField.setEditable(true);
                walletDescriptionTextField.setBorder(walletDescriptionTextFieldBorder);
                walletDescriptionTextField.setSelectedTextColor(ColorAndFontConstants.SELECTION_FOREGROUND_COLOR);
                walletDescriptionTextField.setSelectionColor(ColorAndFontConstants.SELECTION_BACKGROUND_COLOR);
                //setMinimumSize(largePreferredSize);
                //setPreferredSize(largePreferredSize);

            } else {
                walletFilenameLabel.setVisible(false);
                walletDescriptionTextField.setEditable(false);
                walletDescriptionTextField.setBorder(BorderFactory.createEmptyBorder(5, 7, 5, 5));
                walletDescriptionTextField.setBackground(BACKGROUND_COLOR_NORMAL);
                //setMinimumSize(smallPreferredSize);
                //setPreferredSize(smallPreferredSize);
            }
        }
    }

    public void actionPerformed(ActionEvent evt) {
        saveChanges();
        requestFocusInWindow();
    }

    public void requestWalletDescriptionFocus() {
        walletDescriptionTextField.requestFocusInWindow();
    }

    public PerWalletModelData getPerWalletModelData() {
        return perWalletModelData;
    }

    public JLabel getWalletFilenameLabel() {
        return walletFilenameLabel;
    }

    @Override
    public void focusGained(FocusEvent arg0) {
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            walletDescriptionTextField.setSelectedTextColor(ColorAndFontConstants.SELECTION_FOREGROUND_COLOR);
            walletDescriptionTextField.setSelectionColor(ColorAndFontConstants.SELECTION_BACKGROUND_COLOR);
            String text = walletDescriptionTextField.getText();
            walletDescriptionTextField.setCaretPosition(text == null ? 0 : text.length());
            perWalletModelData.setWalletDescription(text);
 //           mainFrame.setActiveWalletTooltip(new File(perWalletModelData.getWalletFilename()), text);
        }
    }

    @Override
    public void focusLost(FocusEvent arg0) {
        saveChanges();
    }

    private void saveChanges() {
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            walletDescriptionTextField.setBackground(BACKGROUND_COLOR_NORMAL);
            walletDescriptionTextField.setForeground(Color.BLACK);
            walletDescriptionTextField.select(0, 0);
            String text = walletDescriptionTextField.getText();
            perWalletModelData.setWalletDescription(text);
        }
    }

    /**
     * update any UI elements from the model (hint that data has changed)
     */
    public void updateFromModel() {
        String newAmountText = controller.getLocaliser().bitcoinValueToString4(
                perWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED), true, false);
        if (newAmountText != null && !newAmountText.equals(amountLabel.getText())) {
            amountLabel.blink(newAmountText);
        }

        if (perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            setOpaque(true);
            setBackground(BACKGROUND_COLOR_DATA_HAS_CHANGED);
            walletDescriptionTextField.setBackground(BACKGROUND_COLOR_DATA_HAS_CHANGED);
            walletDescriptionTextField.setText(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.text"));
            mainFrame.setUpdatesStoppedTooltip(walletDescriptionTextField);
            walletDescriptionTextField.setEnabled(false);
            walletDescriptionTextField.setEditable(false);
            amountLabel.setText("");
            amountLabel.setEnabled(false);
        }
    }
}
