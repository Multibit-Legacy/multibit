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
import java.awt.Font;
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
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
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

public class SingleWalletPanel extends JPanel implements ActionListener, FocusListener {

    private static final int WIDTH_OF_TEXT_FIELD = 16;

    private static final long serialVersionUID = -7110340338285836548L;

    private static final Dimension ABOVE_BASELINE_LEADING_CORNER_PADDING = new Dimension(5, 6);
    private static final Dimension BELOW_BASELINE_TRAILING_CORNER_PADDING = new Dimension(7, 8);

    private PerWalletModelData perWalletModelData;

    private static final Color BACKGROUND_COLOR_NORMAL = (Color) UIManager.get("Button.background");
    private static final Color BACKGROUND_COLOR_DATA_HAS_CHANGED = new Color(0xff, 0xff, 0xff);
    private static final int COLOR_DELTA = 12;

    private static final int HEIGHT_DELTA = 20;
    private static final int DETAIL_HEIGHT_DELTA = 4;
    private static final int WIDTH_DELTA = 4;
    private static final int MIN_WIDTH_SCROLLBAR_DELTA = 20;

    private static Color inactiveBackGroundColor;
    private MultiBitTextField walletDescriptionTextField;
    private Border walletDescriptionTextFieldBorder;

    private BlinkLabel amountLabel;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private int normalHeight;
    private int normalWidth;

    private int selectedHeight;
    private int selectedWidth;

    private RoundedPanel myRoundedPanel;
    private JPanel detailPanel;
    private int detailHeight;

    private static int NUMBER_OF_ROWS_IN_SUMMARY_PANEL = 2;
    private static int NUMBER_OF_ROWS_IN_DETAIL_PANEL = 3;

    private static int DETAIL_PANEL_INDENT = 3;

    public SingleWalletPanel(PerWalletModelData perWalletModelData, MultiBitController controller, MultiBitFrame mainFrame) {

        this.perWalletModelData = perWalletModelData;
        this.controller = controller;
        this.mainFrame = mainFrame;

        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        FontMetrics fontMetrics = getFontMetrics(font);

        normalHeight = NUMBER_OF_ROWS_IN_SUMMARY_PANEL * fontMetrics.getHeight() + HEIGHT_DELTA;
        normalWidth = calculateNormalWidth(this);
        selectedHeight = (int) ((NUMBER_OF_ROWS_IN_SUMMARY_PANEL + NUMBER_OF_ROWS_IN_DETAIL_PANEL) * fontMetrics.getHeight()
                + HEIGHT_DELTA + DETAIL_HEIGHT_DELTA);
        detailHeight = (int) ((NUMBER_OF_ROWS_IN_DETAIL_PANEL) * fontMetrics.getHeight()
                + DETAIL_HEIGHT_DELTA);
        selectedWidth = normalWidth;
        
        // add contents to myRoundedPanel
        myRoundedPanel = new RoundedPanel(controller.getLocaliser().getLocale());
        myRoundedPanel.setLayout(new GridBagLayout());
        myRoundedPanel.setPreferredSize(new Dimension(normalWidth, normalHeight));
        myRoundedPanel.setMinimumSize(new Dimension(normalWidth - 2 * MIN_WIDTH_SCROLLBAR_DELTA, normalHeight));
        myRoundedPanel.setMaximumSize(new Dimension(normalWidth * 2, normalHeight));

        setOpaque(false);
        setFocusable(true);
        setBackground(BACKGROUND_COLOR_NORMAL);

        inactiveBackGroundColor = new Color(Math.max(0, BACKGROUND_COLOR_NORMAL.getRed() - COLOR_DELTA), Math.max(0,
                BACKGROUND_COLOR_NORMAL.getBlue() - COLOR_DELTA), Math.max(0, BACKGROUND_COLOR_NORMAL.getGreen() - COLOR_DELTA));

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
        myRoundedPanel.add(filler1, constraints);

        walletDescriptionTextField = new MultiBitTextField(perWalletModelData.getWalletDescription(), WIDTH_OF_TEXT_FIELD,
                controller);
        walletDescriptionTextField.setFocusable(true);
        walletDescriptionTextField.setOpaque(false);
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
        myRoundedPanel.add(walletDescriptionTextField, constraints);

        amountLabel = new BlinkLabel(controller, false);
        amountLabel.setBackground(BACKGROUND_COLOR_NORMAL);
        amountLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 3));
        amountLabel.setText(controller.getLocaliser().bitcoinValueToString4(
                perWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED), true, false));
        amountLabel.setBlinkEnabled(true);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.92;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        myRoundedPanel.add(amountLabel, constraints);

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
        myRoundedPanel.add(filler4, constraints);

        // add myRoundedPanel to myself
        setLayout(new GridBagLayout());
        GridBagConstraints constraints2 = new GridBagConstraints();
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 1;
        constraints2.weighty = 1;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        add(myRoundedPanel, constraints2);

        // add detail panel
        detailPanel = createWalletDetailPanel();
        detailPanel.setPreferredSize(new Dimension(normalWidth, detailHeight));
        detailPanel.setMinimumSize(new Dimension(normalWidth - 2 * MIN_WIDTH_SCROLLBAR_DELTA, detailHeight));
        detailPanel.setMaximumSize(new Dimension(normalWidth * 2, detailHeight));

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 1;
        constraints2.weightx = 0.92;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(detailPanel, constraints2);

        // add bottom filler
        JPanel filler = new JPanel();
        filler.setOpaque(false);
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.weightx = 1.0;
        constraints2.weighty = 100;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        add(filler, constraints2);


        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        setSelected(false);
    }

    public static int calculateNormalWidth(JComponent component) {
        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        FontMetrics fontMetrics = component.getFontMetrics(font);
        return (int) (fontMetrics.getMaxAdvance() * WIDTH_OF_TEXT_FIELD * 0.6 + WIDTH_DELTA);
    }

    @Override
    public void addMouseListener(MouseListener mouseListener) {
        super.addMouseListener(mouseListener);
        walletDescriptionTextField.addMouseListener(mouseListener);
        amountLabel.addMouseListener(mouseListener);
        detailPanel.addMouseListener(mouseListener);
        myRoundedPanel.addMouseListener(mouseListener);
    }

    public void setSelected(boolean selected) {
        myRoundedPanel.setSelected(selected);
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            if (selected) {
                detailPanel.setVisible(true);
                if (walletDescriptionTextField.isEditable()) {
                    // already editble
                } else {
                    walletDescriptionTextField.setEditable(true);
                    // may not have the caret quite right
                    // send the focus to the panel
                    requestFocusInWindow();
                }
                
                walletDescriptionTextField.setBorder(walletDescriptionTextFieldBorder);
                //walletDescriptionTextField.setSelectedTextColor(ColorAndFontConstants.SELECTION_FOREGROUND_COLOR);
                //walletDescriptionTextField.setSelectionColor(ColorAndFontConstants.SELECTION_BACKGROUND_COLOR);
                walletDescriptionTextField.setBackground(BACKGROUND_COLOR_NORMAL);
                myRoundedPanel.setBackground(BACKGROUND_COLOR_NORMAL);
                setPreferredSize(new Dimension(selectedWidth, selectedHeight));
                setMinimumSize(new Dimension(selectedWidth - 2 * MIN_WIDTH_SCROLLBAR_DELTA, selectedHeight));
                setMaximumSize(new Dimension(selectedWidth * 2, selectedHeight));
            } else {
                detailPanel.setVisible(false);
                walletDescriptionTextField.setEditable(false);
                walletDescriptionTextField.setBorder(BorderFactory.createEmptyBorder(5, 7, 5, 5));
                walletDescriptionTextField.setBackground(inactiveBackGroundColor);
                myRoundedPanel.setBackground(inactiveBackGroundColor);
                setPreferredSize(new Dimension(normalWidth, normalHeight));
                setMinimumSize(new Dimension(normalWidth - 2 * MIN_WIDTH_SCROLLBAR_DELTA, normalHeight));
                setMaximumSize(new Dimension(normalWidth * 2, normalHeight));
            }
        }
    }

    public void actionPerformed(ActionEvent evt) {
        saveChanges();
        requestFocusInWindow();
    }

    public PerWalletModelData getPerWalletModelData() {
        return perWalletModelData;
    }

    @Override
    public void focusGained(FocusEvent arg0) {
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            walletDescriptionTextField.setSelectedTextColor(ColorAndFontConstants.SELECTION_FOREGROUND_COLOR);
            walletDescriptionTextField.setSelectionColor(ColorAndFontConstants.SELECTION_BACKGROUND_COLOR);
            String text = walletDescriptionTextField.getText();
            walletDescriptionTextField.setCaretPosition(text == null ? 0 : text.length());
            perWalletModelData.setWalletDescription(text);
            
            if (arg0.getSource() instanceof JTextField) {
               // text field selection 
            } else {
                // panel selection
                requestFocusInWindow();
            }
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

            String titleText = controller.getLocaliser().getString("multiBitFrame.title");
            if (controller.getModel().getActiveWallet() != null) {
                titleText = titleText + MultiBitFrame.SEPARATOR
                        + controller.getModel().getActivePerWalletModelData().getWalletDescription() + MultiBitFrame.SEPARATOR
                        + controller.getModel().getActivePerWalletModelData().getWalletFilename();
            }
            mainFrame.setTitle(titleText);
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
            myRoundedPanel.setOpaque(true);
            myRoundedPanel.setBackground(BACKGROUND_COLOR_DATA_HAS_CHANGED);
            walletDescriptionTextField.setBackground(BACKGROUND_COLOR_DATA_HAS_CHANGED);
            walletDescriptionTextField.setText(controller.getLocaliser().getString("singleWalletPanel.dataHasChanged.text"));
            mainFrame.setUpdatesStoppedTooltip(walletDescriptionTextField);
            walletDescriptionTextField.setEnabled(false);
            walletDescriptionTextField.setEditable(false);
            amountLabel.setText("");
            amountLabel.setEnabled(false);
        }
    }

    /**
     * create the wallet details panel
     */
    private JPanel createWalletDetailPanel() {
        JPanel detailPanel = new RoundedBottomPanel(controller.getLocaliser().getLocale());
        detailPanel.setOpaque(true);
        detailPanel.setBackground(BACKGROUND_COLOR_NORMAL);
        detailPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitLabel filenameLabel = new MultiBitLabel("");
        filenameLabel.setText(controller.getLocaliser().getString("resetTransactionsPanel.walletFilenameLabel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(filenameLabel, constraints);

        JLabel filler1 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler1, constraints);

        MultiBitLabel walletFilenameLabel = new MultiBitLabel("");

        String walletFilename = perWalletModelData.getWalletFilename();

        File walletFile = new File(walletFilename);
        if (walletFile != null) {
            String walletFilenameFull = walletFile.getName();
            String walletFilenameShort = walletFilenameFull.replaceAll("\\.wallet", "");
            walletFilenameLabel.setText(walletFilenameShort);
            walletFilenameLabel.setToolTipText(walletFilename);
        }

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(walletFilenameLabel, constraints);

        MultiBitLabel sendLabelLabel = new MultiBitLabel("");
        sendLabelLabel.setText("Type");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendLabelLabel, constraints);

        MultiBitLabel sendLabelText = new MultiBitLabel("");
        sendLabelText.setText("unencrypted");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendLabelText, constraints);

        MultiBitLabel sendLabelLabel2 = new MultiBitLabel("");
        sendLabelLabel2.setText("Version");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(sendLabelLabel2, constraints);

        MultiBitLabel sendLabelText2 = new MultiBitLabel("");
        sendLabelText2.setText("1 (serialised)");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendLabelText2, constraints);

        JLabel filler2 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler2, constraints);

        JPanel outerPanel = new JPanel(new GridBagLayout());
        outerPanel.setOpaque(false);
        GridBagConstraints constraints2 = new GridBagConstraints();

        JLabel padLeft = new JLabel();
        padLeft.setOpaque(false);
        padLeft.setMinimumSize(new Dimension(DETAIL_PANEL_INDENT, DETAIL_PANEL_INDENT));
        padLeft.setPreferredSize(new Dimension(DETAIL_PANEL_INDENT, DETAIL_PANEL_INDENT));
        padLeft.setMaximumSize(new Dimension(DETAIL_PANEL_INDENT, DETAIL_PANEL_INDENT));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 0.05;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        outerPanel.add(padLeft, constraints2);

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.9;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        outerPanel.add(detailPanel, constraints2);

        JLabel padRight = new JLabel();
        padRight.setOpaque(false);
        padRight.setMinimumSize(new Dimension(DETAIL_PANEL_INDENT, DETAIL_PANEL_INDENT));
        padRight.setPreferredSize(new Dimension(DETAIL_PANEL_INDENT, DETAIL_PANEL_INDENT));
        padRight.setMaximumSize(new Dimension(DETAIL_PANEL_INDENT, DETAIL_PANEL_INDENT));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.05;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        outerPanel.add(padRight, constraints2);

        return outerPanel;
    }
}