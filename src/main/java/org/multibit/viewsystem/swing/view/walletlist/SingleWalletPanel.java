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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;

import com.google.bitcoin.core.Wallet.BalanceType;

public class SingleWalletPanel extends JPanel implements ActionListener, FocusListener {

    private static final int WIDTH_OF_TEXT_FIELD = 15;

    private static final long serialVersionUID = -7110340338285836548L;

    private static final Dimension ABOVE_BASELINE_LEADING_CORNER_PADDING = new Dimension(5, 6);
    private static final Dimension BELOW_BASELINE_TRAILING_CORNER_PADDING = new Dimension(7, 8);

    private PerWalletModelData perWalletModelData;

    private static final Color BACKGROUND_COLOR_DATA_HAS_CHANGED = new Color(0xff, 0xff, 0xff);
    private static final int COLOR_DELTA = 12;

    private static final int HEIGHT_DELTA = 20;
    private static final int DETAIL_HEIGHT_DELTA = 4;
    private static final int WIDTH_DELTA = 4;
    private static final int MIN_WIDTH_SCROLLBAR_DELTA = 20;

    private static Color inactiveBackGroundColor;
    private Border underlineBorder;
    private MultiBitTextField walletDescriptionTextField;
    private Border walletDescriptionTextFieldBorder;

    private BlinkLabel amountLabel;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private int normalHeight;
    private int normalWidth;

    private int expandedHeight;

    private RoundedPanel myRoundedPanel;
    private JPanel detailPanel;
    private RoundedBottomPanel roundedBottomPanel;
    private int detailHeight;

    private static int NUMBER_OF_ROWS_IN_SUMMARY_PANEL = 2;
    private static int NUMBER_OF_ROWS_IN_DETAIL_PANEL = 3;

    private static int DETAIL_PANEL_OUTER_INDENT = 3;
    private static int DETAIL_PANEL_INNER_INDENT = 1;

    private boolean expanded = false;

    private boolean selected = false;

    private static final int TWISTY_LEFT_BORDER = 8;
    private static final int TWISTY_TOP_BORDER = 3;

    private JLabel twistyLabel;
    private Icon twistyRightIcon;
    private Icon twistyDownIcon;
    
    private MultiBitLabel filenameLabel;
    private JLabel filenameSeparator;
    private MultiBitLabel walletFilenameLabel;

    private final SingleWalletPanel thisPanel;

    public SingleWalletPanel(PerWalletModelData perWalletModelData, MultiBitController controller, MultiBitFrame mainFrame) {
        this.perWalletModelData = perWalletModelData;
        this.controller = controller;
        this.mainFrame = mainFrame;
        thisPanel = this;

        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        FontMetrics fontMetrics = getFontMetrics(font);

        // by default not expanded, not selected
        expanded = false;
        selected = false;

        twistyRightIcon = ImageLoader.createImageIcon(ImageLoader.TWISTY_RIGHT_ICON_FILE);
        twistyDownIcon = ImageLoader.createImageIcon(ImageLoader.TWISTY_DOWN_ICON_FILE);

        normalHeight = NUMBER_OF_ROWS_IN_SUMMARY_PANEL * fontMetrics.getHeight() + HEIGHT_DELTA;
        normalWidth = calculateNormalWidth(this);
        expandedHeight = (int) ((NUMBER_OF_ROWS_IN_SUMMARY_PANEL + NUMBER_OF_ROWS_IN_DETAIL_PANEL) * fontMetrics.getHeight()
                + HEIGHT_DELTA + DETAIL_HEIGHT_DELTA);
        detailHeight = (int) ((NUMBER_OF_ROWS_IN_DETAIL_PANEL) * fontMetrics.getHeight() + DETAIL_HEIGHT_DELTA);

        // add contents to myRoundedPanel
        myRoundedPanel = new RoundedPanel(controller.getLocaliser().getLocale());
        myRoundedPanel.setLayout(new GridBagLayout());
        myRoundedPanel.setOpaque(false);
        myRoundedPanel.setPreferredSize(new Dimension(normalWidth, normalHeight));
        myRoundedPanel.setMinimumSize(new Dimension(normalWidth - MIN_WIDTH_SCROLLBAR_DELTA, normalHeight));
        myRoundedPanel.setMaximumSize(new Dimension(normalWidth * 2, normalHeight));

        setOpaque(true);
        setFocusable(true);
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        inactiveBackGroundColor = new Color(Math.max(0, ColorAndFontConstants.BACKGROUND_COLOR.getRed() - COLOR_DELTA), Math.max(0,
                ColorAndFontConstants.BACKGROUND_COLOR.getBlue() - COLOR_DELTA), Math.max(0, ColorAndFontConstants.BACKGROUND_COLOR.getGreen() - COLOR_DELTA));

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
        walletDescriptionTextField.addActionListener(this);
        walletDescriptionTextField.addFocusListener(this);
        walletDescriptionTextFieldBorder = walletDescriptionTextField.getBorder();
        walletDescriptionTextField.setOpaque(false);
        walletDescriptionTextField.setDisabledTextColor(Color.BLACK);
        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.92;
        constraints.weighty = 4;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        myRoundedPanel.add(walletDescriptionTextField, constraints);

        // twisty is initially invisible
        twistyLabel = new JLabel();
        twistyLabel.setOpaque(false);
        twistyLabel.setIcon(ImageLoader.createImageIcon(ImageLoader.TWISTY_DOWN_ICON_FILE));
        twistyLabel.setBorder(BorderFactory.createEmptyBorder(TWISTY_TOP_BORDER, TWISTY_LEFT_BORDER, 0, 0));
        twistyLabel.setVisible(false);
        twistyLabel.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
                expanded = !expanded;
                setSelected(selected);
                thisPanel.invalidate();
                thisPanel.validate();
                thisPanel.repaint();
            }
        });
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        myRoundedPanel.add(twistyLabel, constraints);

        amountLabel = new BlinkLabel(controller, false);
        amountLabel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
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
        detailPanel.setMinimumSize(new Dimension(normalWidth - MIN_WIDTH_SCROLLBAR_DELTA, detailHeight));
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
        return (int) (fontMetrics.getMaxAdvance() * WIDTH_OF_TEXT_FIELD * 0.66 + WIDTH_DELTA);
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
        this.selected = selected;

        myRoundedPanel.setSelected(selected);
        roundedBottomPanel.setSelected(selected);
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            if (expanded) {
                twistyLabel.setIcon(twistyDownIcon);
                twistyLabel.setToolTipText(controller.getLocaliser().getString("singleWalletPanel.twistyDownText"));
                detailPanel.setVisible(true);
                setPreferredSize(new Dimension(normalWidth, expandedHeight));
                setMinimumSize(new Dimension(normalWidth - MIN_WIDTH_SCROLLBAR_DELTA, expandedHeight));
                setMaximumSize(new Dimension(normalWidth * 2, expandedHeight));
            } else {
                twistyLabel.setIcon(twistyRightIcon);
                twistyLabel.setToolTipText(controller.getLocaliser().getString("singleWalletPanel.twistyRightText"));
                detailPanel.setVisible(false);
                setPreferredSize(new Dimension(normalWidth, normalHeight));
                setMinimumSize(new Dimension(normalWidth - MIN_WIDTH_SCROLLBAR_DELTA, normalHeight));
                setMaximumSize(new Dimension(normalWidth * 2, normalHeight));
            }

            if (selected) {
                walletDescriptionTextField.setEnabled(true);
                walletDescriptionTextField.setForeground(Color.BLACK);
                if (walletDescriptionTextField.isEditable()) {
                    // already editble
                } else {
                    walletDescriptionTextField.setEditable(true);
                    // may not have the caret quite right
                    // send the focus to the panel
                    requestFocusInWindow();
                }

                walletDescriptionTextField.setBorder(walletDescriptionTextFieldBorder);
                walletDescriptionTextField.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
                myRoundedPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
                roundedBottomPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
                
                myRoundedPanel.repaint();
                roundedBottomPanel.repaint();
                twistyLabel.setVisible(true);
            } else {
                walletDescriptionTextField.setEnabled(false);
                walletDescriptionTextField.setForeground(Color.BLACK);
                walletDescriptionTextField.setEditable(false);
                Border border = BorderFactory.createEmptyBorder(5, 7, 5, 5);
                walletDescriptionTextField.setBorder(border);
                walletDescriptionTextField.setBackground(inactiveBackGroundColor);
                myRoundedPanel.setBackground(inactiveBackGroundColor);
                roundedBottomPanel.setBackground(inactiveBackGroundColor);

                myRoundedPanel.repaint();
                roundedBottomPanel.repaint();
                twistyLabel.setVisible(false);
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
            walletDescriptionTextField.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
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
            detailPanel.setBackground(BACKGROUND_COLOR_DATA_HAS_CHANGED);
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
        underlineBorder = BorderFactory.createMatteBorder(0, 0, 1, 0, Color.WHITE);

        roundedBottomPanel = new RoundedBottomPanel(controller.getLocaliser().getLocale());
        roundedBottomPanel.setOpaque(true);
        roundedBottomPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        roundedBottomPanel.setLayout(new GridBagLayout());

        JPanel innerDetailPanel = new JPanel();
        innerDetailPanel.setOpaque(false);
        innerDetailPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        innerDetailPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();

        filenameLabel = new MultiBitLabel("");
        filenameLabel.setOpaque(false);
        filenameLabel.setBorder(underlineBorder);

        filenameLabel.setText(controller.getLocaliser().getString("resetTransactionsPanel.walletFilenameLabel"));
        filenameLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        innerDetailPanel.add(filenameLabel, constraints);
        
        filenameSeparator= new JLabel();
        filenameSeparator.setOpaque(false);
        filenameSeparator.setBorder(underlineBorder);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(filenameSeparator, constraints);

        walletFilenameLabel = new MultiBitLabel("");
        walletFilenameLabel.setBorder(underlineBorder);

        String walletFilename = perWalletModelData.getWalletFilename();

        File walletFile = new File(walletFilename);
        if (walletFile != null) {
            String walletFilenameFull = walletFile.getName();
            String walletFilenameShort = walletFilenameFull.replaceAll("\\.wallet", "");
            walletFilenameLabel.setText(walletFilenameShort);
            walletFilenameLabel.setToolTipText(walletFilename);
        }

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(walletFilenameLabel, constraints);

        MultiBitLabel sendLabelLabel = new MultiBitLabel("");
        sendLabelLabel.setText("Type");
        sendLabelLabel.setBorder(underlineBorder);
        sendLabelLabel.setHorizontalAlignment(SwingConstants.RIGHT);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        innerDetailPanel.add(sendLabelLabel, constraints);

        JLabel filler2 = new JLabel();
        filler2.setOpaque(false);
        filler2.setBorder(underlineBorder);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(filler2, constraints);

        MultiBitLabel sendLabelText = new MultiBitLabel("");
        sendLabelText.setText("unencrypted");
        sendLabelText.setBorder(underlineBorder);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(sendLabelText, constraints);

        MultiBitLabel sendLabelLabel2 = new MultiBitLabel("");
        sendLabelLabel2.setText("Version");

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        innerDetailPanel.add(sendLabelLabel2, constraints);

        JLabel filler3 = new JLabel();
        filler3.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(filler3, constraints);

        MultiBitLabel sendLabelText2 = new MultiBitLabel("");
        sendLabelText2.setText("1 (serialised)");

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(sendLabelText2, constraints);

        JLabel filler4 = new JLabel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(filler4, constraints);

        JPanel outerPanel = new JPanel(new GridBagLayout());
        outerPanel.setOpaque(false);
        outerPanel.setBackground(Color.WHITE);
        GridBagConstraints constraints2 = new GridBagConstraints();

        JLabel outerPadLeft = new JLabel();
        outerPadLeft.setOpaque(false);
        outerPadLeft.setMinimumSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        outerPadLeft.setPreferredSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        outerPadLeft.setMaximumSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 0.05;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        outerPanel.add(outerPadLeft, constraints2);

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.9;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        outerPanel.add(roundedBottomPanel, constraints2);

        JLabel outerPadRight = new JLabel();
        outerPadRight.setOpaque(false);
        outerPadRight.setMinimumSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        outerPadRight.setPreferredSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        outerPadRight.setMaximumSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.05;
        constraints2.weighty = 1.0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        outerPanel.add(outerPadRight, constraints2);

        GridBagConstraints constraints3 = new GridBagConstraints();

        JLabel innerPadLeft = new JLabel();
        innerPadLeft.setOpaque(false);
        innerPadLeft.setMinimumSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        innerPadLeft.setPreferredSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        innerPadLeft.setMaximumSize(new Dimension(DETAIL_PANEL_OUTER_INDENT, DETAIL_PANEL_OUTER_INDENT));
        constraints3.fill = GridBagConstraints.NONE;
        constraints3.gridx = 0;
        constraints3.gridy = 0;
        constraints3.weightx = 0.05;
        constraints3.weighty = 1.0;
        constraints3.gridwidth = 1;
        constraints3.gridheight = 1;
        constraints3.anchor = GridBagConstraints.LINE_START;
        roundedBottomPanel.add(innerPadLeft, constraints3);

        constraints3.fill = GridBagConstraints.BOTH;
        constraints3.gridx = 1;
        constraints3.gridy = 0;
        constraints3.weightx = 0.9;
        constraints3.weighty = 1.0;
        constraints3.gridwidth = 1;
        constraints3.gridheight = 1;
        constraints3.anchor = GridBagConstraints.CENTER;
        roundedBottomPanel.add(innerDetailPanel, constraints3);

        JLabel innerPadRight = new JLabel();
        innerPadRight.setOpaque(false);
        innerPadRight.setMinimumSize(new Dimension(DETAIL_PANEL_INNER_INDENT, DETAIL_PANEL_INNER_INDENT));
        innerPadRight.setPreferredSize(new Dimension(DETAIL_PANEL_INNER_INDENT, DETAIL_PANEL_INNER_INDENT));
        innerPadRight.setMaximumSize(new Dimension(DETAIL_PANEL_INNER_INDENT, DETAIL_PANEL_INNER_INDENT));
        constraints3.fill = GridBagConstraints.NONE;
        constraints3.gridx = 2;
        constraints3.gridy = 0;
        constraints3.weightx = 0.05;
        constraints3.weighty = 1.0;
        constraints3.gridwidth = 1;
        constraints3.gridheight = 1;
        constraints3.anchor = GridBagConstraints.LINE_END;
        roundedBottomPanel.add(innerPadRight, constraints3);

        JLabel innerPadBottom = new JLabel();
        innerPadBottom.setOpaque(false);
        innerPadBottom.setMinimumSize(new Dimension(DETAIL_PANEL_INNER_INDENT, DETAIL_PANEL_INNER_INDENT));
        innerPadBottom.setPreferredSize(new Dimension(DETAIL_PANEL_INNER_INDENT, DETAIL_PANEL_INNER_INDENT));
        innerPadBottom.setMaximumSize(new Dimension(DETAIL_PANEL_INNER_INDENT, DETAIL_PANEL_INNER_INDENT));
        constraints3.fill = GridBagConstraints.NONE;
        constraints3.gridx = 1;
        constraints3.gridy = 1;
        constraints3.weightx = 0.05;
        constraints3.weighty = 1.0;
        constraints3.gridwidth = 1;
        constraints3.gridheight = 1;
        constraints3.anchor = GridBagConstraints.CENTER;
        roundedBottomPanel.add(innerPadBottom, constraints3);

        return outerPanel;
    }
}