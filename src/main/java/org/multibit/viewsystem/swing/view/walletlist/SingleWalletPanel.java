/**
 * Copyright 2013 multibit.org
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
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.math.BigInteger;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

import org.joda.money.Money;
import org.multibit.controller.Controller;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletVersion;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

import com.google.bitcoin.core.Wallet.BalanceType;

public class SingleWalletPanel extends JPanel implements ActionListener, FocusListener {

    private static final int WIDTH_OF_TEXT_FIELD = 12;

    private static final long serialVersionUID = -7110340338285836548L;

    private static final Dimension ABOVE_BASELINE_LEADING_CORNER_PADDING = new Dimension(5, 5);
    private static final Dimension BELOW_BASELINE_TRAILING_CORNER_PADDING = new Dimension(5, 5);

    private PerWalletModelData perWalletModelData;

    private static final Color BACKGROUND_COLOR_DATA_HAS_CHANGED = new Color(0xff, 0xff, 0xff);
    private static final int COLOR_DELTA = 24;

    private static final int HEIGHT_DELTA = 26;
    private static final int DETAIL_HEIGHT_DELTA = 4;
    private static final int WIDTH_DELTA = 4;
    private static final int MIN_WIDTH_SCROLLBAR_DELTA = 20;
    private static final double MINIMUM_WIDTH_SCALE_FACTOR = 0.5;

    private static Color inactiveBackGroundColor;
    private Border underlineBorder;
    private Border overlineBorder;
    private MultiBitTextField walletDescriptionTextField;
    private Border walletDescriptionTextFieldBorder;

    private BlinkLabel amountLabelBTC;
    private BlinkLabel amountLabelFiat;

    private Controller controller;
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
    public static int DESCRIPTION_HEIGHT_DELTA = 4;
    private static int AMOUNT_HEIGHT_DELTA = 4;

    private static int DETAIL_PANEL_OUTER_INDENT = 3;
    private static int DETAIL_PANEL_INNER_INDENT = 1;

    private boolean expanded = false;

    private boolean selected = false;

    private static final int TWISTY_LEFT_OR_RIGHT_BORDER = 8;
    private static final int TWISTY_TOP_BORDER = 3;

    private JLabel twistyLabel;
    private Icon twistyLeftIcon;
    private Icon twistyRightIcon;
    private Icon twistyDownIcon;

    private JPanel twistyStent;

    private MultiBitLabel filenameLabel;
    private JLabel filenameSeparator;
    private MultiBitLabel walletFilenameLabel;

    private FontMetrics fontMetrics;

    public SingleWalletPanel(PerWalletModelData perWalletModelData, final MultiBitController controller, MultiBitFrame mainFrame, final WalletListPanel walletListPanel) {
        this.perWalletModelData = perWalletModelData;
        this.controller = controller;
        this.mainFrame = mainFrame;
                
        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        fontMetrics = getFontMetrics(font);

        // By default not expanded, not selected.
        expanded = false;
        selected = false;

        twistyLeftIcon = ImageLoader.createImageIcon(ImageLoader.TWISTY_LEFT_ICON_FILE);
        twistyRightIcon = ImageLoader.createImageIcon(ImageLoader.TWISTY_RIGHT_ICON_FILE);
        twistyDownIcon = ImageLoader.createImageIcon(ImageLoader.TWISTY_DOWN_ICON_FILE);

        normalHeight = NUMBER_OF_ROWS_IN_SUMMARY_PANEL * fontMetrics.getHeight() + DESCRIPTION_HEIGHT_DELTA + AMOUNT_HEIGHT_DELTA + HEIGHT_DELTA;
        normalWidth = calculateNormalWidth(this);
        expandedHeight = (int) ((NUMBER_OF_ROWS_IN_SUMMARY_PANEL + NUMBER_OF_ROWS_IN_DETAIL_PANEL) * fontMetrics.getHeight()
                + DESCRIPTION_HEIGHT_DELTA + HEIGHT_DELTA + + AMOUNT_HEIGHT_DELTA + DETAIL_HEIGHT_DELTA);
        detailHeight = (int) ((NUMBER_OF_ROWS_IN_DETAIL_PANEL) * fontMetrics.getHeight() + DETAIL_HEIGHT_DELTA);

        // Add contents to myRoundedPanel.
        myRoundedPanel = new RoundedPanel(controller.getLocaliser().getLocale());
        myRoundedPanel.setLayout(new GridBagLayout());
        myRoundedPanel.setOpaque(false);
        myRoundedPanel.setPreferredSize(new Dimension(normalWidth, normalHeight));
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            myRoundedPanel.setMinimumSize(new Dimension(calculateMinimumWidth(normalWidth), normalHeight));
        } else {
            myRoundedPanel.setMinimumSize(new Dimension(normalWidth, normalHeight));
        }

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
        walletDescriptionTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (walletListPanel != null) {
                    walletListPanel.selectAdjacentWallet(e, "SingleWalletPanel#WalletDescriptionTextField"); 
                }
                super.keyTyped(e);
            }            
        });
        //walletDescriptionTextField.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        walletDescriptionTextFieldBorder = walletDescriptionTextField.getBorder();
        walletDescriptionTextField.setOpaque(false);
        walletDescriptionTextField.setDisabledTextColor(Color.BLACK);
        
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 100;
        constraints.weighty = 8;
        constraints.gridwidth = 5;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        myRoundedPanel.add(walletDescriptionTextField, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        myRoundedPanel.add(MultiBitTitledPanel.createStent(2, 2), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        JPanel iconRowStent = MultiBitTitledPanel.createStent((int)(walletDescriptionTextFieldBorder.getBorderInsets(this).left * 0.5));
        //iconRowStent.setBorder(BorderFactory.createLineBorder(Color.RED));
        myRoundedPanel.add(iconRowStent, constraints);

        // Twisty is initially invisible.
        twistyLabel = new JLabel();
        twistyLabel.setOpaque(false);
        twistyLabel.setIcon(ImageLoader.createImageIcon(ImageLoader.TWISTY_DOWN_ICON_FILE));
        twistyLabel.setBorder(BorderFactory.createEmptyBorder(TWISTY_TOP_BORDER, TWISTY_LEFT_OR_RIGHT_BORDER, 0, TWISTY_LEFT_OR_RIGHT_BORDER));
        twistyLabel.setVisible(false);
        twistyLabel.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
                expanded = !expanded;
                setSelected(selected);
            }
        });
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        myRoundedPanel.add(twistyLabel, constraints);
        twistyStent = MultiBitTitledPanel.createStent(16, 16);
        twistyStent.setBorder(BorderFactory.createEmptyBorder(TWISTY_TOP_BORDER, TWISTY_LEFT_OR_RIGHT_BORDER, 0, TWISTY_LEFT_OR_RIGHT_BORDER));
        twistyStent.setOpaque(false);
        myRoundedPanel.add(twistyStent, constraints);

        amountLabelBTC = new BlinkLabel(controller, false);
        amountLabelBTC.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        amountLabelBTC.setBorder(BorderFactory.createEmptyBorder(0, 3, 0, 3));
        //amountLabelBTC.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        amountLabelBTC.setBlinkEnabled(true);
        amountLabelBTC.setHorizontalAlignment(JLabel.TRAILING);

        amountLabelFiat = new BlinkLabel(controller, false);
        amountLabelFiat.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        amountLabelFiat.setBorder(BorderFactory.createEmptyBorder(0, 3, 0, 3));
        //amountLabelFiat.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        amountLabelFiat.setBlinkEnabled(true);
        amountLabelFiat.setHorizontalAlignment(JLabel.TRAILING);

        JPanel stent = new JPanel();
        stent.setOpaque(false);
        //stent.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1,Color.RED));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 10000;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        myRoundedPanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        myRoundedPanel.add(amountLabelBTC, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        myRoundedPanel.add(MultiBitTitledPanel.createStent(1), constraints);

        constraints.fill = GridBagConstraints.VERTICAL;
        constraints.gridx = 5;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        myRoundedPanel.add(amountLabelFiat, constraints);

        JPanel filler4 = new JPanel();
        filler4.setMinimumSize(BELOW_BASELINE_TRAILING_CORNER_PADDING);
        filler4.setPreferredSize(BELOW_BASELINE_TRAILING_CORNER_PADDING);
        filler4.setMaximumSize(BELOW_BASELINE_TRAILING_CORNER_PADDING);
        //filler4.setBorder(BorderFactory.createLineBorder(Color.RED));
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 6;
        constraints.gridy = 4;
        constraints.weightx = 0.02;
        constraints.weighty = 0.02;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.BELOW_BASELINE_TRAILING;
        myRoundedPanel.add(filler4, constraints);

        // Add myRoundedPanel to myself.
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

        // Add detail panel.
        detailPanel = createWalletDetailPanel();
        detailPanel.setPreferredSize(new Dimension(normalWidth, detailHeight));
        detailPanel.setMinimumSize(new Dimension(calculateMinimumWidth(normalWidth), detailHeight));
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

        // Add bottom filler.
        JPanel filler = new JPanel();
        filler.setOpaque(false);
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.weightx = 1.0;
        constraints2.weighty = 1000;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        add(filler, constraints2);

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        setSelected(false);
       
        updateFromModel(false);
        
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent arg0) {
                if (walletListPanel != null) {
                    walletListPanel.selectAdjacentWallet(arg0, "SingleWalletPanel");
                }
                super.keyTyped(arg0);                
            }    
        });
    }

    public static int calculateNormalWidth(JComponent component) {
        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        FontMetrics fontMetrics = component.getFontMetrics(font);
        return (int) (fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_MEDIUM_FIELD_TEXT) + WIDTH_DELTA) ;
        
    }
    
    private int calculateMinimumWidth(int normalWidth) {
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            return (int)Math.max(0, normalWidth * MINIMUM_WIDTH_SCALE_FACTOR - MIN_WIDTH_SCROLLBAR_DELTA);            
        } else {
            return normalWidth;
        }
    }

    @Override
    public void addMouseListener(MouseListener mouseListener) {
        super.addMouseListener(mouseListener);
        walletDescriptionTextField.addMouseListener(mouseListener);
        amountLabelBTC.addMouseListener(mouseListener);
        amountLabelFiat.addMouseListener(mouseListener);
        detailPanel.addMouseListener(mouseListener);
        myRoundedPanel.addMouseListener(mouseListener);
        
        filenameLabel.addMouseListener(mouseListener);
        filenameSeparator.addMouseListener(mouseListener);
        walletFilenameLabel.addMouseListener(mouseListener);
    }

    public void setSelected(boolean selected) {
        this.selected = selected;

        myRoundedPanel.setSelected(selected);
        roundedBottomPanel.setSelected(selected);
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            if (expanded) {
                twistyLabel.setIcon(twistyDownIcon);
                twistyLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("singleWalletPanel.twistyDownText")));
                detailPanel.setVisible(true);
                setPreferredSize(new Dimension(normalWidth, expandedHeight));
                if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
                    setMinimumSize(new Dimension(calculateMinimumWidth(normalWidth), expandedHeight));
                } else {
                    setMinimumSize(new Dimension(normalWidth, expandedHeight));
                }
                setMaximumSize(new Dimension(normalWidth * 2, expandedHeight));
            } else {
                if (ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()) == ComponentOrientation.LEFT_TO_RIGHT) {
                    twistyLabel.setIcon(twistyRightIcon);
                } else {
                    twistyLabel.setIcon(twistyLeftIcon);                    
                }
                twistyLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("singleWalletPanel.twistyRightText")));
                detailPanel.setVisible(false);
                setPreferredSize(new Dimension(normalWidth, normalHeight));
                if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
                    setMinimumSize(new Dimension(calculateMinimumWidth(normalWidth), normalHeight));
                } else {
                    setMinimumSize(new Dimension(normalWidth, normalHeight));                    
                }
                setMaximumSize(new Dimension(normalWidth * 2, normalHeight));
            }

            if (selected) {
                walletDescriptionTextField.setForeground(Color.BLACK);
                if (!walletDescriptionTextField.isEditable()) {
                    walletDescriptionTextField.setEditable(true);
                }
                requestFocusInWindow();

                walletDescriptionTextField.setBorder(walletDescriptionTextFieldBorder);
                walletDescriptionTextField.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
                myRoundedPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
                roundedBottomPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
                
                myRoundedPanel.repaint();
                roundedBottomPanel.repaint();
                twistyLabel.setVisible(true);
                twistyStent.setVisible(false);
            } else {
                walletDescriptionTextField.setForeground(Color.BLACK);
                walletDescriptionTextField.setEditable(false);
                Insets insets = walletDescriptionTextFieldBorder.getBorderInsets(this);
                Border border = BorderFactory.createEmptyBorder(insets.top, insets.left, insets.bottom, insets.right);
                walletDescriptionTextField.setBorder(border);
                walletDescriptionTextField.setBackground(inactiveBackGroundColor);
                myRoundedPanel.setBackground(inactiveBackGroundColor);
                roundedBottomPanel.setBackground(inactiveBackGroundColor);

                myRoundedPanel.repaint();
                roundedBottomPanel.repaint();
                twistyLabel.setVisible(false);
                twistyStent.setVisible(true);
            }
        }
    }

    public void actionPerformed(ActionEvent evt) {
        saveChanges();
        if (!perWalletModelData.isFilesHaveBeenChangedByAnotherProcess()) {
            walletDescriptionTextField.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        }
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

            if (!(arg0.getSource() instanceof JTextField)) {
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
     * Update any UI elements from the model (hint that data has changed).
     */
    public void updateFromModel(boolean blinkEnabled) { 
        inactiveBackGroundColor = new Color(Math.max(0, ColorAndFontConstants.BACKGROUND_COLOR.getRed() - COLOR_DELTA), Math.max(0,
                ColorAndFontConstants.BACKGROUND_COLOR.getBlue() - COLOR_DELTA), Math.max(0, ColorAndFontConstants.BACKGROUND_COLOR.getGreen() - COLOR_DELTA));

        BigInteger estimatedBalance = perWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED);
        String balanceTextToShowBTC = controller.getLocaliser().bitcoinValueToString(estimatedBalance, true, false);
        String balanceTextToShowFiat = "";
        if (CurrencyConverter.INSTANCE.getRate() != null && CurrencyConverter.INSTANCE.isShowingFiat()) {
            Money fiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(estimatedBalance);
            balanceTextToShowFiat = "(" + CurrencyConverter.INSTANCE.getFiatAsLocalisedString(fiat) + ")";
        }
        
        if (amountLabelBTC != null && amountLabelBTC.getText() != null && !"".equals(amountLabelBTC.getText()) && !balanceTextToShowBTC.equals(amountLabelBTC.getText()) && blinkEnabled) {
            amountLabelBTC.blink(balanceTextToShowBTC);
            amountLabelFiat.blink(balanceTextToShowFiat);
        } else {
            amountLabelBTC.setText(balanceTextToShowBTC);
            amountLabelFiat.setText(balanceTextToShowFiat);
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
            amountLabelBTC.setText("");
            amountLabelBTC.setEnabled(false);
            amountLabelFiat.setText("");
            amountLabelFiat.setEnabled(false);
        }
    }

    /**
     * create the wallet details panel
     */
    private JPanel createWalletDetailPanel() {
        underlineBorder = BorderFactory.createMatteBorder(0, 0, 1, 0, Color.WHITE);
        overlineBorder = BorderFactory.createMatteBorder(1, 0, 0, 0, Color.WHITE);

        roundedBottomPanel = new RoundedBottomPanel(controller.getLocaliser().getLocale());
        roundedBottomPanel.setOpaque(true);
        roundedBottomPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        roundedBottomPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        roundedBottomPanel.setLayout(new GridBagLayout());

        JPanel innerDetailPanel = new JPanel();
        innerDetailPanel.setOpaque(false);
        innerDetailPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        innerDetailPanel.setLayout(new GridBagLayout());

        GridBagConstraints constraints = new GridBagConstraints();

        filenameLabel = new MultiBitLabel("");
        filenameLabel.setOpaque(false);
        filenameLabel.setBorder(underlineBorder);

        String filenameString = controller.getLocaliser().getString("resetTransactionsPanel.walletFilenameLabel");
        filenameString = WhitespaceTrimmer.trim(filenameString.replaceAll(":", "")) + " : ";
        filenameLabel.setText(filenameString);
        filenameLabel.setHorizontalAlignment(SwingConstants.TRAILING);
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
            String tooltipText = HelpContentsPanel.createTooltipText(walletFilename);
            walletFilenameLabel.setToolTipText(tooltipText);
            filenameSeparator.setToolTipText(tooltipText);
            filenameLabel.setToolTipText(tooltipText);
        }

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(walletFilenameLabel, constraints);

        WalletVersion walletVersion = perWalletModelData.getWalletInfo().getWalletVersion();
        
        Action walletFormatHelpAction = new HelpContextAction(controller, null, walletVersion.getLocalisationKey(),
                walletVersion.getLocalisationKey(), walletVersion.getLocalisationKey(), HelpContentsPanel.HELP_WALLET_FORMATS_URL);

        MultiBitButton walletFormatLabel = new HelpButton(walletFormatHelpAction, controller);
        String walletFormatString = WhitespaceTrimmer.trim(controller.getLocaliser().getString("singleWalletPanel.walletFormat.text").replaceAll(":", "")) + " : ";
        walletFormatLabel.setText(walletFormatString);
        walletFormatLabel.setBorder(BorderFactory.createEmptyBorder());
        walletFormatLabel.setContentAreaFilled(false);
        walletFormatLabel.setHorizontalAlignment(SwingConstants.LEADING);
        walletFormatLabel.setHorizontalTextPosition(SwingConstants.LEADING);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        innerDetailPanel.add(walletFormatLabel, constraints);

        MultiBitButton walletFormatFiller = new HelpButton(walletFormatHelpAction, controller);
        walletFormatFiller.setText("");
        walletFormatFiller.setBorder(BorderFactory.createEmptyBorder());
        walletFormatFiller.setContentAreaFilled(false);
        walletFormatFiller.setHorizontalAlignment(SwingConstants.LEADING);
        walletFormatFiller.setHorizontalTextPosition(SwingConstants.LEADING);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(walletFormatFiller, constraints);

        MultiBitButton walletFormatButton = new HelpButton(walletFormatHelpAction, controller);
        walletFormatButton.setText(controller.getLocaliser().getString(walletVersion.getLocalisationKey()));
        walletFormatButton.setBorder(BorderFactory.createEmptyBorder());
        walletFormatButton.setContentAreaFilled(false);
        walletFormatButton.setHorizontalAlignment(SwingConstants.LEADING);
        walletFormatButton.setHorizontalTextPosition(SwingConstants.LEADING);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(walletFormatButton, constraints);

        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("singleWalletPanel.walletFormat.tooltip"), "\n",
                controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        walletFormatButton.setToolTipText(tooltipText);
        walletFormatFiller.setToolTipText(tooltipText);
        walletFormatLabel.setToolTipText(tooltipText);

        MultiBitLabel walletTypeLabel = new MultiBitLabel("");
        String walletTypeString = WhitespaceTrimmer.trim(controller.getLocaliser().getString("singleWalletPanel.type").replaceAll(":", "")) + " : ";

        walletTypeLabel.setText(walletTypeString);
        walletTypeLabel.setBorder(overlineBorder);
        walletTypeLabel.setHorizontalAlignment(SwingConstants.TRAILING);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        innerDetailPanel.add(walletTypeLabel, constraints);

        JLabel walletTypeFiller = new JLabel("");
        walletTypeFiller.setOpaque(false);
        walletTypeFiller.setText("");
        walletTypeFiller.setBorder(overlineBorder);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(walletTypeFiller, constraints);

        MultiBitLabel walletTypeText = new MultiBitLabel("");
        walletTypeText.setText(controller.getLocaliser().getString("singleWalletPanel.unencrypted"));
        walletTypeText.setBorder(overlineBorder);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        innerDetailPanel.add(walletTypeText, constraints);

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

    public int getFiatLabelWidth() {
        return fontMetrics.stringWidth(amountLabelFiat.getText());
    }
    
    public void setFiatLabelWidth(int fiatLabelMinimumWidth) {
        amountLabelFiat.setMinimumSize(new Dimension(fiatLabelMinimumWidth, amountLabelFiat.getMinimumSize().height));
        amountLabelFiat.setPreferredSize(new Dimension(fiatLabelMinimumWidth, amountLabelFiat.getPreferredSize().height));
        amountLabelFiat.setMaximumSize(new Dimension(fiatLabelMinimumWidth, amountLabelFiat.getMaximumSize().height));
    }

    public boolean isSelectedInternal() {
        return selected;
    }
}