/**
 * Copyright 2012 multibit.org
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
package org.multibit.viewsystem.swing.view.panels;

import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ExportPrivateKeysSubmitAction;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.view.PrivateKeyFileFilter;
import org.multibit.viewsystem.swing.view.components.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Arrays;
import java.util.Locale;


/**
 * The export private keys panel.
 */
public class ExportPrivateKeysPanel extends JPanel implements Viewable, WalletBusyListener {

    private static final long serialVersionUID = 444992298119957705L;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private MultiBitFrame mainFrame;

    private MultiBitLabel walletFilenameLabel;

    private MultiBitLabel walletDescriptionLabel;

    private String chooseFilenameButtonText;

    private MultiBitLabel outputFilenameLabel;

    private MultiBitLabel messageLabel1;
    private MultiBitLabel messageLabel2;

    private String outputFilename;
    private String walletFilenameForChosenOutputFilename;
    
    private JRadioButton passwordProtect;
    private JRadioButton doNotPasswordProtect;
    private MultiBitLabel doNotPasswordProtectWarningLabel;

    private JPasswordField exportFilePasswordField;
    private JPasswordField repeatExportFilePasswordField;

    private JPasswordField walletPasswordField;
    private MultiBitLabel walletPasswordPromptLabel;
    
    private ExportPrivateKeysSubmitAction exportPrivateKeySubmitAction;

    private JLabel tickLabel;

    public static final int STENT_HEIGHT = 12;
    public static final int STENT_DELTA = 20;
    
    private Font adjustedFont;

    /**
     * Creates a new {@link ExportPrivateKeysPanel}.
     */
    public ExportPrivateKeysPanel(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;

        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        outputFilename = "";
        walletFilenameForChosenOutputFilename = "";

        initUI();
        
        this.bitcoinController.registerWalletBusyListener(this);
        walletBusyChange(this.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
        
        boolean walletPasswordRequired = false;
        if (this.bitcoinController.getModel().getActiveWallet() != null && this.bitcoinController.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
            walletPasswordRequired = true;
        }
        enableWalletPassword(walletPasswordRequired);
    }

    @Override
    public void navigateAwayFromView() {
    }

    private void initUI() {
        setLayout(new BorderLayout());

        JPanel mainPanel = new JPanel();
        mainPanel.setMinimumSize(new Dimension(550, 160));
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setOpaque(false);
        mainPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        String[] keys = new String[] { "resetTransactionsPanel.walletDescriptionLabel",
                "resetTransactionsPanel.walletFilenameLabel", "showExportPrivateKeysPanel.passwordPrompt",
                "showExportPrivateKeysPanel.repeatPasswordPrompt", "showImportPrivateKeysPanel.numberOfKeys.text",
                "showImportPrivateKeysPanel.replayDate.text" };

        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this) + STENT_DELTA;

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createWalletPanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createFilenamePanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createPasswordPanel(stentWidth), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(filler1, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createButtonPanel(), constraints);

        messageLabel1 = new MultiBitLabel("");
        messageLabel1.setOpaque(false);
        messageLabel1.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));
        messageLabel1.setHorizontalAlignment(JLabel.LEADING);
        messageLabel1.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(messageLabel1, constraints);

        messageLabel2 = new MultiBitLabel("");
        messageLabel2.setOpaque(false);
        messageLabel2.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));
        messageLabel2.setHorizontalAlignment(JLabel.LEADING);
        messageLabel2.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(messageLabel2, constraints);

        Action helpAction;
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
                    "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                    HelpContentsPanel.HELP_EXPORTING_PRIVATE_KEYS_URL);
        } else {
            helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_RTL_ICON_FILE,
                    "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
                    HelpContentsPanel.HELP_EXPORTING_PRIVATE_KEYS_URL);
        }
        HelpButton helpButton = new HelpButton(helpAction, controller);
        helpButton.setText("");

        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("multiBitFrame.helpMenuTooltip") });
        helpButton.setToolTipText(tooltipText);
        helpButton.setHorizontalAlignment(SwingConstants.LEADING);
        helpButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        helpButton.setBorder(BorderFactory.createEmptyBorder(0, AbstractTradePanel.HELP_BUTTON_INDENT, AbstractTradePanel.HELP_BUTTON_INDENT, AbstractTradePanel.HELP_BUTTON_INDENT));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 7;
        constraints.weightx = 1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.BASELINE_LEADING;
        mainPanel.add(helpButton, constraints);

        JLabel filler2 = new JLabel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 8;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 100;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(filler2, constraints);

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        //mainScrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, ColorAndFontConstants.DARK_BACKGROUND_COLOR));
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        mainScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        mainScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

        add(mainScrollPane, BorderLayout.CENTER);
    }

    private JPanel createWalletPanel(int stentWidth) {
        MultiBitTitledPanel inputWalletPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.wallet.title"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
                controller.getLocaliser().getString("showExportPrivateKeysPanel.wallet.text"), 3, inputWalletPanel);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(MultiBitTitledPanel.createStent(stentWidth, STENT_HEIGHT), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        inputWalletPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        JPanel filler0 = new JPanel();
        filler0.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx =3;
        constraints.gridy = 4;
        constraints.weightx = 100;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(filler0, constraints);
  
        MultiBitLabel walletDescriptionLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletDescriptionLabel"));
        walletDescriptionLabelLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(walletDescriptionLabelLabel, constraints);

        walletDescriptionLabel = new MultiBitLabel(this.bitcoinController.getModel().getActivePerWalletModelData().getWalletDescription());
        walletDescriptionLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 5;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(walletDescriptionLabel, constraints);

        MultiBitLabel walletFilenameLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletFilenameLabel"));
        walletFilenameLabelLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(walletFilenameLabelLabel, constraints);

        walletFilenameLabel = new MultiBitLabel(this.bitcoinController.getModel().getActiveWalletFilename());
        walletFilenameLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 6;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(walletFilenameLabel, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(fill1, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(filler3, constraints);

        walletPasswordPromptLabel = new MultiBitLabel(controller.getLocaliser().getString("showExportPrivateKeysPanel.walletPasswordPrompt"));
        walletPasswordPromptLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 8;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(walletPasswordPromptLabel, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 8;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        inputWalletPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS),
                constraints);

        walletPasswordField = new JPasswordField(24);
        walletPasswordField.setMinimumSize(new Dimension(200, 20));
        walletPasswordField.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 8;
        constraints.weightx = 0.3;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(walletPasswordField, constraints);

        JPanel filler4 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 9;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(filler4, constraints);

        return inputWalletPanel;
    }

    private JPanel createFilenamePanel(int stentWidth) {
        MultiBitTitledPanel outputFilenamePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.filename.title"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel indent = MultiBitTitledPanel.getIndentPanel(1);
        outputFilenamePanel.add(indent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth, STENT_HEIGHT);
        outputFilenamePanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        outputFilenamePanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        JPanel filler0 = new JPanel();
        filler0.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx =3;
        constraints.gridy = 3;
        constraints.weightx = 100;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        outputFilenamePanel.add(filler0, constraints);
 
        chooseFilenameButtonText = "";
        String chooseFilenameButtonText1 = controller.getLocaliser().getString("showExportPrivateKeysPanel.filename.text");
        String chooseFilenameButtonText2 = controller.getLocaliser().getString("showExportPrivateKeysPanel.filename.text.2");
        // If the second term is localised, use that, otherwise the first.
        if (controller.getLocaliser().getLocale().equals(Locale.ENGLISH)) {
            chooseFilenameButtonText = chooseFilenameButtonText2;
        } else {
            if (!"Export to ...".equals(chooseFilenameButtonText2)) {
                chooseFilenameButtonText = chooseFilenameButtonText2;
            } else {
                chooseFilenameButtonText = chooseFilenameButtonText1;
            }
        }

        MultiBitButton chooseOutputFilenameButton = new MultiBitButton(chooseFilenameButtonText);
        chooseOutputFilenameButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        chooseOutputFilenameButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                chooseFile();
            }
        });
        chooseOutputFilenameButton.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser()
                .getString("showExportPrivateKeysPanel.filename.tooltip")));

        MultiBitLabel walletFilenameLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletFilenameLabel"));
        walletFilenameLabelLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        outputFilenamePanel.add(walletFilenameLabelLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler2, constraints);

        outputFilenameLabel = new MultiBitLabel(outputFilename);
        outputFilenameLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(outputFilenameLabel, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 4;
        constraints.weightx = 20;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        outputFilenamePanel.add(fill1, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler3, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 6;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(chooseOutputFilenameButton, constraints);

        JPanel filler4 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler4, constraints);

        return outputFilenamePanel;
    }

    private JPanel createPasswordPanel(int stentWidth) {
        // do/do not password protect radios
        MultiBitTitledPanel passwordProtectPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.password.title"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel indent = MultiBitTitledPanel.getIndentPanel(1);
        passwordProtectPanel.add(indent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel stent = MultiBitTitledPanel.createStent(stentWidth, STENT_HEIGHT);
        passwordProtectPanel.add(stent, constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        JPanel filler0 = new JPanel();
        filler0.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 3;
        constraints.weightx = 100;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        passwordProtectPanel.add(filler0, constraints);

        ButtonGroup usePasswordGroup = new ButtonGroup();
        passwordProtect = new JRadioButton(controller.getLocaliser().getString("showExportPrivateKeysPanel.passwordProtect"));
        passwordProtect.setOpaque(false);
        passwordProtect.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        passwordProtect.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        doNotPasswordProtect = new JRadioButton(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.doNotPasswordProtect"));
        doNotPasswordProtect.setOpaque(false);
        doNotPasswordProtect.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        doNotPasswordProtect.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        ItemListener itemListener = new ChangePasswordProtectListener();
        passwordProtect.addItemListener(itemListener);
        doNotPasswordProtect.addItemListener(itemListener);
        usePasswordGroup.add(passwordProtect);
        usePasswordGroup.add(doNotPasswordProtect);
        passwordProtect.setSelected(true);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(passwordProtect, constraints);

        MultiBitLabel passwordPromptLabel = new MultiBitLabel("");
        passwordPromptLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        passwordPromptLabel.setText(controller.getLocaliser().getString("showExportPrivateKeysPanel.passwordPrompt"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        passwordProtectPanel.add(passwordPromptLabel, constraints);

        exportFilePasswordField = new JPasswordField(24);
        exportFilePasswordField.setMinimumSize(new Dimension(200, 20));
        exportFilePasswordField.addKeyListener(new PasswordListener());
        exportFilePasswordField.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.25;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(exportFilePasswordField, constraints);

        JLabel filler3 = new JLabel();
        filler3.setMinimumSize(new Dimension(3, 3));
        filler3.setMaximumSize(new Dimension(3, 3));
        filler3.setPreferredSize(new Dimension(3, 3));
        filler3.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler3, constraints);

        MultiBitLabel repeatPasswordPromptLabel = new MultiBitLabel("");
        repeatPasswordPromptLabel.setText(controller.getLocaliser().getString("showExportPrivateKeysPanel.repeatPasswordPrompt"));
        repeatPasswordPromptLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        passwordProtectPanel.add(repeatPasswordPromptLabel, constraints);

        repeatExportFilePasswordField = new JPasswordField(24);
        repeatExportFilePasswordField.setMinimumSize(new Dimension(200, 20));
        repeatExportFilePasswordField.addKeyListener(new PasswordListener());
        repeatExportFilePasswordField.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 7;
        constraints.weightx = 0.3;
        constraints.weighty = 0.25;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(repeatExportFilePasswordField, constraints);

        ImageIcon tickIcon = ImageLoader.createImageIcon(ImageLoader.TICK_ICON_FILE);
        tickLabel = new JLabel(tickIcon);
        tickLabel.setToolTipText(HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showExportPrivateKeysPanel.theTwoPasswordsMatch")));

        tickLabel.setVisible(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(tickLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 8;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(doNotPasswordProtect, constraints);

        doNotPasswordProtectWarningLabel = new MultiBitLabel(" ");
        doNotPasswordProtectWarningLabel.setForeground(Color.RED);
        doNotPasswordProtectWarningLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 9;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(doNotPasswordProtectWarningLabel, constraints);

        return passwordProtectPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.TRAILING);
        buttonPanel.setLayout(flowLayout);
        buttonPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));


        /**
         * Create submit action with references to the password fields - this
         * avoids having any public accessors on the panel
         */
        exportPrivateKeySubmitAction = new ExportPrivateKeysSubmitAction(this.bitcoinController, this,
                ImageLoader.createImageIcon(ImageLoader.EXPORT_PRIVATE_KEYS_ICON_FILE), walletPasswordField, exportFilePasswordField, repeatExportFilePasswordField, mainFrame);
        MultiBitButton submitButton = new MultiBitButton(exportPrivateKeySubmitAction, controller);
        submitButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

    @Override
    public void displayView(DisplayHint displayHint) {
        // If it is a wallet transaction change no need to update.
        if (DisplayHint.WALLET_TRANSACTIONS_HAVE_CHANGED == displayHint) {
            return;
        }
        walletFilenameLabel.setText(this.bitcoinController.getModel().getActiveWalletFilename());
        walletDescriptionLabel.setText(this.bitcoinController.getModel().getActivePerWalletModelData().getWalletDescription());

        boolean walletPasswordRequired = false;
        if (this.bitcoinController.getModel().getActiveWallet() != null && this.bitcoinController.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
            walletPasswordRequired = true;
        }
        enableWalletPassword(walletPasswordRequired);
        
        walletBusyChange(this.bitcoinController.getModel().getActivePerWalletModelData().isBusy());

        if (outputFilename == null || "".equals(outputFilename) || 
                (walletFilenameForChosenOutputFilename != null && !walletFilenameForChosenOutputFilename.equals(this.bitcoinController.getModel().getActiveWalletFilename()))) {
            outputFilename = createDefaultKeyFilename(this.bitcoinController.getModel().getActiveWalletFilename());
            walletFilenameForChosenOutputFilename = this.bitcoinController.getModel().getActiveWalletFilename(); 
            outputFilenameLabel.setText(outputFilename);
        }

        clearMessages();
    }
    
    private void enableWalletPassword(boolean enableWalletPassword) {
        if (enableWalletPassword) {
            // Enable the wallet password.
            walletPasswordField.setEnabled(true);
            walletPasswordPromptLabel.setEnabled(true);
        } else {
            // Disable the wallet password.
            walletPasswordField.setEnabled(false);
            walletPasswordPromptLabel.setEnabled(false);
        }
    }

    public boolean requiresEncryption() {
        boolean requiresEncryption = false;
        if (passwordProtect != null && passwordProtect.isSelected()) {
            requiresEncryption = true;
        }
        return requiresEncryption;
    }

    private void chooseFile() {
        JFileChooser.setDefaultLocale(controller.getLocaliser().getLocale());
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setLocale(controller.getLocaliser().getLocale());
        fileChooser.setDialogTitle(chooseFilenameButtonText);
        adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
        if (adjustedFont != null) {
            setFileChooserFont(new Container[] {fileChooser});
        }
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(new PrivateKeyFileFilter(controller));
        fileChooser.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        if (outputFilename != null && !"".equals(outputFilename)) {
            fileChooser.setCurrentDirectory(new File(outputFilename));
            fileChooser.setSelectedFile(new File(outputFilename));
        } else {
            if (this.bitcoinController.getModel().getActiveWalletFilename() != null) {
                fileChooser.setCurrentDirectory(new File(this.bitcoinController.getModel().getActiveWalletFilename()));
            }
            String defaultFileName = fileChooser.getCurrentDirectory().getAbsoluteFile() + File.separator
                    + controller.getLocaliser().getString("saveWalletAsView.untitled") + "."
                    + BitcoinModel.PRIVATE_KEY_FILE_EXTENSION;
            fileChooser.setSelectedFile(new File(defaultFileName));
        }

        int returnVal = fileChooser.showSaveDialog(mainFrame);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            if (file != null) {
                outputFilename = file.getAbsolutePath();

                // add a key suffix if not present
                if (!outputFilename.endsWith("." + BitcoinModel.PRIVATE_KEY_FILE_EXTENSION)) {
                    outputFilename = outputFilename + "." + BitcoinModel.PRIVATE_KEY_FILE_EXTENSION;
                }
                
                walletFilenameForChosenOutputFilename = this.bitcoinController.getModel().getActiveWalletFilename(); 

                outputFilenameLabel.setText(outputFilename);
                clearMessages();
            }
        }
    }

    // Used in testing.
    public void setOutputFilename(String outputFilename) {
        this.outputFilename = outputFilename;
    }

    public String getOutputFilename() {
        return outputFilename;
    }

    public void clearMessages() {
        setMessage1(" ");
        setMessage2(" ");
    }

    public void clearPasswords() {
        walletPasswordField.setText("");
        exportFilePasswordField.setText("");
        repeatExportFilePasswordField.setText("");
    }

    public void setMessage1(String message1) {
        if (messageLabel1 != null) {
            messageLabel1.setText(message1);
        }
    }

    public void setMessage2(String message2) {
        if (messageLabel2 != null) {
            messageLabel2.setText(message2);
        }
    }

    private String createDefaultKeyFilename(String walletFilename) {
        if (walletFilename == null) {
            return null;
        }
        
        int suffixSeparator = walletFilename.lastIndexOf('.');
        String stem = walletFilename.substring(0, suffixSeparator + 1);
        return stem + BitcoinModel.PRIVATE_KEY_FILE_EXTENSION;
    }

    class ChangePasswordProtectListener implements ItemListener {
        public ChangePasswordProtectListener() {

        }

        @Override
        public void itemStateChanged(ItemEvent e) {
            if (doNotPasswordProtectWarningLabel != null) {
                if (e.getSource().equals(passwordProtect)) {
                    doNotPasswordProtectWarningLabel.setText(" ");
                    exportFilePasswordField.setEnabled(true);
                    repeatExportFilePasswordField.setEnabled(true);
                    tickLabel.setEnabled(true);
                    exportFilePasswordField.requestFocusInWindow();
                    clearMessages();
                } else {
                    doNotPasswordProtectWarningLabel.setText(controller.getLocaliser().getString(
                            "showExportPrivateKeysPanel.doNotPasswordProtectWarningLabel"));
                    exportFilePasswordField.setEnabled(false);
                    repeatExportFilePasswordField.setEnabled(false);
                    tickLabel.setEnabled(false);
                    clearMessages();
                }
            }
        }
    }

    class PasswordListener implements KeyListener {
        /** Handle the key typed event from the text field. */
        @Override
        public void keyTyped(KeyEvent e) {
        }

        /** Handle the key-pressed event from the text field. */
        @Override
        public void keyPressed(KeyEvent e) {
            // do nothing
        }

        /** Handle the key-released event from the text field. */
        @Override
        public void keyReleased(KeyEvent e) {
            char[] password1 = null;
            char[] password2 = null;

            if (exportFilePasswordField != null) {
                password1 = exportFilePasswordField.getPassword();
            }
            if (repeatExportFilePasswordField != null) {
                password2 = repeatExportFilePasswordField.getPassword();
            }

            boolean tickLabelVisible = false;
            if (password1 != null && password2 != null) {
                if (Arrays.equals(password1, password2)) {
                    tickLabelVisible = true;
                }
            }
            tickLabel.setVisible(tickLabelVisible);

            clearMessages();

            // clear the password arrays (if necessary)
            if (password1 != null) {
                for (int i = 0; i < password1.length; i++) {
                    password1[i] = 0;
                }
            }
            if (password2 != null) {
                for (int i = 0; i < password2.length; i++) {
                    password2[i] = 0;
                }
            }
        }
    }

    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.EXPORT_PRIVATE_KEYS_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showExportPrivateKeysAction.text");
    }

    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip");
    }

    @Override
    public View getViewId() {
        return View.SHOW_EXPORT_PRIVATE_KEYS_VIEW;
    }

    // Used in testing.
    
    public ExportPrivateKeysSubmitAction getExportPrivateKeySubmitAction() {
        return exportPrivateKeySubmitAction;
    }

    public String getMessageText1() {
        return messageLabel1.getText();
    }

    public String getMessageText2() {
        return messageLabel2.getText();
    }
    
    public void setWalletPassword(CharSequence walletPassword) {
        walletPasswordField.setText(walletPassword.toString());
    }

    public boolean isWalletPasswordFieldEnabled() {
        return walletPasswordField.isEnabled();
    }
    
    public void setExportPassword(CharSequence exportPassword) {
        exportFilePasswordField.setText(exportPassword.toString());
    }
    
    public void setRepeatExportPassword(CharSequence exportPassword) {
        repeatExportFilePasswordField.setText(exportPassword.toString());
    }

    public JRadioButton getDoNotPasswordProtect() {
        return doNotPasswordProtect;
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {       
        // Update the enable status of the action to match the wallet busy status.
        if (this.bitcoinController.getModel().getActivePerWalletModelData().isBusy()) {
            // Wallet is busy with another operation that may change the private keys - Action is disabled.
            exportPrivateKeySubmitAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("multiBitSubmitAction.walletIsBusy", 
                    new Object[]{controller.getLocaliser().getString(this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey())})));
            exportPrivateKeySubmitAction.setEnabled(false);           
        } else {
            // Enable
            exportPrivateKeySubmitAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showExportPrivateKeysAction.tooltip")));
            exportPrivateKeySubmitAction.setEnabled(true);
        }
    }
  
    private void setFileChooserFont(Component[] components) {
        for (Component component : components) {
            if (component instanceof Container)
                setFileChooserFont(((Container) component).getComponents());
            try {
                component.setFont(adjustedFont);
            } catch (Exception e) {
            } // TODO Why there is an empty catch block here?
        }
    }
}