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
package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.util.Arrays;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ExportPrivateKeysSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitleLabel;

/**
 * The export private keys view
 */
public class ExportPrivateKeysPanel extends JPanel implements View {

    private static final long serialVersionUID = 444992298119957705L;

    private MultiBitController controller;

    private MultiBitFrame mainFrame;

    private MultiBitLabel walletFilenameLabel;

    private MultiBitLabel walletDescriptionLabel;

    private JFileChooser fileChooser;

    private MultiBitLabel outputFilenameLabel;

    private MultiBitLabel messageLabel1;
    private MultiBitLabel messageLabel2;

    private String outputFilename;

    private JRadioButton passwordProtect;
    private JRadioButton doNotPasswordProtect;
    private MultiBitLabel doNotPasswordProtectWarningLabel;

    private JPasswordField passwordField;
    private JPasswordField repeatPasswordField;

    private JLabel tickLabel;
    
    /**
     * Creates a new {@link ExportPrivateKeysPanel}.
     */
    public ExportPrivateKeysPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker())));
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        this.controller = controller;

        outputFilename = "";

        initUI();
    }

    public void displayView() {
        walletFilenameLabel.setText(controller.getModel().getActiveWalletFilename());
        walletDescriptionLabel.setText(controller.getModel().getActivePerWalletModelData().getWalletDescription());

        if (outputFilename == null || "".equals(outputFilename)) {
            outputFilename = createDefaultKeyFilename(controller.getModel().getActiveWalletFilename());
            outputFilenameLabel.setText(outputFilename);
        }

        clearMessages();
    }

    @Override
    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    private void initUI() {
        setLayout(new BorderLayout());
        
        JPanel mainPanel = new JPanel();
        mainPanel.setMinimumSize(new Dimension(550, 160));
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setOpaque(false);
        
        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel1 = new JPanel();
        fillerPanel1.setOpaque(false);
        mainPanel.add(fillerPanel1, constraints);

        MultiBitTitleLabel titleLabel = new MultiBitTitleLabel(controller.getLocaliser().getString("showExportPrivateKeysAction.text"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1.8;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createWalletPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createFilenamePanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createPasswordPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(createButtonPanel(), constraints);

        messageLabel1 = new MultiBitLabel("", controller);
        messageLabel1.setOpaque(false);
        messageLabel1.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(messageLabel1, constraints);

        messageLabel2 = new MultiBitLabel("", controller);
        messageLabel2.setOpaque(false);
        messageLabel2.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 7;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(messageLabel2, constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 8;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(filler1, constraints);
        
        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        mainScrollPane.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, ColorAndFontConstants.DARK_BACKGROUND_COLOR));
        mainScrollPane.setOpaque(true);
        mainScrollPane.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        add(mainScrollPane, BorderLayout.CENTER);
    }

    private JPanel createWalletPanel() {
        JPanel inputWalletPanel = new JPanel(new GridBagLayout());
        TitledBorder titledBorder = BorderFactory.createTitledBorder(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.wallet.title"));
        inputWalletPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0), titledBorder));
        inputWalletPanel.setOpaque(false);

        titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitLabel explainLabel1 = new MultiBitLabel(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.wallet.text"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(explainLabel1, constraints);

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(filler1, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        inputWalletPanel.add(filler2, constraints);

        MultiBitLabel walletFilenameLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletFilenameLabel"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(walletFilenameLabelLabel, constraints);

        walletFilenameLabel = new MultiBitLabel(controller.getModel().getActiveWalletFilename(), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(walletFilenameLabel, constraints);

        MultiBitLabel walletDescriptionLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletDescriptionLabel"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        inputWalletPanel.add(walletDescriptionLabelLabel, constraints);

        walletDescriptionLabel = new MultiBitLabel(controller.getModel().getActivePerWalletModelData().getWalletDescription(),
                controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(walletDescriptionLabel, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        inputWalletPanel.add(filler3, constraints);

        return inputWalletPanel;
    }

    private JPanel createFilenamePanel() {
        JPanel outputFilenamePanel = new JPanel(new GridBagLayout());
        TitledBorder titledBorder = BorderFactory.createTitledBorder(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.filename.title"));
        outputFilenamePanel
                .setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0), titledBorder));
        outputFilenamePanel.setOpaque(false);

        titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler1, constraints);

        MultiBitButton chooseOutputFilenameButton = new MultiBitButton(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.filename.text"));

        chooseOutputFilenameButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                chooseFile();
            }
        });
        chooseOutputFilenameButton.setToolTipText(controller.getLocaliser().getString("showExportPrivateKeysPanel.filename.tooltip"));

        MultiBitLabel walletFilenameLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletFilenameLabel"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        outputFilenamePanel.add(walletFilenameLabelLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler2, constraints);

        outputFilenameLabel = new MultiBitLabel(outputFilename, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(outputFilenameLabel, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler3, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(chooseOutputFilenameButton, constraints);

        JPanel filler4 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler4, constraints);

        return outputFilenamePanel;
    }

    private JPanel createPasswordPanel() {
        // do/do not password protect radios
        JPanel passwordProtectPanel = new JPanel(new GridBagLayout());
        TitledBorder titledBorder = BorderFactory.createTitledBorder(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.password.title"));
        titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        Border border = BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0), titledBorder);

        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        int minimumHeight = fontMetrics.getHeight() * 5 + 80;
        int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), fontMetrics.stringWidth(controller.getLocaliser().getString(
                            "showExportPrivateKeysPanel.doNotPasswordProtectWarningLabel"))) + 60;
        passwordProtectPanel.setMinimumSize(new Dimension(minimumWidth, minimumHeight));
 
        passwordProtectPanel.setBorder(border);
        passwordProtectPanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel();
        filler1.setMinimumSize(new Dimension(25, 1));
        filler1.setMaximumSize(new Dimension(25, 1));
        filler1.setPreferredSize(new Dimension(25, 1));
        filler1.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler1, constraints);

        ButtonGroup usePasswordGroup = new ButtonGroup();
        passwordProtect = new JRadioButton(controller.getLocaliser().getString("showExportPrivateKeysPanel.passwordProtect"));
        passwordProtect.setOpaque(false);
        passwordProtect.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        doNotPasswordProtect = new JRadioButton(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.doNotPasswordProtect"));
        doNotPasswordProtect.setOpaque(false);
        doNotPasswordProtect.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        ItemListener itemListener = new ChangePasswordProtectListener();
        passwordProtect.addItemListener(itemListener);
        doNotPasswordProtect.addItemListener(itemListener);
        usePasswordGroup.add(passwordProtect);
        usePasswordGroup.add(doNotPasswordProtect);
        passwordProtect.setSelected(true);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(passwordProtect, constraints);

        MultiBitLabel passwordPromptLabel = new MultiBitLabel("", controller);
        passwordPromptLabel.setText(controller.getLocaliser().getString("showExportPrivateKeysPanel.passwordPrompt"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        passwordProtectPanel.add(passwordPromptLabel, constraints);

        passwordField = new JPasswordField(30);
        passwordField.setMinimumSize(new Dimension(250, 20));
        passwordField.addKeyListener(new PasswordListener());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.25;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(passwordField, constraints);

        JLabel filler2 = new JLabel();
        filler2.setMinimumSize(new Dimension(3, 3));
        filler2.setMaximumSize(new Dimension(3, 3));
        filler2.setPreferredSize(new Dimension(3, 3));
        filler2.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler2, constraints);

        MultiBitLabel repeatPasswordPromptLabel = new MultiBitLabel("", controller);
        repeatPasswordPromptLabel.setText(controller.getLocaliser().getString("showExportPrivateKeysPanel.repeatPasswordPrompt"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        passwordProtectPanel.add(repeatPasswordPromptLabel, constraints);

        repeatPasswordField = new JPasswordField(30);
        repeatPasswordField.setMinimumSize(new Dimension(250, 20));
        repeatPasswordField.addKeyListener(new PasswordListener());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.25;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(repeatPasswordField, constraints);

        JLabel filler3 = new JLabel();
        filler3.setMinimumSize(new Dimension(20, 1));
        filler3.setMaximumSize(new Dimension(20, 1));
        filler3.setPreferredSize(new Dimension(20, 1));
        filler3.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler3, constraints);

        ImageIcon tickIcon = ImageLoader.createImageIcon(ImageLoader.TICK_ICON_FILE);
        tickLabel = new JLabel(tickIcon);
        tickLabel.setToolTipText(controller.getLocaliser().getString("showExportPrivateKeysPanel.theTwoPasswordsMatch"));
        tickLabel.setVisible(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 3;
        constraints.gridy = 2;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(tickLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(doNotPasswordProtect, constraints);

        doNotPasswordProtectWarningLabel = new MultiBitLabel(" ", controller);
        doNotPasswordProtectWarningLabel.setForeground(Color.RED);
        doNotPasswordProtectWarningLabel.setBorder(BorderFactory.createEmptyBorder(0, 40, 0, 0));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 6;
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
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        /**
         * Create submit action with references to the password fields
         * - this avoids having any public accessors on the panel
         */
        ExportPrivateKeysSubmitAction submitAction = new ExportPrivateKeysSubmitAction(controller, this,
                ImageLoader.createImageIcon(ImageLoader.EXPORT_PRIVATE_KEYS_ICON_FILE), passwordField, repeatPasswordField);
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

    @Override
    public void updateView() {
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
        fileChooser = new JFileChooser();
        fileChooser.setLocale(controller.getLocaliser().getLocale());
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(new PrivateKeyFileFilter(controller));

        if (outputFilename != null && !"".equals(outputFilename)) {
            fileChooser.setCurrentDirectory(new File(outputFilename));
            fileChooser.setSelectedFile(new File(outputFilename));
        } else {
            if (controller.getModel().getActiveWalletFilename() != null) {
                fileChooser.setCurrentDirectory(new File(controller.getModel().getActiveWalletFilename()));
            }
            String defaultFileName = fileChooser.getCurrentDirectory().getAbsoluteFile() + File.separator
                    + controller.getLocaliser().getString("saveWalletAsView.untitled") + "."
                    + MultiBitModel.PRIVATE_KEY_FILE_EXTENSION;
            fileChooser.setSelectedFile(new File(defaultFileName));
        }

        int returnVal = fileChooser.showSaveDialog(mainFrame);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            if (file != null) {
                outputFilename = file.getAbsolutePath();

                // add a key suffix if not present
                if (!outputFilename.endsWith("." + MultiBitModel.PRIVATE_KEY_FILE_EXTENSION)) {
                    outputFilename = outputFilename + "." + MultiBitModel.PRIVATE_KEY_FILE_EXTENSION;
                }
                outputFilenameLabel.setText(outputFilename);
                clearMessages();
            }
        }
    }

    public String getOutputFilename() {
        return outputFilename;
    }

    public void clearMessages() {
        setMessage1(" ");
        setMessage2(" ");
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
        // find suffix
        int suffixSeparator = walletFilename.lastIndexOf(".");
        String stem = walletFilename.substring(0, suffixSeparator + 1);
        String defaultKeyFilename = stem + MultiBitModel.PRIVATE_KEY_FILE_EXTENSION;
        return defaultKeyFilename;
    }

    class ChangePasswordProtectListener implements ItemListener {
        public ChangePasswordProtectListener() {

        }

        public void itemStateChanged(ItemEvent e) {
            if (doNotPasswordProtectWarningLabel != null) {
                if (e.getSource().equals(passwordProtect)) {
                    doNotPasswordProtectWarningLabel.setText(" ");
                    passwordField.setEnabled(true);
                    repeatPasswordField.setEnabled(true);
                    tickLabel.setEnabled(true);
                    passwordField.requestFocusInWindow();
                    clearMessages();
                } else {
                    doNotPasswordProtectWarningLabel.setText(controller.getLocaliser().getString(
                            "showExportPrivateKeysPanel.doNotPasswordProtectWarningLabel"));
                    passwordField.setEnabled(false);
                    repeatPasswordField.setEnabled(false);
                    tickLabel.setEnabled(false);
                    clearMessages();
                }
            }
        }
    }

    class PasswordListener implements KeyListener {
        /** Handle the key typed event from the text field. */
        public void keyTyped(KeyEvent e) {
        }

        /** Handle the key-pressed event from the text field. */
        public void keyPressed(KeyEvent e) {
            // do nothing
        }

        /** Handle the key-released event from the text field. */
        public void keyReleased(KeyEvent e) {
            char[] password1 = null;
            char[] password2 = null;

            if (passwordField != null) {
                password1 = passwordField.getPassword();
            }
            if (repeatPasswordField != null) {
                password2 = repeatPasswordField.getPassword();
            }

            boolean tickLabelVisible = false;
            if (password1 != null && password2 != null) {
                if (Arrays.equals(password1, password2)) {
                    tickLabelVisible = true;
                }
            }
            tickLabel.setVisible(tickLabelVisible);

            clearMessages();

            // clear the password arrays
            for (int i = 0; i < password1.length; i++) {
                password1[i] = 0;
            }

            for (int i = 0; i < password2.length; i++) {
                password2[i] = 0;
            }
        }
    }

}