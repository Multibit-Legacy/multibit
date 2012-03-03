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
package org.multibit.viewsystem.swing.view;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.text.DateFormat;
import java.util.Collection;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.apache.commons.codec.binary.Base64;
import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncrypterDecrypter;
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.file.PrivateKeyAndDate;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ImportPrivateKeysSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitleLabel;

/**
 * The import private keys view
 */
public class ImportPrivateKeysPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 444992294329957705L;

    private MultiBitController controller;

    private MultiBitFrame mainFrame;

    private Data data;

    private MultiBitLabel walletFilenameLabel;

    private MultiBitLabel walletDescriptionLabel;

    private MultiBitButton chooseFilenameButton;

    private JFileChooser fileChooser;

    private MultiBitLabel outputFilenameLabel;

    private MultiBitLabel messageLabel;

    private String outputFilename;

    private ImportPrivateKeysPanel thisPanel;

    private MultiBitLabel passwordInfoLabel;
    private JPasswordField passwordField;
    private MultiBitLabel passwordPromptLabel;
    private MultiBitButton unlockButton;

    private String openSSLMagicText;

    private boolean importFileIsEncrypted = false;

    private JLabel numberOfKeysLabel;
    private JLabel dateOfEarliestKeyLabel;

    /**
     * Creates a new {@link ImportPrivateKeysPanel}.
     */
    public ImportPrivateKeysPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        thisPanel = this;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker())));
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        outputFilename = "";

        // clear any old message info
        controller.getModel().setUserPreference(MultiBitModel.DISPLAY_IMPORT_PRIVATE_KEYS_MESSAGE, "false");
        controller.getModel().setUserPreference(MultiBitModel.IMPORT_PRIVATE_KEYS_MESSAGE, " ");

        try {
            openSSLMagicText = Base64.encodeBase64String(
                    EncrypterDecrypter.OPENSSL_SALTED_TEXT.getBytes(EncrypterDecrypter.STRING_ENCODING)).substring(0,
                    EncrypterDecrypter.NUMBER_OF_CHARACTERS_TO_MATCH_IN_OPENSSL_MAGIC_TEXT);
        } catch (UnsupportedEncodingException e) {
            // ignore the unsupported encoding error
        }
        initUI();
    }

    public void displayView() {
        walletFilenameLabel.setText(controller.getModel().getActiveWalletFilename());
        walletDescriptionLabel.setText(controller.getModel().getActivePerWalletModelData().getWalletDescription());

        if (outputFilename == null || "".equals(outputFilename)) {
            outputFilenameLabel.setText(controller.getLocaliser().getString("showImportPrivateKeysPanel.noFileSelected"));
            passwordInfoLabel.setText(controller.getLocaliser().getString("showImportPrivateKeysPanel.noFileSelected"));
        }

        if (Boolean.TRUE.toString().equals(
                controller.getModel().getUserPreference(MultiBitModel.DISPLAY_IMPORT_PRIVATE_KEYS_MESSAGE))) {
            messageLabel.setText("  " + controller.getModel().getUserPreference(MultiBitModel.IMPORT_PRIVATE_KEYS_MESSAGE));
            controller.getModel().setUserPreference(MultiBitModel.DISPLAY_IMPORT_PRIVATE_KEYS_MESSAGE, "false");
        } else {
            messageLabel.setText(" ");
        }
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));

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

        MultiBitTitleLabel titleLabel = new MultiBitTitleLabel(controller.getLocaliser().getString(
                "showImportPrivateKeysAction.text"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1.8;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createWalletPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createFilenamePanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.2;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createPasswordPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        messageLabel = new MultiBitLabel("", controller);
        messageLabel.setOpaque(false);
        messageLabel.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(messageLabel, constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 7;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.CENTER;
        add(filler1, constraints);

    }

    private JPanel createWalletPanel() {
        JPanel inputWalletPanel = new JPanel(new GridBagLayout());
        TitledBorder titledBorder = BorderFactory.createTitledBorder(controller.getLocaliser().getString(
                "showImportPrivateKeysPanel.wallet.title"));
        inputWalletPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0), titledBorder));
        inputWalletPanel.setOpaque(false);

        titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitLabel explainLabel1 = new MultiBitLabel(controller.getLocaliser().getString(
                "showImportPrivateKeysPanel.wallet.text"), controller);
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
                "showImportPrivateKeysPanel.filename.title"));
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

        chooseFilenameButton = new MultiBitButton(controller.getLocaliser().getString("showImportPrivateKeysPanel.filename.text"));

        final MultiBitButton finalChooseFilenameButton = chooseFilenameButton;
        chooseFilenameButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                chooseFile(finalChooseFilenameButton);
            }
        });

        outputFilenameLabel = new MultiBitLabel(outputFilename, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(outputFilenameLabel, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler2, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(chooseFilenameButton, constraints);

        JLabel descriptionLabel = new MultiBitLabel(controller.getLocaliser().getString("showImportPrivateKeysPanel.details"),
                controller);
        descriptionLabel.setBorder(BorderFactory.createEmptyBorder(4, 0, 0, 0));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(descriptionLabel, constraints);

        MultiBitLabel numberOfKeysLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "showImportPrivateKeysPanel.numberOfKeys"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        outputFilenamePanel.add(numberOfKeysLabelLabel, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler3, constraints);

        numberOfKeysLabel = new MultiBitLabel(" ", controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 5;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(numberOfKeysLabel, constraints);

        MultiBitLabel dateOfEarliestKeyLabelLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "showImportPrivateKeysPanel.dateOfEarliestKey"), controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        outputFilenamePanel.add(dateOfEarliestKeyLabelLabel, constraints);

        dateOfEarliestKeyLabel = new MultiBitLabel(" ", controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 6;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(dateOfEarliestKeyLabel, constraints);

        JPanel filler4 = new JPanel();
        filler4.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 7;
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

        // FontMetrics fontMetrics =
        // getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        // int minimumHeight = fontMetrics.getHeight() * 5 + 80;
        // int minimumWidth =
        // fontMetrics.stringWidth(controller.getLocaliser().getString(
        // "showExportPrivateKeysPanel.doNotPasswordProtectWarningLabel")) + 60;
        // passwordProtectPanel.setMinimumSize(new Dimension(minimumWidth,
        // minimumHeight));
        passwordProtectPanel.setBorder(border);
        passwordProtectPanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler1 = new JLabel();
        filler1.setPreferredSize(new Dimension(120, 1));
        filler1.setOpaque(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 2;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler1, constraints);

        passwordInfoLabel = new MultiBitLabel("", controller);
        passwordInfoLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 4, 0));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(passwordInfoLabel, constraints);

        passwordPromptLabel = new MultiBitLabel(controller.getLocaliser().getString("showExportPrivateKeysPanel.passwordPrompt"),
                controller);
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
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.6;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(passwordField, constraints);

        JLabel filler3 = new JLabel();
        filler3.setMinimumSize(new Dimension(3, 3));
        filler3.setMaximumSize(new Dimension(3, 3));
        filler3.setPreferredSize(new Dimension(3, 3));
        filler3.setOpaque(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler3, constraints);

        unlockButton = new MultiBitButton(controller.getLocaliser().getString("showImportPrivateKeysPanel.unlock.text"));
        unlockButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                unlock();
                thisPanel.setMessage(" ");
            }
        });
        unlockButton.setEnabled(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.6;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        passwordProtectPanel.add(unlockButton, constraints);

        JLabel filler4 = new JLabel();
        filler4.setMinimumSize(new Dimension(3, 3));
        filler4.setMaximumSize(new Dimension(3, 3));
        filler4.setPreferredSize(new Dimension(3, 3));
        filler4.setOpaque(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        passwordProtectPanel.add(filler4, constraints);

        return passwordProtectPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        ImportPrivateKeysSubmitAction submitAction = new ImportPrivateKeysSubmitAction(controller, this,
                ImageLoader.createImageIcon(ImageLoader.IMPORT_PRIVATE_KEYS_ICON_FILE), passwordField);
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

    public Data getData() {
        Item privateKeyFilenameItem = new Item(MultiBitModel.PRIVATE_KEY_FILENAME);
        privateKeyFilenameItem.setNewValue(outputFilename);
        data.addItem(MultiBitModel.PRIVATE_KEY_FILENAME, privateKeyFilenameItem);

        return data;
    }

    @Override
    public void updateView() {
    }

    private void chooseFile(MultiBitButton callingButton) {
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

        callingButton.setEnabled(false);
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        fileChooser.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        int returnVal = fileChooser.showOpenDialog(mainFrame);
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        callingButton.setEnabled(true);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            numberOfKeysLabel.setText(" ");
            dateOfEarliestKeyLabel.setText(" ");
            
            File file = fileChooser.getSelectedFile();
            if (file != null) {
                outputFilename = file.getAbsolutePath();

                outputFilenameLabel.setText(outputFilename);

                try {
                    String firstLine = readFirstLineInFile(file);
                    char[] password = null;

                    if (firstLine != null && firstLine.startsWith(openSSLMagicText)) {
                        // file is encrypted
                        importFileIsEncrypted = true;
                        passwordPromptLabel.setEnabled(true);
                        passwordField.setEnabled(true);
                        passwordInfoLabel.setText(controller.getLocaliser().getString("showImportPrivateKeysPanel.enterPassword"));
                        unlockButton.setEnabled(true);
                        password = passwordField.getPassword();
                    } else {
                        // file is not encrypted
                        importFileIsEncrypted = false;
                        passwordPromptLabel.setEnabled(false);
                        passwordField.setEnabled(false);
                        passwordInfoLabel.setText(controller.getLocaliser().getString(
                                "showImportPrivateKeysPanel.importFileIsNotPasswordProtected"));
                        unlockButton.setEnabled(false);
                    }
                    
                    // update number of keys and earliest date
                    
                    // read in contents of file
                    PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getMultiBitService()
                            .getNetworkParameters());
                    Collection<PrivateKeyAndDate> privateKeyAndDates = privateKeysHandler.readInPrivateKeys(
                            new File(outputFilename), password);
                    numberOfKeysLabel.setText("" + privateKeyAndDates.size());

                    Date earliestKeyDate = calculateEarliestKeyDate(privateKeyAndDates);

                    if (earliestKeyDate == null) {
                        dateOfEarliestKeyLabel.setText(controller.getLocaliser().getString(
                                "showImportPrivateKeysPanel.thereWereMissingKeyDates"));
                    } else {
                        dateOfEarliestKeyLabel.setText(DateFormat.getDateInstance(DateFormat.MEDIUM,
                                controller.getLocaliser().getLocale()).format(earliestKeyDate));
                    }

                } catch (IOException e) {
                    setMessage(controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { e.getClass().getName() + " " + e.getMessage() }));
                } catch (EncrypterDecrypterException e) {
                    setMessage(controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { e.getClass().getName() + " " + e.getMessage() }));
                }
            }
        }
    }

    private Date calculateEarliestKeyDate(Collection<PrivateKeyAndDate> privateKeyAndDates) {
        boolean thereWereMissingDates = false;
        Date earliestKeyDate = null;
        for (PrivateKeyAndDate loop : privateKeyAndDates) {
            if (loop.getDate() == null) {
                thereWereMissingDates = true;
            } else {
                if (earliestKeyDate == null) {
                    earliestKeyDate = loop.getDate();
                } else {
                    earliestKeyDate = earliestKeyDate.before(loop.getDate()) ? earliestKeyDate : loop.getDate();
                }
            }
        }

        if (thereWereMissingDates) {
            return null;
        } else {
            return earliestKeyDate;
        }
    }

    /**
     * unlock the encrypted file and show the file details
     */
    private void unlock() {

    }

    public String getOutputFilename() {
        return outputFilename;
    }

    public void setMessage(String message) {
        if (messageLabel != null) {
            messageLabel.setText(message);
        }
    }

    private String readFirstLineInFile(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        return reader.readLine();
    }
}