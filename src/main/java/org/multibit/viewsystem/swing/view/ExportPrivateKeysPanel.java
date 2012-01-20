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

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ExportPrivateKeysSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;

/**
 * The export private keys view
 */
public class ExportPrivateKeysPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 444992298119957705L;

    private MultiBitController controller;

    private MultiBitFrame mainFrame;

    private Data data;

    private MultiBitLabel walletFilenameLabel;

    private MultiBitLabel walletDescriptionLabel;

    private JFileChooser fileChooser;

    private MultiBitLabel outputFilenameLabel;

    private MultiBitLabel messageLabel;

    private String outputFilename;

    /**
     * Creates a new {@link ExportPrivateKeysPanel}.
     */
    public ExportPrivateKeysPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker())));
        setBackground(MultiBitFrame.VERY_LIGHT_BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        outputFilename = "";
        
        // clear any old message info
        controller.getModel().setUserPreference(MultiBitModel.DISPLAY_EXPORT_PRIVATE_KEYS_MESSAGE, "false");
        controller.getModel().setUserPreference(MultiBitModel.EXPORT_PRIVATE_KEYS_MESSAGE, " ");

        initUI();
    }

    public void displayView() {
        walletFilenameLabel.setText(controller.getModel().getActiveWalletFilename());
        walletDescriptionLabel.setText(controller.getModel().getActivePerWalletModelData().getWalletDescription());

        if (outputFilename == null || "".equals(outputFilename)) {
            outputFilename = createDefaultKeyFilename(controller.getModel().getActiveWalletFilename());
            outputFilenameLabel.setText(outputFilename);
        }
        
        if (Boolean.TRUE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.DISPLAY_EXPORT_PRIVATE_KEYS_MESSAGE))) {
            messageLabel.setText("  " + controller.getModel().getUserPreference(MultiBitModel.EXPORT_PRIVATE_KEYS_MESSAGE));
            controller.getModel().setUserPreference(MultiBitModel.DISPLAY_EXPORT_PRIVATE_KEYS_MESSAGE, "false");
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

        MultiBitLabel titleLabel = new MultiBitLabel("", controller);
        titleLabel.setHorizontalTextPosition(JLabel.LEFT);
        titleLabel.setText(controller.getLocaliser().getString("showExportPrivateKeysAction.text"));

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1.8;
        constraints.weighty = 0.06;
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
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        messageLabel = new MultiBitLabel("", controller);
        messageLabel.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(messageLabel, constraints);
        
        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.CENTER;
        add(filler1, constraints);

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

        JButton chooseOutputFilenameButton = new JButton(controller.getLocaliser().getString(
                "showExportPrivateKeysPanel.filename.text"));

        chooseOutputFilenameButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent arg0) {
                chooseFile();
            }
        });

        outputFilenameLabel = new MultiBitLabel(outputFilename, controller);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
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
        outputFilenamePanel.add(chooseOutputFilenameButton, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        outputFilenamePanel.add(filler3, constraints);

        return outputFilenamePanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        ExportPrivateKeysSubmitAction submitAction = new ExportPrivateKeysSubmitAction(controller, this, MultiBitFrame.createImageIcon(MultiBitFrame.EXPORT_PRIVATE_KEYS_ICON_FILE));
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

    public JPanel getFormPanel() {
        return null;
    }

    public JTextField getLabelTextField() {
        return null;
    }

    @Override
    public void updateView() {
        // TODO Auto-generated method stub

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
            }
        }
    }

    private String createDefaultKeyFilename(String walletFilename) {
        // find suffix
        int suffixSeparator = walletFilename.lastIndexOf(".");
        String stem = walletFilename.substring(0, suffixSeparator + 1);
        String defaultKeyFilename = stem + MultiBitModel.PRIVATE_KEY_FILE_EXTENSION;
        return defaultKeyFilename;
    }
}