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
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CreateBulkAddressesSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitleLabel;

/**
 * The create bulk addresses view (MultiBitMerchant support)
 */
public class CreateBulkAddressesPanel extends JPanel implements View, DataProvider {

    private static final int DIALOG_HEIGHT_DELTA = 20;

    private static final int DIALOG_WIDTH_DELTA = 100;

    private static final String DEFAULT_BULK_ADDRESSES_FILENAME = "bulkAddresses.csv";

    private static final int DEFAULT_NUMBER_OF_ADDRESSES = 1000;
    
    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;

    private MultiBitLabel actualNumberOfAddressesLabel;
    private MultiBitLabel actualFilenameLabel;

    private Data data;

    /**
     * Creates a new {@link CreateBulkAddressesPanel}.
     */
    public CreateBulkAddressesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker())));
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        initUI();
    }

    @Override
    public void displayView() {
    }

    @Override
    public void navigateAwayFromView() {
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

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(createNumberOfAddressesPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(createFileNamePanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(filler1, constraints);
    }

    /**
     * panel to ask user for outputfile name - currently hardwired
     * @return
     */
    private JPanel createFileNamePanel() {
        JPanel filenamePanel = new JPanel(new GridBagLayout());
        TitledBorder titledBorder = BorderFactory.createTitledBorder(controller.getLocaliser().getString("createBulkAddressesPanel.filename.title"));
        titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

        Border border = BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0), titledBorder);
        filenamePanel.setBorder(border);

        filenamePanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitLabel filenameLabel = new MultiBitLabel(controller.getLocaliser().getString("createBulkAddressesPanel.filenameLabel.text"), controller);
        filenameLabel.setToolTipText(controller.getLocaliser().getString("createBulkAddressesPanel.filenameLabel.tooltip"));
        filenameLabel.setOpaque(false);
        
        // work out default output filename
        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = controller.getApplicationDataDirectoryLocator();
        String applicationDataDirectory = applicationDataDirectoryLocator.getApplicationDataDirectory();
        
        String defaultOutputFilename = applicationDataDirectory + File.separator + DEFAULT_BULK_ADDRESSES_FILENAME;

        actualFilenameLabel = new MultiBitLabel(defaultOutputFilename, controller);
        actualFilenameLabel.setOpaque(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        filenamePanel.add(filenameLabel, constraints);

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        filenamePanel.add(filler1, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        filenamePanel.add(actualFilenameLabel, constraints);

        return filenamePanel;
    }


    /**
     * panel to ask user for number of addresses - currently hardwired
     * @return
     */
    private JPanel createNumberOfAddressesPanel() {
        JPanel numberOfAddressesPanel = new JPanel(new GridBagLayout());
        numberOfAddressesPanel.setOpaque(false);
        String titleText = controller.getLocaliser().getString("createBulkAddressesPanel.numberOfAddresses.title");
        TitledBorder titledBorder = BorderFactory.createTitledBorder(titleText);
        titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        Border border = BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0), titledBorder);
        numberOfAddressesPanel.setBorder(border);
        numberOfAddressesPanel.setOpaque(false);
 
        GridBagConstraints constraints = new GridBagConstraints();

        MultiBitLabel numberOfAddressesLabel = new MultiBitLabel(controller.getLocaliser().getString("createBulkAddressesPanel.numberOfAddresses.text"), controller);
        numberOfAddressesLabel.setToolTipText(controller.getLocaliser().getString("createBulkAddressesPanel.numberOfAddresses.tooltip"));
        numberOfAddressesLabel.setOpaque(false);
 
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        Dimension preferredSize = new Dimension(fontMetrics.stringWidth(titleText) + DIALOG_WIDTH_DELTA, fontMetrics.getHeight() * 2 + DIALOG_HEIGHT_DELTA);
        numberOfAddressesPanel.setMinimumSize(preferredSize);
        numberOfAddressesPanel.setPreferredSize(preferredSize);

        actualNumberOfAddressesLabel = new MultiBitLabel("" + DEFAULT_NUMBER_OF_ADDRESSES, controller);
        actualNumberOfAddressesLabel.setOpaque(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(numberOfAddressesLabel, constraints);

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(filler1, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(actualNumberOfAddressesLabel, constraints);

        return numberOfAddressesPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CreateBulkAddressesSubmitAction submitAction = new CreateBulkAddressesSubmitAction(controller, this);
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

    public Data getData() {
        data = new Data();

        Item outputFilenameItem = new Item(MultiBitModel.MERCHANT_BULK_ADDRESSES_OUTPUT_FILENAME);
        outputFilenameItem.setNewValue(actualFilenameLabel.getText());
        data.addItem(MultiBitModel.MERCHANT_BULK_ADDRESSES_OUTPUT_FILENAME, outputFilenameItem);

        Item numberOfAddressesItem = new Item(MultiBitModel.MERCHANT_BULK_ADDRESSES_NUMBER_OF_ADDRESSES);
        numberOfAddressesItem.setNewValue(actualNumberOfAddressesLabel.getText());
        data.addItem(MultiBitModel.MERCHANT_BULK_ADDRESSES_NUMBER_OF_ADDRESSES, numberOfAddressesItem);
        return data;
    }

    @Override
    public void updateView() {    
    }

    @Override
    public Icon getViewIcon() {
        return null;
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showCreateBulkAddressesAction.text");
    }
}