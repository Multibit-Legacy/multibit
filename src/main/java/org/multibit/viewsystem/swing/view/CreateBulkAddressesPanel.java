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
import java.io.File;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.dataproviders.CreateBulkAddressesDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CreateBulkAddressesSubmitAction;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

/**
 * The create bulk addresses view (MultiBitMerchant support)
 */
public class CreateBulkAddressesPanel extends JPanel implements View, CreateBulkAddressesDataProvider {

    private static final String DEFAULT_BULK_ADDRESSES_FILENAME = "bulkAddresses.csv";

    private static final int DEFAULT_NUMBER_OF_ADDRESSES = 1000;
    
    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;

    private MultiBitLabel actualNumberOfAddressesLabel;
    private MultiBitLabel actualFilenameLabel;

//    private Data data;

    /**
     * Creates a new {@link CreateBulkAddressesPanel}.
     */
    public CreateBulkAddressesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;

        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        this.controller = controller;

//        data = new Data();

        initUI();
    }

    @Override
    public void navigateAwayFromView() {
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));

        GridBagConstraints constraints = new GridBagConstraints();
        setLayout(new GridBagLayout());
        
        String[] keys = new String[] {"createBulkAddressesPanel.numberOfAddresses.text", "createBulkAddressesPanel.filenameLabel.text"};
        int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createNumberOfAddressesPanel(stentWidth), constraints);

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createFileNamePanel(stentWidth), constraints);

        constraints = new GridBagConstraints();        
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 100;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler1, constraints);
    }

    /**
     * panel to ask user for outputfile name - currently hardwired
     * @return
     */
    private JPanel createFileNamePanel(int stentWidth) {
        MultiBitTitledPanel filenamePanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "createBulkAddressesPanel.filename.title"));
        
        filenamePanel.setOpaque(false);
        filenamePanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        GridBagConstraints constraints2 = new GridBagConstraints();

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 3;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        filenamePanel.add(MultiBitTitledPanel.getIndentPanel(1), constraints2);
        
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 1;
        constraints2.gridy = 3;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        filenamePanel.add(MultiBitTitledPanel.createStent(stentWidth), constraints2);

        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 2;
        constraints2.gridy = 3;
        constraints2.weightx = 0.05;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.CENTER;
        filenamePanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints2);

        MultiBitLabel filenameLabel = new MultiBitLabel(controller.getLocaliser().getString("createBulkAddressesPanel.filenameLabel.text"));
        filenameLabel.setToolTipText(controller.getLocaliser().getString("createBulkAddressesPanel.filenameLabel.tooltip"));
        filenameLabel.setOpaque(false);
        
        // work out default output filename
        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = controller.getApplicationDataDirectoryLocator();
        String applicationDataDirectory = applicationDataDirectoryLocator.getApplicationDataDirectory();
        
        String defaultOutputFilename = applicationDataDirectory + File.separator + DEFAULT_BULK_ADDRESSES_FILENAME;

        actualFilenameLabel = new MultiBitLabel(defaultOutputFilename);
        actualFilenameLabel.setOpaque(false);

        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 1;
        constraints2.gridy = 5;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.3;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        filenamePanel.add(filenameLabel, constraints2);

        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 3;
        constraints2.gridy = 5;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.3;
        constraints2.anchor = GridBagConstraints.LINE_START;
        filenamePanel.add(actualFilenameLabel, constraints2);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 4;
        constraints2.gridy = 5;
        constraints2.gridwidth = 1;
        constraints2.weightx = 20;
        constraints2.weighty = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        add(filler1, constraints2);

        return filenamePanel;
    }


    /**
     * panel to ask user for number of addresses - currently hardwired
     * @return
     */
    private JPanel createNumberOfAddressesPanel(int stentWidth) {
        MultiBitTitledPanel numberOfAddressesPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
        "createBulkAddressesPanel.numberOfAddresses.title"));
        
        numberOfAddressesPanel.setOpaque(false);
        numberOfAddressesPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(MultiBitTitledPanel.getIndentPanel(1), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(MultiBitTitledPanel.createStent(stentWidth), constraints);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        numberOfAddressesPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

        MultiBitLabel numberOfAddressesLabel = new MultiBitLabel(controller.getLocaliser().getString("createBulkAddressesPanel.numberOfAddresses.text"));
        numberOfAddressesLabel.setToolTipText(controller.getLocaliser().getString("createBulkAddressesPanel.numberOfAddresses.tooltip"));
        numberOfAddressesLabel.setOpaque(false);
 
        actualNumberOfAddressesLabel = new MultiBitLabel("" + DEFAULT_NUMBER_OF_ADDRESSES);
        actualNumberOfAddressesLabel.setOpaque(false);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        numberOfAddressesPanel.add(numberOfAddressesLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(actualNumberOfAddressesLabel, constraints);

        JPanel filler5 = new JPanel();
        filler5.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 4;
        constraints.gridy = 3;
        constraints.weightx = 20;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        numberOfAddressesPanel.add(filler5, constraints);

        return numberOfAddressesPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(flowLayout);

        CreateBulkAddressesSubmitAction submitAction = new CreateBulkAddressesSubmitAction(controller, this);
        MultiBitButton submitButton = new MultiBitButton(submitAction, controller);
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

//    public Data getData() {
//        data = new Data();
//
//        Item outputFilenameItem = new Item(MultiBitModel.MERCHANT_BULK_ADDRESSES_OUTPUT_FILENAME);
//        outputFilenameItem.setNewValue(actualFilenameLabel.getText());
//        data.addItem(MultiBitModel.MERCHANT_BULK_ADDRESSES_OUTPUT_FILENAME, outputFilenameItem);
//
//        Item numberOfAddressesItem = new Item(MultiBitModel.MERCHANT_BULK_ADDRESSES_NUMBER_OF_ADDRESSES);
//        numberOfAddressesItem.setNewValue(actualNumberOfAddressesLabel.getText());
//        data.addItem(MultiBitModel.MERCHANT_BULK_ADDRESSES_NUMBER_OF_ADDRESSES, numberOfAddressesItem);
//        return data;
//    }

    @Override
    public void displayView() {
        this.removeAll();
        initUI();
    }

    @Override
    public Icon getViewIcon() {
        return null;
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showCreateBulkAddressesAction.text");
    }

    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showCreateBulkAddressesAction.tooltip");
    }

    @Override
    public int getViewId() {
        return View.CREATE_BULK_ADDRESSES_VIEW;
    }

    // CreateBulkAddressesDataProvider methods
    
    @Override
    public String getOutputFilename() {
        return actualFilenameLabel.getText();
    }

    @Override
    public int getNumberOfAddresses() {
        return Integer.parseInt(actualNumberOfAddressesLabel.getText());
    }

    public String getPreviousCurrency2() {
        return null;
    }

    public String getPreviousExchange2() {
        return null;
    }

    public boolean getPreviousShowSecondRow() {
        return false;
    }

    public String getPreviousCurrency1() {
        return null;
    }
}