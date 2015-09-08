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
package org.multibit.viewsystem.swing.view.dialogs;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.dataproviders.ShowUriDialogDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ShowOpenUriCancelAction;
import org.multibit.viewsystem.swing.action.ShowOpenUriSubmitAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;

/**
 * The show open uri view.
 */
public class ShowOpenUriDialog extends MultiBitDialog implements Viewable, ShowUriDialogDataProvider {

    private Logger log = LoggerFactory.getLogger(ShowOpenUriDialog.class);

    private static final long serialVersionUID = 191411112345057705L;

    private MultiBitFrame mainFrame;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private JButton submitButton;
    private JButton cancelButton;
    
    private JCheckBox rememberCheckBox;

    private Font adjustedFont;
    
    private int DEFAULT_WIDTH = 640;
    private int DEFAULT_HEIGHT = 200;
    private int WIDTH_DELTA = 100;
    private int HEIGHT_DELTA = 50;
    private int NUMBER_OF_ROWS = 4;

    /**
     * Creates a new {@link ShowOpenUriDialog}.
     */
    public ShowOpenUriDialog(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
        super(mainFrame, bitcoinController.getLocaliser().getString("showOpenUriView.title"));
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;
        
        setAlwaysOnTop(true);

        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }
    
        initUI();
        
        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        log.debug("Constructor called for ShowOpenUriDialog " + this.toString());
    }

    /**
     * show open uri view
     */
    private void initUI() {
        adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
        int width = DEFAULT_WIDTH;
        int height = DEFAULT_HEIGHT;
        
        if (adjustedFont != null) {
            FontMetrics fontMetrics = this.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
            int message1Width = fontMetrics.stringWidth(controller.getLocaliser().getString("showOpenUriView.message1"));
            int message2Width = fontMetrics.stringWidth(controller.getLocaliser().getString("showOpenUriView.message2"));
            width = Math.max(Math.max(message1Width, message2Width) + WIDTH_DELTA, width);
            height = Math.max(height, NUMBER_OF_ROWS * fontMetrics.getHeight() + HEIGHT_DELTA);
        }
        setMinimumSize(new Dimension(width, height));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);

        setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel filler00 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler00, constraints);

        JLabel filler01 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler01, constraints);
       
        ImageIcon bigIcon = ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 3;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel bigIconLabel = new JLabel(bigIcon);
        add(bigIconLabel, constraints);

        JLabel messageLabel1 = new JLabel();
        messageLabel1.setText(controller.getLocaliser().getString("showOpenUriView.message1"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(messageLabel1, constraints);

        JLabel messageLabel2 = new JLabel();
        messageLabel2.setText(controller.getLocaliser().getString("showOpenUriView.message2"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(messageLabel2, constraints);

        rememberCheckBox = new JCheckBox();
        rememberCheckBox.setText(controller.getLocaliser().getString("showOpenUriView.rememberMyDecision"));
        
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        add(rememberCheckBox, constraints);
      
        JPanel buttonPanel = new JPanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        add(buttonPanel, constraints);

        ShowOpenUriCancelAction cancelAction = new ShowOpenUriCancelAction(controller, this, this);
        cancelButton = new JButton(cancelAction);
        cancelButton.setText(controller.getLocaliser().getString("showOpenUriView.noText"));
        buttonPanel.add(cancelButton);

        ShowOpenUriSubmitAction showOpenUriSubmitAction = new ShowOpenUriSubmitAction(mainFrame, this.bitcoinController, this, this);
        submitButton = new JButton(showOpenUriSubmitAction);
        submitButton.setText(controller.getLocaliser().getString("showOpenUriView.yesText"));
        buttonPanel.add(submitButton);

        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(filler3, constraints);
    }

    @Override
    public void navigateAwayFromView() {
        setVisible(false);
        log.debug("Navigate away from view called for ShowOpenUriDialog " + this.toString());
    }

    @Override
    public void displayView(DisplayHint displayHint) { 
        log.debug("display called for ShowOpenUriDialog " + this.toString());
        adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
        if (adjustedFont != null) {
            setFileChooserFont(new Container[] {this});
        }

        String showDialogString = controller.getModel().getUserPreference(BitcoinModel.OPEN_URI_SHOW_DIALOG);
       
        if (!(Boolean.FALSE.toString().equalsIgnoreCase(showDialogString))) {
            // missing showDialog or it is set to true
            rememberCheckBox.setSelected(false);
        } else {
            rememberCheckBox.setSelected(true);
        }
        setVisible(true);

        invalidate();
        validate();
        repaint();
        
        // bring this dialog to the front
        bringToFront();
    }
    
    private void bringToFront() {
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                toFront();
                repaint();
            }
        });
    }
    
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.MULTIBIT_SMALL_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showOpenUriView.title");
    }
  
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showOpenUriView.title");
    }

    @Override
    public View getViewId() {
        return View.SHOW_OPEN_URI_DIALOG_VIEW;
    }

    // BitcoinFormDataProvider methods
    @Override
    public String getAddress() {
        return controller.getModel().getUserPreference(BitcoinModel.OPEN_URI_ADDRESS);
    }

    @Override
    public String getLabel() {
        return controller.getModel().getUserPreference(BitcoinModel.OPEN_URI_LABEL);
    }

    @Override
    public String getAmount() {
        return controller.getModel().getUserPreference(BitcoinModel.OPEN_URI_AMOUNT);
    }

    // ShowUriDialogDataProvider method
    @Override
    public boolean isShowUriDialog() {
        return !rememberCheckBox.isSelected();
    }
      
    private void setFileChooserFont(Component[] comp) {
        for (int x = 0; x < comp.length; x++) {
            if (comp[x] instanceof Container)
                setFileChooserFont(((Container) comp[x]).getComponents());
            try {
                comp[x].setFont(adjustedFont);
            } catch (Exception e) {
            }// do nothing
        }
    }

    @Override
    public String getAmountFiat() {
        // TODO Auto-generated method stub
        return null;
    }
}