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

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.DeleteWalletSubmitAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;

/**
 * The delete wallet confirm dialog
 */
public class DeleteWalletConfirmDialog extends MultiBitDialog {
    private static final long serialVersionUID = 191435612345057705L;

    private static final int HEIGHT_DELTA = 100;
    private static final int WIDTH_DELTA = 200;

    private MultiBitController controller;

    private MultiBitLabel walletDescriptionText;
    private MultiBitLabel sendLabelText;
    private MultiBitLabel balanceText;

    private MultiBitLabel explainLabel;
    private MultiBitLabel confirmText1, confirmText2;

    private MultiBitButton deleteWalletButton;
    private MultiBitButton cancelButton;

    /**
     * Creates a new {@link DeleteWalletConfirmDialog}.
     */
    public DeleteWalletConfirmDialog(MultiBitController controller, MultiBitFrame mainFrame) {
        super(mainFrame, controller.getLocaliser().getString("deleteWalletConfirmDialog.title"));
        this.controller = controller;

        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }

        initUI();

        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * initialise bitcoin confirm dialog
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());

        int minimumHeight = fontMetrics.getHeight() * 10 + HEIGHT_DELTA;
        int minimumWidth = Math.max(fontMetrics.stringWidth(controller.getModel().getActiveWalletFilename()),
                fontMetrics.stringWidth(controller.getLocaliser().getString("deleteWalletConfirmDialog.message")))
                + WIDTH_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        positionDialogRelativeToParent(this, 0.5D, 0.47D);

        JPanel mainPanel = new JPanel();
        mainPanel.setOpaque(false);

        setLayout(new BorderLayout());
        add(mainPanel, BorderLayout.CENTER);

        mainPanel.setLayout(new GridBagLayout());

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
        mainPanel.add(filler00, constraints);

        JLabel filler01 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 5;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler01, constraints);

        ImageIcon bigIcon = ImageLoader.createImageIcon(ImageLoader.EXCLAMATION_MARK_ICON_FILE);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.2;
        constraints.gridwidth = 1;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;
        JLabel bigIconLabel = new JLabel(bigIcon);
        mainPanel.add(bigIconLabel, constraints);

        explainLabel = new MultiBitLabel("");
        explainLabel.setText(controller.getLocaliser().getString("deleteWalletConfirmDialog.message"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 0.3;
        constraints.gridwidth = 5;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(explainLabel, constraints);

        JPanel detailPanel = new JPanel(new GridBagLayout());
        detailPanel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.6;
        constraints.weighty = 0.8;
        constraints.gridwidth = 3;
        constraints.gridheight = 5;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(detailPanel, constraints);

        GridBagConstraints constraints2 = new GridBagConstraints();

        MultiBitLabel walletDescriptionLabel = new MultiBitLabel("");
        walletDescriptionLabel.setText(controller.getLocaliser().getString("resetTransactionsPanel.walletDescriptionLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(walletDescriptionLabel, constraints2);

        JLabel filler1 = new JLabel();
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 1;
        constraints2.gridy = 0;
        constraints2.weightx = 0.1;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler1, constraints2);

        walletDescriptionText = new MultiBitLabel(controller.getModel().getActivePerWalletModelData().getWalletDescription());
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 0;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(walletDescriptionText, constraints2);

        MultiBitLabel walletFilenameLabel = new MultiBitLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletFilenameLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(walletFilenameLabel, constraints2);

        sendLabelText = new MultiBitLabel(controller.getModel().getActiveWalletFilename());
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 1;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(sendLabelText, constraints2);

        MultiBitLabel balanceLabel = new MultiBitLabel("");
        balanceLabel.setText(controller.getLocaliser().getString("multiBitFrame.balanceLabel"));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_END;
        detailPanel.add(balanceLabel, constraints2);

        balanceText = new MultiBitLabel(controller.getLocaliser().bitcoinValueToString(
                controller.getModel().getActiveWalletEstimatedBalance(), true, false));
        constraints2.fill = GridBagConstraints.NONE;
        constraints2.gridx = 2;
        constraints2.gridy = 2;
        constraints2.weightx = 0.3;
        constraints2.weighty = 0.1;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(balanceText, constraints2);

        JLabel filler2 = new JLabel();
        constraints2.fill = GridBagConstraints.HORIZONTAL;
        constraints2.gridx = 0;
        constraints2.gridy = 3;
        constraints2.weightx = 0.05;
        constraints2.weighty = 0.05;
        constraints2.gridwidth = 1;
        constraints2.anchor = GridBagConstraints.LINE_START;
        detailPanel.add(filler2, constraints2);

        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.8;
        constraints.weighty = 0.1;
        constraints.gridwidth = 4;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(buttonPanel, constraints);

        CancelBackToParentAction cancelAction = new CancelBackToParentAction(controller, null, this);
        cancelButton = new MultiBitButton(cancelAction, controller);
        buttonPanel.add(cancelButton);

        DeleteWalletSubmitAction deleteWalletSubmitAction = new DeleteWalletSubmitAction(controller, ImageLoader.createImageIcon(ImageLoader.DELETE_WALLET_ICON_FILE), this);
        deleteWalletButton = new MultiBitButton(deleteWalletSubmitAction, controller);
        buttonPanel.add(deleteWalletButton);

        confirmText1 = new MultiBitLabel("");
        confirmText1.setText(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 8;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(confirmText1, constraints);

        JLabel filler3 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 8;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler3, constraints);

        confirmText2 = new MultiBitLabel(" ");
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 9;
        constraints.weightx = 0.8;
        constraints.weighty = 0.15;
        constraints.gridwidth = 4;
        constraints.anchor = GridBagConstraints.LINE_END;
        mainPanel.add(confirmText2, constraints);

        JLabel filler4 = new JLabel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 5;
        constraints.gridy = 9;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        mainPanel.add(filler4, constraints);
    }

    public void setDeleteConfirmText(String confirm1, String confirm2) {
        confirmText1.setText(confirm1);
        confirmText2.setText(confirm2);

        OkBackToParentAction okAction = new OkBackToParentAction(controller, this);
        deleteWalletButton.setAction(okAction);

        cancelButton.setVisible(false);
    }

    public MultiBitLabel getExplainLabel() {
        return explainLabel;
    }
}