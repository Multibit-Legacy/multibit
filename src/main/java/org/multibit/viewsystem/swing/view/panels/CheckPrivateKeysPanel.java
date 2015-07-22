/**
 * Copyright 2014 multibit.org
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
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.action.CheckPrivateKeysSubmitAction;
import org.multibit.viewsystem.swing.action.HelpContextAction;
import org.multibit.viewsystem.swing.view.components.HelpButton;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

import javax.swing.*;
import java.awt.*;

/**
 * View for checking private keys panel
 */
public class CheckPrivateKeysPanel extends JPanel implements Viewable, WalletBusyListener {

  private static final long serialVersionUID = 44499432329957705L;

  private final Controller controller;
  private final BitcoinController bitcoinController;

  private MultiBitLabel messageLabel1;
  private MultiBitLabel messageLabel2;

  private MultiBitLabel walletTextLabel;
  private JPasswordField walletPasswordField;
  private MultiBitLabel walletPasswordPromptLabel;

  private CheckPrivateKeysSubmitAction checkPrivateKeysSubmitAction;

  /**
   * Creates a new {@link org.multibit.viewsystem.swing.view.panels.CheckPrivateKeysPanel}.
   */
  public CheckPrivateKeysPanel(BitcoinController bitcoinController) {
    this.bitcoinController = bitcoinController;
    this.controller = this.bitcoinController;

    setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
    applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    initUI();

    walletBusyChange(this.bitcoinController.getModel().getActivePerWalletModelData().isBusy());
    this.bitcoinController.registerWalletBusyListener(this);
  }

  private void initUI() {
    setLayout(new BorderLayout());

    JPanel mainPanel = new JPanel();
    mainPanel.setMinimumSize(new Dimension(800, 480));
    mainPanel.setLayout(new GridBagLayout());
    mainPanel.setOpaque(false);
    mainPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    String[] keys = new String[]{"showExportPrivateKeysPanel.walletPasswordPrompt"};

    int stentWidth = MultiBitTitledPanel.calculateStentWidthForKeys(controller.getLocaliser(), keys, this)
            + ExportPrivateKeysPanel.STENT_DELTA;

    GridBagConstraints constraints = new GridBagConstraints();

    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    constraints.weighty = 1;
    constraints.anchor = GridBagConstraints.LINE_START;
    JPanel instructionsPanel = createInstructionsPanel(stentWidth);
    mainPanel.add(instructionsPanel, constraints);

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.gridy = 1;
    constraints.gridwidth = 1;
    constraints.gridheight = 1;
    constraints.weightx = 1;
    constraints.weighty = 0.1;
    constraints.anchor = GridBagConstraints.CENTER;
    mainPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);

    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.gridy = 2;
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    constraints.weighty = 1;
    constraints.anchor = GridBagConstraints.LINE_START;
    JPanel walletPanel = createWalletPanel(stentWidth);
    mainPanel.add(walletPanel, constraints);

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.gridy = 3;
    constraints.gridwidth = 1;
    constraints.gridheight = 1;
    constraints.weightx = 1;
    constraints.weighty = 0.1;
    constraints.anchor = GridBagConstraints.CENTER;
    mainPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.gridy = 5;
    constraints.gridwidth = 1;
    constraints.gridheight = 1;
    constraints.weightx = 1;
    constraints.weighty = 0.1;
    constraints.anchor = GridBagConstraints.CENTER;
    mainPanel.add(MultiBitTitledPanel.createStent(12, 12), constraints);

    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.gridy = 6;
    constraints.gridwidth = 1;
    constraints.weightx = 0.4;
    constraints.weighty = 0.06;
    constraints.anchor = GridBagConstraints.LINE_START;
    JPanel buttonPanel = createButtonPanel();
    mainPanel.add(buttonPanel, constraints);

    messageLabel1 = new MultiBitLabel(" ");
    messageLabel1.setOpaque(false);
    messageLabel1.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));
    messageLabel1.setHorizontalAlignment(JLabel.LEADING);
    messageLabel1.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.gridy = 7;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    constraints.weighty = 0.06;
    constraints.anchor = GridBagConstraints.LINE_START;
    mainPanel.add(messageLabel1, constraints);

    messageLabel2 = new MultiBitLabel(" ");
    messageLabel2.setOpaque(false);
    messageLabel2.setBorder(BorderFactory.createEmptyBorder(0, 30, 0, 0));
    messageLabel2.setHorizontalAlignment(JLabel.LEADING);
    messageLabel2.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.gridy = 8;
    constraints.gridwidth = 3;
    constraints.weightx = 1;
    constraints.weighty = 0.06;
    constraints.anchor = GridBagConstraints.LINE_START;
    mainPanel.add(messageLabel2, constraints);

    Action helpAction;
    if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
      helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_ICON_FILE,
              "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
              HelpContentsPanel.HELP_CHECK_PRIVATE_KEYS_URL);
    } else {
      helpAction = new HelpContextAction(controller, ImageLoader.HELP_CONTENTS_BIG_RTL_ICON_FILE,
              "multiBitFrame.helpMenuText", "multiBitFrame.helpMenuTooltip", "multiBitFrame.helpMenuText",
              HelpContentsPanel.HELP_CHECK_PRIVATE_KEYS_URL);
    }
    HelpButton helpButton = new HelpButton(helpAction, controller);
    helpButton.setText("");

    String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[]{controller.getLocaliser().getString(
            "multiBitFrame.helpMenuTooltip")});
    helpButton.setToolTipText(tooltipText);
    helpButton.setHorizontalAlignment(SwingConstants.LEADING);
    helpButton.setBorder(BorderFactory.createEmptyBorder(0, AbstractTradePanel.HELP_BUTTON_INDENT,
            AbstractTradePanel.HELP_BUTTON_INDENT, AbstractTradePanel.HELP_BUTTON_INDENT));
    helpButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.gridy = 9;
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
    constraints.gridy = 10;
    constraints.gridwidth = 1;
    constraints.weightx = 1;
    constraints.weighty = 100;
    constraints.anchor = GridBagConstraints.CENTER;
    mainPanel.add(filler2, constraints);

    JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
    mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
    mainScrollPane.getViewport().setOpaque(true);
    mainScrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    mainScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
    mainScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

    add(mainScrollPane, BorderLayout.CENTER);
  }

  private JPanel createInstructionsPanel(int stentWidth) {
    MultiBitTitledPanel instructionsPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
            "resetTransactionsPanel.explainTitle"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
            controller.getLocaliser().getString("showCheckPrivateKeysAction.explain1"), 3, instructionsPanel);

    return instructionsPanel;
  }

  private JPanel createWalletPanel(int stentWidth) {
    MultiBitTitledPanel inputWalletPanel = new MultiBitTitledPanel(controller.getLocaliser().getString(
            "showExportPrivateKeysPanel.walletPasswordPrompt"), ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    GridBagConstraints constraints = new GridBagConstraints();

    walletTextLabel = MultiBitTitledPanel.addLeftJustifiedTextAtIndent(
            controller.getLocaliser().getString("signMessagePanel.wallet.text"), 3, inputWalletPanel);

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 1;
    constraints.gridy = 4;
    constraints.weightx = 0.3;
    constraints.weighty = 0.3;
    constraints.gridwidth = 1;
    constraints.anchor = GridBagConstraints.LINE_START;
    inputWalletPanel.add(MultiBitTitledPanel.createStent(stentWidth, (int) (ExportPrivateKeysPanel.STENT_HEIGHT * 0.5)), constraints);

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 2;
    constraints.gridy = 5;
    constraints.weightx = 0.05;
    constraints.weighty = 0.3;
    constraints.gridwidth = 1;
    constraints.anchor = GridBagConstraints.CENTER;
    inputWalletPanel.add(MultiBitTitledPanel.createStent(MultiBitTitledPanel.SEPARATION_BETWEEN_NAME_VALUE_PAIRS), constraints);

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

  private JPanel createButtonPanel() {
    JPanel buttonPanel = new JPanel();
    buttonPanel.setOpaque(false);
    FlowLayout flowLayout = new FlowLayout();
    flowLayout.setAlignment(FlowLayout.LEADING);
    buttonPanel.setLayout(flowLayout);
    buttonPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    checkPrivateKeysSubmitAction = new CheckPrivateKeysSubmitAction(this.bitcoinController, this,
            ImageLoader.createImageIcon(ImageLoader.CHECK_PRIVATE_KEYS_ICON_FILE));
    MultiBitButton submitButton = new MultiBitButton(checkPrivateKeysSubmitAction, controller);
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

    boolean walletPasswordRequired = false;
    if (this.bitcoinController.getModel().getActiveWallet() != null && this.bitcoinController.getModel().getActiveWallet().getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
      walletPasswordRequired = true;
    }
    enableWalletPassword(walletPasswordRequired);

    walletBusyChange(this.bitcoinController.getModel().getActivePerWalletModelData().isBusy());

    messageLabel1.setText(" ");
    messageLabel2.setText(" ");
  }

  @Override
  public void navigateAwayFromView() {
  }

  public void setMessageText1(String message1) {
    if (messageLabel1 != null) {
      messageLabel1.setText(message1);
    }
  }

  public String getMessageText1() {
    if (messageLabel1 != null) {
      return messageLabel1.getText();
    } else {
      return "";
    }
  }

  public void setMessageText2(String message2) {
    if (messageLabel2 != null) {
      messageLabel2.setText(message2);
    }
  }

  @Override
  public Icon getViewIcon() {
    return ImageLoader.createImageIcon(ImageLoader.CHECK_PRIVATE_KEYS_ICON_FILE);
  }

  @Override
  public String getViewTitle() {
    return controller.getLocaliser().getString("showCheckPrivateKeysAction.text");
  }

  @Override
  public String getViewTooltip() {
    return controller.getLocaliser().getString("showCheckPrivateKeysAction.tooltip");
  }

  @Override
  public View getViewId() {
    return View.SHOW_CHECK_PRIVATE_KEYS_VIEW;
  }

  public CheckPrivateKeysSubmitAction getCheckPrivateKeysSubmitAction() {
    return checkPrivateKeysSubmitAction;
  }

  @Override
  public void walletBusyChange(boolean newWalletIsBusy) {
    // Update the enable status of the action to match the wallet busy status.
    if (this.bitcoinController.getModel().getActivePerWalletModelData().isBusy()) {
      // Wallet is busy with another operation that may change the private keys - Action is disabled.
      checkPrivateKeysSubmitAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("multiBitSubmitAction.walletIsBusy",
              new Object[]{controller.getLocaliser().getString(this.bitcoinController.getModel().getActivePerWalletModelData().getBusyTaskKey())})));
      checkPrivateKeysSubmitAction.setEnabled(false);
    } else {
      // Enable
      checkPrivateKeysSubmitAction.putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("showCheckPrivateKeysAction.tooltip")));
      checkPrivateKeysSubmitAction.setEnabled(true);
    }
  }

  private void enableWalletPassword(boolean enableWalletPassword) {
    // Enable/ disable the wallet password fields.
    walletPasswordField.setEnabled(enableWalletPassword);
    walletPasswordPromptLabel.setEnabled(enableWalletPassword);
    walletTextLabel.setEnabled(enableWalletPassword);
  }

  public JPasswordField getWalletPasswordField() {
    return walletPasswordField;
  }

  public void setWalletPassword(CharSequence password) {
    walletPasswordField.setText(password.toString());
  }


  public boolean isWalletPasswordFieldEnabled() {
    return walletPasswordField.isEnabled();
  }

}