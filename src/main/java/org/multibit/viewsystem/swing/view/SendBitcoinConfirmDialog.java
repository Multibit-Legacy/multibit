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
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OkBackToParentAction;
import org.multibit.viewsystem.swing.action.SendBitcoinNowAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

import com.google.bitcoin.core.WalletType;

/**
 * The send bitcoin confirm dialog.
 */
public class SendBitcoinConfirmDialog extends MultiBitDialog {

    private static final long serialVersionUID = 191435612345057705L;

    private static final int HEIGHT_DELTA = 150;
    private static final int WIDTH_DELTA = 280;
    
    private static final int STENT_WIDTH = 10;
    
    private MultiBitFrame mainFrame;
    private SendBitcoinConfirmPanel sendBitcoinConfirmPanel;

    private MultiBitController controller;

    private MultiBitLabel sendAddressText;
    private MultiBitLabel sendLabelText;
    private MultiBitLabel sendAmountText;
    private MultiBitLabel sendFeeText;

    private String sendAddress;
    private String sendLabel;
    private String sendAmount;
    private String sendFee;

    private MultiBitLabel confirmText1, confirmText2;
    
    private SendBitcoinNowAction sendBitcoinNowAction;
    private MultiBitButton sendButton;
    private MultiBitButton cancelButton;
    
    private JPasswordField walletPasswordField;
    private MultiBitLabel walletPasswordPromptLabel;

    /**
     * Creates a new {@link SendBitcoinConfirmDialog}.
     */
    public SendBitcoinConfirmDialog(MultiBitController controller, MultiBitFrame mainFrame) {
        super(mainFrame, controller.getLocaliser().getString("sendBitcoinConfirmView.title"));
        this.controller = controller;
        this.mainFrame = mainFrame;

        ImageIcon imageIcon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }
        
        initUI();
        
        cancelButton.requestFocusInWindow();
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    /**
     * Initialise bitcoin confirm dialog.
     */
    public void initUI() {
        FontMetrics fontMetrics = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        if (mainFrame != null) {
            int minimumHeight = fontMetrics.getHeight() * 11 + HEIGHT_DELTA;
            int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinConfirmView.message"))) + WIDTH_DELTA;
            setMinimumSize(new Dimension(minimumWidth, minimumHeight));
            positionDialogRelativeToParent(this, 0.5D, 0.47D);
        }
        
        sendBitcoinConfirmPanel = new SendBitcoinConfirmPanel(controller, mainFrame, this);
        sendBitcoinConfirmPanel.setOpaque(false);
        
        setLayout(new BorderLayout());
        add(sendBitcoinConfirmPanel, BorderLayout.CENTER);
    }

//    public void setMessageText(final String message1, final String message2) {
//        SwingUtilities.invokeLater(new Runnable() {
//            @Override
//            public void run() {
//                confirmText1.setText(message1);
//                confirmText2.setText(" " + message2);
//            }});
//        invalidate();
//        validate();
//        repaint();
//    }    
//    
//    public void clearPassword() {
//        SwingUtilities.invokeLater(new Runnable() {
//            @Override
//            public void run() {
//                walletPasswordField.setText("");
//            }});
//    }
//    
//    public void showOkButton() {
//        OkBackToParentAction okAction = new OkBackToParentAction(controller, this);
//        sendButton.setAction(okAction);
//        
//        cancelButton.setVisible(false);
//    }
//    
//    // Used in testing.
//    public SendBitcoinNowAction getSendBitcoinNowAction() {
//        return sendBitcoinNowAction;
//    }
//    
//    public String getMessageText1() {
//        return confirmText1.getText();
//    }    
//    
//    public String getMessageText2() {
//        return confirmText2.getText();
//    }
//    
//    public void setWalletPassword(char[] password) {
//        walletPasswordField.setText(new String(password));
//    }
//    
//    public boolean isWalletPasswordFieldEnabled() {
//        return walletPasswordField.isEnabled();
//    }
}