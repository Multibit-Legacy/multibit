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
package org.multibit.viewsystem.swing.action;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.qrcode.QRCodeGenerator;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.AbstractTradePanel;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 * This {@link Action} displays a QR code zoomed to the whole display.
 */
public class ZoomAction extends AbstractAction {
    private static final long serialVersionUID = 1923492460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private MultiBitFrame mainFrame;

    private AbstractTradePanel tradePanel;

    private static final int HEIGHT_DELTA = 80;
    private static final int WIDTH_DELTA = 20;

    /**
     * Creates a new {@link ZoomAction}.
     */
    public ZoomAction(BitcoinController bitcoinController, ImageIcon icon, MultiBitFrame mainFrame, AbstractTradePanel tradePanel) {
        super(bitcoinController.getLocaliser().getString("zoomAction.text"), icon);
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;
        this.tradePanel = tradePanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("zoomAction.text")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("zoomAction.text"));
        KeyStroke ctrlZ = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK); // controller.getLocaliser().getString("zoomAction.acceleratorKey")
        putValue(ACCELERATOR_KEY, ctrlZ);
    }

    /**
     * Zoom the QR code.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            // Get the current address, label and amount.
            String address = this.bitcoinController.getModel().getActiveWalletPreference(tradePanel.getAddressConstant());
            String label = this.bitcoinController.getModel().getActiveWalletPreference(tradePanel.getLabelConstant());
            String amount = this.bitcoinController.getModel().getActiveWalletPreference(tradePanel.getAmountConstant());

            // Get the bounds of the current frame.
            Dimension mainFrameSize = mainFrame.getSize();

            int scaleWidth = (int) (mainFrameSize.getWidth() - WIDTH_DELTA);
            int scaleHeight = (int) (mainFrameSize.getHeight() - HEIGHT_DELTA);

            QRCodeGenerator qrCodeGenerator = new QRCodeGenerator(this.bitcoinController);

            Image image = qrCodeGenerator.generateQRcode(address, amount, label, 1);
            if (image != null) {
                int scaleFactor = (int) (Math.floor(Math.min(scaleHeight / image.getHeight(null),
                        scaleWidth / image.getWidth(null))));
                image = qrCodeGenerator.generateQRcode(address, amount, label, scaleFactor);
            }

            // Display the icon.
            JPanel iconPanel = new JPanel(new BorderLayout());
            JLabel iconLabel = new JLabel();
            iconLabel.setIcon(new ImageIcon(image));
            iconLabel.setHorizontalAlignment(SwingConstants.CENTER);
            iconPanel.add(iconLabel, BorderLayout.CENTER);

            String dialogTitle = controller.getLocaliser().getString("multiBitFrame.title") + MultiBitFrame.SEPARATOR
                    + controller.getLocaliser().getString("zoomAction.text");
            final JDialog dialog = new JDialog(mainFrame, dialogTitle, true);
            dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

            final JOptionPane optionPane = new JOptionPane(iconPanel, JOptionPane.PLAIN_MESSAGE);
            optionPane.addPropertyChangeListener(new PropertyChangeListener() {
                @Override
                public void propertyChange(PropertyChangeEvent e) {
                    String prop = e.getPropertyName();
                    Object newValue = e.getNewValue();
                    
                    String newValueString = "";
                    if (newValue != null) {
                        newValueString = newValue.toString();
                    }

                    //System.out.println("property changed = " + prop + ", new value = " + newValue);
                    if (dialog.isVisible() && (e.getSource() == optionPane) && (prop.equals(JOptionPane.VALUE_PROPERTY)) && newValueString.equals("0")) {
                        dialog.setVisible(false);
                    }
                }
            });
            optionPane.setOpaque(false);

            dialog.setContentPane(optionPane);
            dialog.setSize(mainFrameSize);
            dialog.setLocation(mainFrame.getLocationOnScreen());
            // Reduces flickiness.
            dialog.repaint();
            dialog.setVisible(true);
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }
}