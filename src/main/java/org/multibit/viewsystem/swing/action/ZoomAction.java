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
package org.multibit.viewsystem.swing.action;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletInfoException;
import org.multibit.qrcode.SwatchGenerator;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.AbstractTradePanel;
import org.multibit.viewsystem.swing.view.WalletFileFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;

/**
 * This {@link Action} displays a swatch or QR code zoomed to the whole display
 */
public class ZoomAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(ZoomAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private AbstractTradePanel tradePanel;

    private static final int HEIGHT_DELTA = 80;
    private static final int WIDTH_DELTA = 20;

    /**
     * Creates a new {@link ZoomAction}.
     */
    public ZoomAction(MultiBitController controller, ImageIcon icon, MultiBitFrame mainFrame, AbstractTradePanel tradePanel) {
        super(controller.getLocaliser().getString("zoomAction.text"), icon);
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.tradePanel = tradePanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("zoomAction.text"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("zoomAction.text"));
        KeyStroke ctrlZ = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK); // controller.getLocaliser().getString("zoomAction.acceleratorKey")
        putValue(ACCELERATOR_KEY, ctrlZ);
    }

    /**
     * create new wallet
     */
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            // get the current address, label and amount
            String address = controller.getModel().getActiveWalletPreference(tradePanel.getAddressConstant());
            String label = controller.getModel().getActiveWalletPreference(tradePanel.getLabelConstant());
            String amount = controller.getModel().getActiveWalletPreference(tradePanel.getAmountConstant());

            // get the bounds of the current frame
            Dimension mainFrameSize = mainFrame.getSize();

            int scaleWidth = (int) (mainFrameSize.getWidth() - WIDTH_DELTA);
            int scaleHeight = (int) (mainFrameSize.getHeight() - HEIGHT_DELTA);

            SwatchGenerator swatchGenerator = new SwatchGenerator(controller);

            Image image = null;
            if (tradePanel.isDisplayAsQRcode()) {
                image = swatchGenerator.generateQRcode(address, amount, label, 1);

                if (image != null) {
                    int scaleFactor = (int) (Math.floor(Math.min(scaleHeight / image.getHeight(null),
                            scaleWidth / image.getWidth(null))));
                    image = swatchGenerator.generateQRcode(address, amount, label, scaleFactor);
                }
            } else if (tradePanel.isDisplayAsSwatch()) {
                image = swatchGenerator.generateSwatch(address, amount, label);
                if (image != null) {
                    int widthFactor = (int)(Math.floor(scaleWidth / image.getWidth(null)));
                    int heightFactor = (int)(Math.floor(scaleHeight / image.getHeight(null)));
                    int scaleFactor = Math.min(widthFactor, heightFactor);
                    image = swatchGenerator.generateSwatch(address, amount, label, scaleFactor);
                }
            }

            // display the icon
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
            //optionPane.setIcon(ImageLoader.createImageIcon(ImageLoader.ZOOM_ICON_FILE));
            optionPane.addPropertyChangeListener(new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent e) {
                    String prop = e.getPropertyName();

                    if (dialog.isVisible() && (e.getSource() == optionPane) && (prop.equals(JOptionPane.VALUE_PROPERTY))) {
                        dialog.setVisible(false);
                    }
                }
            });
            optionPane.setOpaque(false);

            dialog.setContentPane(optionPane);
            dialog.pack();
            dialog.setSize(mainFrameSize);
            dialog.setLocation(mainFrame.getLocationOnScreen());
            dialog.setVisible(true);
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }
}