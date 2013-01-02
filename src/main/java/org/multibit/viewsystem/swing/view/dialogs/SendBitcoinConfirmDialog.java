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

import org.multibit.viewsystem.swing.view.panels.SendBitcoinConfirmPanel;
import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FontMetrics;

import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;

/**
 * The send bitcoin confirm dialog.
 */
public class SendBitcoinConfirmDialog extends MultiBitDialog {

    private static final long serialVersionUID = 191435612345057705L;

    private static final int HEIGHT_DELTA = 150;
    private static final int WIDTH_DELTA = 400;
        
    private MultiBitFrame mainFrame;
    private SendBitcoinConfirmPanel sendBitcoinConfirmPanel;
    
    private MultiBitController controller;

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
        
        sendBitcoinConfirmPanel.getCancelButton().requestFocusInWindow();
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
}