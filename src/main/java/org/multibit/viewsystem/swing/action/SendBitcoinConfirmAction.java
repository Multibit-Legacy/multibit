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

import java.awt.event.ActionEvent;

import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.dataproviders.BitcoinFormDataProvider;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.SendBitcoinConfirmDialog;
import org.multibit.viewsystem.swing.view.dialogs.ValidationErrorDialog;

/**
 * This {@link Action} shows the send bitcoin confirm dialog or validation dialog on an attempted spend.
 */
public class SendBitcoinConfirmAction extends MultiBitSubmitAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitFrame mainFrame;
    private BitcoinFormDataProvider dataProvider;

    /**
     * Creates a new {@link SendBitcoinConfirmAction}.
     */
    public SendBitcoinConfirmAction(MultiBitController controller, MultiBitFrame mainFrame, BitcoinFormDataProvider dataProvider) {
        super(controller, "sendBitcoinConfirmAction.text", "sendBitcoinConfirmAction.tooltip","sendBitcoinConfirmAction.mnemonicKey", ImageLoader.createImageIcon(ImageLoader.SEND_BITCOIN_ICON_FILE));
        this.mainFrame = mainFrame;
        this.dataProvider = dataProvider;
    }

    /**
     * Show the send bitcoin confirm dialog.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }

        SendBitcoinConfirmDialog sendBitcoinConfirmDialog = null;
        ValidationErrorDialog validationErrorDialog = null;

        String sendAddress = dataProvider.getAddress();
        String sendAmount = dataProvider.getAmount();
        
        Validator validator = new Validator(controller);
        if (validator.validate(sendAddress, sendAmount)) {
            sendBitcoinConfirmDialog = new SendBitcoinConfirmDialog(controller, mainFrame);
            sendBitcoinConfirmDialog.setVisible(true);
        } else {
            validationErrorDialog = new ValidationErrorDialog(controller, mainFrame);
            validationErrorDialog.setVisible(true);
        }
    }
}