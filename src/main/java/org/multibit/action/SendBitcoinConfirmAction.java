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
package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;

/**
 * an action to the send bitcoin confirm view
 * 
 * @author jim
 * 
 */
public class SendBitcoinConfirmAction implements Action {

    private MultiBitController controller;

    public SendBitcoinConfirmAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // copy the data into the user preferences (side effect of getData call)
        dataProvider.getData();

        String sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
        String sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);

        // trim the whitespace from the address
//        String trimmedSendAddress = WhitespaceTrimmer.trim(sendAddress);
//        if (sendAddress != null && !sendAddress.equals(trimmedSendAddress)) {
//            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, trimmedSendAddress);
//        }

        Validator validator = new Validator(controller);
        if (validator.validate(sendAddress, sendAmount)) {
            controller.setActionForwardToChild(ActionForward.FORWARD_TO_SEND_BITCOIN_CONFIRM);
        } else {
            controller.setActionForwardToChild(ActionForward.FORWARD_TO_VALIDATION_ERROR);
        }
    }
}
