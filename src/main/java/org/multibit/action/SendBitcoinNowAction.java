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

import java.io.IOException;
import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;

/**
 * an action that actually sends bitcoin
 * 
 * @author jim
 * 
 */
public class SendBitcoinNowAction implements Action {

    private static final Logger log = LoggerFactory.getLogger(SendBitcoinNowAction.class);

    private MultiBitController controller;

    private final static int MAX_LENGTH_OF_ERROR_MESSAGE = 70;

    public SendBitcoinNowAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            // get the data out of the wallet preferences
            String sendAddress = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_ADDRESS);
            String sendLabel = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_LABEL);
            String sendAmount = controller.getModel().getActiveWalletPreference(MultiBitModel.SEND_AMOUNT);
            String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            BigInteger fee;
            if (sendFeeString == null || sendFeeString.equals("")) {
                fee = MultiBitModel.SEND_MINIMUM_FEE;
            } else {
                fee = Utils.toNanoCoins(sendFeeString);
            }

            if (sendLabel != null && !sendLabel.equals("")) {
                WalletInfo addressBook = perWalletModelData.getWalletInfo();
                addressBook.addSendingAddress(new AddressBookData(sendLabel, sendAddress));
            }

            Boolean sendWasSuccessful = Boolean.FALSE;
            String errorMessage = " ";
            try {
                controller.sendCoins(perWalletModelData, sendAddress, sendLabel, sendAmount, fee);
                sendWasSuccessful = Boolean.TRUE;

                // save the wallet
                controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
            } catch (IOException e) {
                log.error(e.getMessage(), e);

                errorMessage = e.getMessage();
            } catch (AddressFormatException e) {
                log.error(e.getMessage(), e);

                errorMessage = e.getMessage();
            } catch (Throwable t) {
                // really trying to catch anything that goes wrong
                log.error(t.getMessage(), t);
                errorMessage = t.getMessage();
            }

            // // for testing
            // sendWasSuccessful = Boolean.FALSE;
            // errorMessage = "snuibbnfjhsbfjlsfbjslfbnsjkfb";

            if (errorMessage != null && errorMessage.length() > MAX_LENGTH_OF_ERROR_MESSAGE) {
                errorMessage = errorMessage.substring(0, MAX_LENGTH_OF_ERROR_MESSAGE) + "...";
            }
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_WAS_SUCCESSFUL, sendWasSuccessful.toString());
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ERROR_MESSAGE, errorMessage);
        }
    }
}
