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
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * an action that actually processes a bitcoin URI submit
 * 
 * @author jim
 * 
 */
public class ShowOpenUriSubmitAction implements Action {

    private static final Logger log = LoggerFactory.getLogger(ShowOpenUriSubmitAction.class);

    private MultiBitController controller;

    public ShowOpenUriSubmitAction(MultiBitController controller) {
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
            // get the data out of the temporary data and put it in the wallet preferences
            Item sendAddressItem = dataProvider.getData().getItem(MultiBitModel.OPEN_URI_ADDRESS);
            Item sendLabelItem = dataProvider.getData().getItem(MultiBitModel.OPEN_URI_LABEL);
            Item sendAmountItem = dataProvider.getData().getItem(MultiBitModel.OPEN_URI_AMOUNT);
            Item showDialogItem = dataProvider.getData().getItem(MultiBitModel.OPEN_URI_SHOW_DIALOG);
 
            if (sendAddressItem != null) {
                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, (String)sendAddressItem.getNewValue());                
            }
            if (sendLabelItem != null) {
                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, (String)sendLabelItem.getNewValue());                
            }
            if (sendAmountItem != null) {
                controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, (String)sendAmountItem.getNewValue());                
            }
            // we want the send view to paste in the send data
            controller.getModel().setActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW, "true");

            // we want to set the user preference to use the uri as the user clicked yes
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI, "true");      
            
            // save as user preference whether to show dialog or not
            if (showDialogItem != null) {
                controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG, (String)showDialogItem.getNewValue());                
            }
             
            controller.setActionForwardToSiblingOfParent(ActionForward.FORWARD_TO_SEND_BITCOIN);

        }
    }
}
