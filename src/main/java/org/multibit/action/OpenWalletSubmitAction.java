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

import java.io.File;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

/**
 * an action to process the submit of the Open Wallet view
 * 
 * @author jim
 * 
 */
public class OpenWalletSubmitAction implements Action {

    private MultiBitController controller;

    public OpenWalletSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the file name from the data provider and see if it has changed
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item item = data.getItem(MultiBitModel.ACTIVE_WALLET_FILENAME);
                if (item != null && item.getNewValue() != null && !item.getNewValue().equals(item.getOriginalValue())) {

                    String walletFilename = (String) (item.getNewValue());

                    // defensive check on file being a directory - should never
                    // happen
                    if (!(new File(walletFilename).isDirectory())) {
                        controller.addWalletFromFilename(walletFilename);
                        controller.getModel().setActiveWalletByFilename(walletFilename);
                        controller.fireNewWalletCreated();
                    }
                }
            }
            controller.setActionForwardToParent();

        } else {
            // should never happen return to parent view
            controller.setActionForwardToParent();
        }
    }
}
