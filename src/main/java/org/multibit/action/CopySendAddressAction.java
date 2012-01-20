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

import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.action.TextTransfer;

/**
 * an action to copy the send address in the supplied formbean to the system
 * clipboard
 * 
 * @author jim
 * 
 */
public class CopySendAddressAction implements Action {

    public CopySendAddressAction() {
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item addressItem = data.getItem(MultiBitModel.SEND_ADDRESS);
                String addressText = "";
                
                if (addressItem != null && addressItem.getNewValue() != null) {
                    addressText = (String) addressItem.getNewValue();

                    // copy to clipboard
                    TextTransfer textTransfer = new TextTransfer();
                    textTransfer.setClipboardContents(addressText);
                }
            }
        }
    }
}
