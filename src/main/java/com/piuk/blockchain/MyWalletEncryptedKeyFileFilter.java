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
package com.piuk.blockchain;

import java.io.File;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

public class MyWalletEncryptedKeyFileFilter extends javax.swing.filechooser.FileFilter {

    public MyWalletEncryptedKeyFileFilter() {    }
    
    public boolean accept(File file) {
        return (file.getName().toLowerCase().endsWith(MultiBitModel.BLOCKCHAIN_WALLET_ENCRYPTED_SUFFIX));
    }

    public String getDescription() {
        String multiBitText = "";
        
        multiBitText += "Blockchain.info " + " (*." + MultiBitModel.BLOCKCHAIN_WALLET_ENCRYPTED_SUFFIX + ")";

         return multiBitText;
    }
}
