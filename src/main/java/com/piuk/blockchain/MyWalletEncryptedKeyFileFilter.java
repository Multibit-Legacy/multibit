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

import org.multibit.model.bitcoin.BitcoinModel;

public class MyWalletEncryptedKeyFileFilter extends javax.swing.filechooser.FileFilter {

    public MyWalletEncryptedKeyFileFilter() {    }
    
    @Override
    public boolean accept(File file) {
        return (file.getName().toLowerCase().endsWith(BitcoinModel.BLOCKCHAIN_WALLET_ENCRYPTED_SUFFIX));
    }

    @Override
    public String getDescription() {
        String multiBitText = "";
        
        multiBitText += "Blockchain.info " + " (*." + BitcoinModel.BLOCKCHAIN_WALLET_ENCRYPTED_SUFFIX + ")";

         return multiBitText;
    }
}
