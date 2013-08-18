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
package org.multibit.model;

import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletInfoData;
import java.io.File;
import java.util.ArrayList;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.store.MultiBitWalletVersion;

public class WalletInfoTest extends TestCase {
    public static final String WALLET_TESTDATA_DIRECTORY = "wallets";

    public static final String WALLET_TEST1 = "walletInfoTest.wallet";
    public static final String DESCRIPTION_TEST1 = "myDescription1, textAfterComma\ntextAfterReturn";

    public static final String NON_EXISTENT_WALLET = "nonExistentWallet.wallet";

    public static final String EXAMPLE_RECEIVING_ADDRESS = "1NzESHfiazCbxwhTCg2jiTWcZgpSMKDKhy";
    public static final String EXAMPLE_RECEIVING_ADDRESS_LABEL = "myReceivingAddress label, text after comma\n text after return";

    public static final String EXAMPLE_SENDING_ADDRESS = "1K9A6xh9wGZD1xNLBdxUSFNxtRFUsw5Z4n";
    public static final String EXAMPLE_SENDING_ADDRESS_LABEL = "mySendingAddress label, text after comma\n text after return";

    private String PROPERTY_NAME1 = "aProperty";
    private String PROPERTY_VALUE1 = "aValue";
       
    private String PROPERTY_NAME2 = "anotherProperty";
    private String PROPERTY_VALUE2 = "anotherValue";
       
    @Test
    public void testRoundTrip() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;

        // get test directory and wallet
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + WALLET_TEST1;

        // create wallet info
        WalletInfoData walletInfo = new WalletInfoData(walletName, null, MultiBitWalletVersion.SERIALIZED);
        assertNotNull(walletInfo);

        walletInfo.put(WalletInfoData.DESCRIPTION_PROPERTY, DESCRIPTION_TEST1);

        WalletAddressBookData receivingAddress = new WalletAddressBookData(EXAMPLE_RECEIVING_ADDRESS_LABEL, EXAMPLE_RECEIVING_ADDRESS);
        walletInfo.addReceivingAddress(receivingAddress, true);

        WalletAddressBookData sendingAddress = new WalletAddressBookData(EXAMPLE_SENDING_ADDRESS_LABEL, EXAMPLE_SENDING_ADDRESS);
        walletInfo.addSendingAddress(sendingAddress);

        walletInfo.put(PROPERTY_NAME1, PROPERTY_VALUE1);
        walletInfo.put(PROPERTY_NAME2, PROPERTY_VALUE2);
        
        // write to file
        walletInfo.writeToFile(WalletInfoData.createWalletInfoFilename(walletName), MultiBitWalletVersion.SERIALIZED);

        String createdWalletInfoFile = WalletInfoData.createWalletInfoFilename(walletName);

        assertTrue((new File(createdWalletInfoFile)).exists());

        // create new wallet info and reload
        WalletInfoData rebornWalletInfo = new WalletInfoData(walletName, null, MultiBitWalletVersion.SERIALIZED);
        assertNotNull(rebornWalletInfo);

        // check description
        assertEquals(DESCRIPTION_TEST1, rebornWalletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY));

        // check sending address
        ArrayList<WalletAddressBookData> sendAddresses = rebornWalletInfo.getSendingAddresses();
        assertEquals(1, sendAddresses.size());
        WalletAddressBookData sendAddress = sendAddresses.get(0);
        assertEquals(EXAMPLE_SENDING_ADDRESS_LABEL, sendAddress.getLabel());
        assertEquals(EXAMPLE_SENDING_ADDRESS, sendAddress.getAddress());
 
        // check receiving address
        ArrayList<WalletAddressBookData> receiveAddresses = rebornWalletInfo.getReceivingAddresses();
        assertEquals(1, receiveAddresses.size());
        WalletAddressBookData receiveAddress = receiveAddresses.get(0);
        assertEquals(EXAMPLE_RECEIVING_ADDRESS_LABEL, receiveAddress.getLabel());
        assertEquals(EXAMPLE_RECEIVING_ADDRESS, receiveAddress.getAddress());
        
        // Check properties.
        assertEquals(PROPERTY_VALUE1, rebornWalletInfo.getProperty(PROPERTY_NAME1));
        assertEquals(PROPERTY_VALUE2, rebornWalletInfo.getProperty(PROPERTY_NAME2));
    }

    @Test
    public void testloadNonExistentInfoFile() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        final BitcoinController controller = controllers.bitcoinController;
        
        // Get test directory and wallet.
        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator
                + WALLET_TESTDATA_DIRECTORY + File.separator + NON_EXISTENT_WALLET;

        // Create wallet info - should not throw exception.
        WalletInfoData walletInfo = new WalletInfoData(walletName, null, MultiBitWalletVersion.PROTOBUF);
        assertNotNull(walletInfo);
    }
    
    @Test
    public void testURLEncodeDecode() {
        String initialText = "abcdefghijklmnopqrstuvwxyz%201234567890 !@#$%^&*()_+-= []{};':|`~,./<>?";
        String encodedText = WalletInfoData.encodeURLString(initialText);
        String decodedText = WalletInfoData.decodeURLString(encodedText);
        
        assertEquals(initialText, decodedText);
        
        decodedText = WalletInfoData.decodeURLString("%20abcdef");
        assertEquals(" abcdef", decodedText);

        // checking passing unencoded
        decodedText = WalletInfoData.decodeURLString("abc% de+f, jb\n etc");
        assertEquals("abc% de+f, jb\n etc", decodedText);

        decodedText = WalletInfoData.decodeURLString("abc def, jb\n etc");
        assertEquals("abc def, jb\n etc", decodedText);

        // checking percent character
        decodedText = WalletInfoData.decodeURLString("abc%");
        assertEquals("abc%", decodedText);

        decodedText = WalletInfoData.decodeURLString("abc%d");
        assertEquals("abc%d", decodedText);
    }
}
