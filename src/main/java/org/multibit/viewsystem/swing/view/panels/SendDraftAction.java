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
package org.multibit.viewsystem.swing.action;

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletInfoData;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import static java.lang.System.out;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.AbstractTradePanel;
import org.multibit.viewsystem.swing.view.panels.DraftsPanel;

/**
 * This {@link Action} represents an action to create a sending address.
 */
public class SendDraftAction extends AbstractAction {

    private static final long serialVersionUID = 200111935465875405L;
    private BitcoinController bitcoinController;
    private AbstractTradePanel abstractTradePanel;
    private PrintWriter writer;
    JTextField addressField;
    JTextArea label;
    JTextField amount;
    public DraftsPanel dPanel;
    private MultiBitFrame mainFrame;



    /**
     * Creates a new {@link CreateNewSendingAddressAction}.
     */
    public SendDraftAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, AbstractTradePanel abastractTradePanel, JTextField address, JTextArea label, JTextField amount) throws IOException {
        this.abstractTradePanel = abstractTradePanel;
        this.bitcoinController = bitcoinController;
        this.addressField = address;
        this.label = label;
        this.amount = amount;
        this.mainFrame = mainFrame;
        dPanel = new DraftsPanel(bitcoinController, mainFrame);
    }

    /**
     * Create new send address.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        String address ="";
         WalletInfoData addressBook = bitcoinController.getModel().getActiveWalletWalletInfo();
            if (addressBook != null) {
                ArrayList<WalletAddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        WalletAddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            address = addressBookData.getAddress();
                        }
                    }
                }
            }
        try {
            writer = new PrintWriter(new BufferedWriter(new FileWriter(address+"_Drafts.txt",true)));
        } catch (FileNotFoundException ex) {
            Logger.getLogger(SendDraftAction.class.getName()).log(Level.SEVERE, null, ex);
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(SendDraftAction.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(SendDraftAction.class.getName()).log(Level.SEVERE, null, ex);
        }
        String draftsInfo = addressField.getText() + "////" + label.getText() + "////" + amount.getText();
        writer.println(draftsInfo);
        writer.close();
        
        bitcoinController.displayView(View.DRAFT_VIEW);
    }
    private ArrayList draftData(){
        ArrayList<String> array = new ArrayList();
        String address = "";
        WalletInfoData addressBook = bitcoinController.getModel().getActiveWalletWalletInfo();
            if (addressBook != null) {
                ArrayList<WalletAddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        WalletAddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            address = addressBookData.getAddress();
                        }
                    }
                }
            }
        BufferedReader br;
        try {
            br = new BufferedReader(new FileReader(address + "_Drafts.txt"));
            String line;
                try {
                    line = br.readLine();
            while (line != null) {
                array.add(line);
                line = br.readLine();
            }
        } finally {
            br.close();
        }
        } catch (IOException ex) {
            Logger.getLogger(DraftsPanel.class.getName()).log(Level.SEVERE, null, ex);
        }
        return array;
    }

}