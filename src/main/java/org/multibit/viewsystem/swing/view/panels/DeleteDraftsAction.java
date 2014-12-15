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

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;
import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.models.DraftsTableModel;
import org.multibit.viewsystem.swing.view.panels.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} exports transactions from a wallet.
 */
public class DeleteDraftsAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(CreateWalletSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    private MultiBitFrame mainFrame;
    private Font adjustedFont;
    private DraftsTableModel model;
    private DraftsPanel dPanel;
    private JTable table;
    int rowIndex;
     private PrintWriter writer;
    /**
     * Creates a new {@link ExportTransactionsSubmitAction}.
     */
    public DeleteDraftsAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, JTable table) {
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;
        this.table = table;
        
    }

    /**
     * Ask the user for a filename and then export transactions to there.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        int response;
        response = JOptionPane.showConfirmDialog(null,"Are You Sure You Want To Delete The Draft?",null, JOptionPane.YES_NO_OPTION);
        if (response == JOptionPane.NO_OPTION){
           // JOptionPane.showMessageDialog(null,"Will Not Delete");
        } else if (response == JOptionPane.YES_OPTION){
        rowIndex = table.getSelectedRow();
        if(rowIndex != -1){
        String address = (String)table.getValueAt(table.getSelectedRow(), 0);
        String label = (String)table.getValueAt(table.getSelectedRow(), 1);
        String amount = (String)table.getValueAt(table.getSelectedRow(), 2);
        
        String walletAddress = "";
        WalletInfoData addressBook = bitcoinController.getModel().getActiveWalletWalletInfo();
            if (addressBook != null) {
                ArrayList<WalletAddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        WalletAddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            walletAddress = addressBookData.getAddress();
                        }
                    }
                }
            }
            
        try {
        File file = new File(walletAddress+"_Drafts.txt");
        File tempFile = new File("temp1.txt");
        
        BufferedReader br = new BufferedReader(new FileReader(file));
        PrintWriter pw = new PrintWriter(new FileWriter(tempFile));
        
        
        try{
            Scanner scanner=new Scanner(file);
            String x=(address + "////" + label + "////" + amount);
            int i =0;
            while (scanner.hasNextLine()){
                String line=scanner.nextLine();
                if (i != rowIndex) {
                    pw.println(line);
                    pw.flush();
                }
                else{
                    model = new DraftsTableModel(bitcoinController);
                    table.setModel(model);
                }
                i++;
            }
            scanner.close(); 
            pw.close();
            br.close();
            //if (file.delete()) {
              //  int response;
                //response = JOptionPane.showConfirmDialog(null,"Are You Sure You Want To Delete The Draft?");
                //if (response == JOptionPane.YES_OPTION){
                  //  System.out.print("yes");
                    
               // }
               // else{
                //    System.out.print("no/cancel");
                    
                //}
                //return;
            //}
            if (!file.delete()) {
                JOptionPane.showMessageDialog(null,"Cannot Delete");
                return;
            }
            
            if (!tempFile.renameTo(file)){
                JOptionPane.showMessageDialog(null,"Could not rename file");
            }
            
            
            } catch (FileNotFoundException ex) {
                JOptionPane.showMessageDialog(null, "Error");
            }
        }catch (FileNotFoundException ex) {
      ex.printStackTrace();
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }}
    else {
        JOptionPane.showMessageDialog(null, "No selected row in the table..");
    }
    }}
}