
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

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.*;
import java.awt.*;
import java.util.Arrays;
import java.awt.event.ActionEvent;
import static java.lang.System.out;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import static javax.swing.JOptionPane.showMessageDialog;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.WalletTableModel;
import org.multibit.viewsystem.swing.view.dialogs.SendEmailDialog;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;

/**
 * This {@link Action} exports transactions from a wallet.
 */
public class ShowStatisticsAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(CreateWalletSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private MultiBitFrame mainFrame;

    private Font adjustedFont;
    
    private SendEmailDialog sendEmailDialog;
    
    private ShowTransactionsPanel showTransactionsPanel;
    private JTable table;
    private WalletTableModel walletTableModel;
     private TableRowSorter<TableModel> rowSorter;
    
    /**
     * Creates a new {@link ExportTransactionsSubmitAction}.
     */
    public ShowStatisticsAction(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
        super(bitcoinController.getLocaliser().getString("exportTransactionsSubmitAction.text"), ImageLoader.createImageIcon(ImageLoader.TRANSACTIONS_EXPORT_ICON_FILE));
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;

        
    }

    /**
     * Ask the user for a filename and then export transactions to there.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        walletTableModel = new WalletTableModel(bitcoinController);
        table = new JTable(walletTableModel);
        table.setOpaque(false);
        // Row sorter.
        rowSorter = new TableRowSorter<TableModel>(table.getModel());
        table.setRowSorter(rowSorter);

        // Sort by date descending.
        java.util.List<TableRowSorter.SortKey> sortKeys = new ArrayList<TableRowSorter.SortKey>();
        sortKeys.add(new TableRowSorter.SortKey(1, SortOrder.DESCENDING));
        rowSorter.setSortKeys(sortKeys);
        Comparator<Date> comparator = new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                if (o1 == null) {
                    if (o2 == null) {
                        return 0;
                    } else {
                        return 1;
                    }
                } else {
                    if (o2 == null) {
                        return -1;
                    }
                }
                long n1 = o1.getTime();
                long n2 = o2.getTime();
                if (n1 == 0) {
                    // Object 1 has missing date.
                    return 1;
                }
                if (n2 == 0) {
                    // Object 2 has missing date.
                    return -1;
                }
                if (n1 < n2) {
                    return -1;
                } else if (n1 > n2) {
                    return 1;
                } else {
                    return 0;
                }
            }
        };
        rowSorter.setComparator(1, comparator);
        int totalTransactions = table.getRowCount();
        
        int row = table.getRowCount();
        int r_size = 0;
        int s_size = 0;
        // count received transactions and sent transactions:
         for (int j = 0; j  < row; j++) { 
             if (table.getValueAt(j, 2).toString().matches("Rece.*")){
                 r_size += 1;
            }
            if (table.getValueAt(j, 2).toString().matches("Sent.*")) {
                 s_size += 1;
            }  
         }
        
        double[] R = new double[r_size];
        double[] S = new double[s_size];
        double Receieved_BTC = 0.0;
        double Sent_BTC = 0.0;
        
        double max_r = 0.0;
        double min_r = 0.0;
        double average_r = 0.0;
        
        double max_s = 0.0;
        double min_s = 0.0;
        double average_s = 0.0;
                 
        boolean bR = false;
        boolean bS = false;
        
        //extract received transaction into R array, and calculate the total BTC:
        int counter =0;
        for (int j = 0; j  < row; j++) { 
            if (table.getValueAt(j, 2).toString().matches("Rece.*")){
                bR = true;
                R[counter] = (Double.parseDouble((String) table.getValueAt(j,3)));
                Receieved_BTC += R[counter];  
                counter ++;
            }
        }
        counter = 0;
        //extract sent transaction into S array, and calculate the total BTC:
        for (int j = 0; j<row; j++){
            if (table.getValueAt(j, 2).toString().matches("Sent.*")) {
                bS = true;
                S[counter] = Math.abs(Double.parseDouble((String) table.getValueAt(j,3)));
                Sent_BTC += S[counter]; 
                counter++;
            }  
            
        }
        //calculate max, min, avg BTC for each transaction type:
        if (bR){
            Arrays.sort(R);
            max_r = R[R.length-1];
            min_r = R[0];
            average_r = (double) Receieved_BTC / R.length;   
        }
        if (bS){
            Arrays.sort(S);
            max_s = S[S.length-1];
            min_s = S[0];
            average_s = (double) Sent_BTC / S.length;   
        }
        
        //display the basic statistics:
        Object [] objects = {"Total Number of Transactions: " + totalTransactions,
        "......................................................",
        "# Received Transactions:",
        "- Total BTC: " + Receieved_BTC,
        "- Maximum BTC: " + max_r,
        "- Minimum BTC: " + min_r,
        "- Average Transactions: " + average_r,
        "......................................................",
        "# Sent Transactions:",
        "- Total BTC: " + Sent_BTC,
        "- Maximum BTC: " + max_s,
        "- Minimum BTC: " + min_s,
        "- Average Transactions: " + average_s,
        };
        JOptionPane.showMessageDialog(null, objects,
                "Transactions Statistics", JOptionPane.PLAIN_MESSAGE);

    }
}

