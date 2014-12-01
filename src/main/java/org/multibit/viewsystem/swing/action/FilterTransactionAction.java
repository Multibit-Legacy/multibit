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
package org.multibit.viewsystem.swing.action;

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import javax.swing.*;
import java.awt.event.ActionEvent;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

/**
 */
public class FilterTransactionAction extends AbstractAction {

    private MultiBitFrame mainFrame;
    BitcoinController bitcoinController;
    JTextField toDate;
    JTextField fromDate;
    JComboBox transactionT;
    TableRowSorter<TableModel> rowSorter;
    JTable table;


    /**
     * Creates a new {@link ExportPrivateKeysSubmitAction}.
     */ 
    public FilterTransactionAction(BitcoinController bitcoinController, MultiBitFrame mainFrame, JTable table, TableRowSorter<TableModel> rowSorter, JTextField toDate, JTextField fromDate, JComboBox transactionT) {
        this.mainFrame = mainFrame;
        this.bitcoinController = bitcoinController;
        this.toDate = toDate;
        this.fromDate = fromDate;
        this.transactionT = transactionT;
        this.rowSorter = rowSorter;
        this.table = table;
    }

    /**
     */
    public void actionPerformed(ActionEvent e) {
        SimpleDateFormat sdf = new SimpleDateFormat("E MMM dd HH:mm:ss Z yyyy");
        SimpleDateFormat sdf2 = new SimpleDateFormat("dd MMM yyyy");
        List<RowFilter<Object,Object>> filters = new ArrayList<RowFilter<Object,Object>>();
        RowFilter<TableModel, Object> rf = null;
        if(!transactionT.getSelectedItem().toString().equals("Both")){
            filters.add(RowFilter.regexFilter(transactionT.getSelectedItem().toString(), 2));
        }
        try {
            String dateFormat = fromDate.getText();
            Date date = sdf2.parse(dateFormat);
            String strDateFormat = sdf.format(date);
            String dateArr[] = strDateFormat.split(" ");
            dateArr[3]= "23:59:59";
            String collectDay = dateArr[0]+" " +dateArr[1]+" " +dateArr[2]+ " " +dateArr[3]+" " +dateArr[4]+" " +dateArr[5];
            Date rightDateFormat = sdf.parse(collectDay);
            filters.add(RowFilter.dateFilter(RowFilter.ComparisonType.BEFORE, rightDateFormat, 1));  
        } catch (ParseException ex) {
        }

        try {
            String dateFormatTo = toDate.getText();
            Date dateTo = sdf2.parse(dateFormatTo);
            String strDateFormatTo = sdf.format(dateTo);
            Date rightDateFormatTo = sdf.parse(strDateFormatTo);
            filters.add(RowFilter.dateFilter(RowFilter.ComparisonType.AFTER, rightDateFormatTo, 1));
                
        } catch (ParseException ex) {
        }
        rf = RowFilter.andFilter(filters);
        rowSorter.setRowFilter(rf);
        table.setRowSorter(rowSorter);
    }
}