package org.multibit.viewsystem.swing;

import com.google.bitcoin.core.StoredBlock;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletTableData;

import javax.swing.table.AbstractTableModel;
import java.math.BigInteger;
import java.util.Date;
import java.util.Vector;

public class WalletTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012854496208L;

    private Vector<String> headers;

    private Vector<WalletTableData> walletData;

    /**
     * the MultiBit model
     */
    private MultiBitModel multiBitModel;

    private MultiBitController controller;

    public WalletTableModel(MultiBitController controller) {
        this.multiBitModel = controller.getModel();
        this.controller = controller;

        createHeaders();

        walletData = multiBitModel.createWalletData(controller.getModel().getActiveWalletFilename());
    }

    public int getColumnCount() {
        return WalletTableData.COLUMN_HEADER_KEYS.length;
    }

    public int getRowCount() {
        return walletData.size();
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        WalletTableData walletDataRow = null;
        if (row >= 0 && row < walletData.size()) {
            walletDataRow = walletData.get(row);
        }
        if (walletDataRow == null) {
            return null;
        }

        switch (column) {
        case 0: {
            // work out the difference between the wallet data height and the
            // current head
            StoredBlock currentHead;
            if (controller.getMultiBitService() != null && controller.getMultiBitService().getChain() != null
                    && controller.getMultiBitService().getChain().getChainHead() != null) {
                currentHead = controller.getMultiBitService().getChain().getChainHead();
            } else {
                return 0; // we do not know yet
            }
            int currentHeight = Integer.MIN_VALUE;
            if (currentHead != null) {
                currentHeight = currentHead.getHeight();
            }
            if (walletDataRow.getHeight() != -1) {
              return currentHeight - walletDataRow.getHeight() + 1;
            } else {
                // do not know the height - probably a send that is not
                // confirmed
                return 0; // not confirmed yet
            }
        }
        case 1: {
            if (walletDataRow.getDate() == null) {
                return new Date(0); // the earliest date (for sorting)
            } else {
            }
            return walletDataRow.getDate();
        }
        case 2:
            return walletDataRow.getDescription();
        case 3:
            BigInteger debitAmount = walletDataRow.getDebit();
            if (debitAmount == null) {
                return null;
            } else {
                return controller.getLocaliser().bitcoinValueToString4(debitAmount, false, true);
            }
        case 4:
            BigInteger creditAmount = walletDataRow.getCredit();
            if (creditAmount == null) {
                return null;
            } else {
                return controller.getLocaliser().bitcoinValueToString4(creditAmount, false, true);
            }
        default:
            return null;
        }
    }

    /**
     * table model is read only
     */
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    public void recreateWalletData() {
        // recreate the wallet data as the underlying wallet has changed
        walletData = multiBitModel.createWalletData(controller.getModel().getActiveWalletFilename());
        this.fireTableDataChanged();
    }

    public void createHeaders() {
        // TODO Consider an ArrayList if possible
        headers = new Vector<String>();
        for (int j = 0; j < WalletTableData.COLUMN_HEADER_KEYS.length; j++) {
            headers.add(controller.getLocaliser().getString(WalletTableData.COLUMN_HEADER_KEYS[j]));
        }
    }
}
