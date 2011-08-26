package org.multibit.viewsystem.swing;

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletData;

import com.google.bitcoin.core.StoredBlock;

public class WalletTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012854496208L;

    private Vector<String> headers;

    private Vector<WalletData> walletData;

    /**
     * the MultiBit model
     */
    private MultiBitModel multiBitModel;

    private MultiBitController controller;

    SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm");

    public WalletTableModel(MultiBitController controller) {
        this.multiBitModel = controller.getModel();
        this.controller = controller;

        createHeaders();

        walletData = multiBitModel.createWalletData();
    }

    public int getColumnCount() {
        return WalletData.COLUMN_HEADER_KEYS.length;
    }

    public int getRowCount() {
        return walletData.size();
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        WalletData walletDataRow = null;
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
            StoredBlock currentHead = controller.getMultiBitService().getChain().getChainHead();
            int currentHeight = Integer.MIN_VALUE;
            if (currentHead != null) {
                currentHeight = currentHead.getHeight();
            }
            if (walletDataRow.getHeight() != -1) {
                int numberOfBlocksEmbedded = currentHeight - walletDataRow.getHeight() + 1;
                return numberOfBlocksEmbedded;
            } else {
                // do not know the height - probably a send that is not confirmed
                return 0; // not confirmed yet
            }
        }
        case 1: {
            if (walletDataRow.getDate() == null) {
                return new Date(0);   // the earliest date (for sorting)
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
                return Localiser.bitcoinValueToFriendlyString(debitAmount, false, true);
            }
        case 4:
            BigInteger creditAmount = walletDataRow.getCredit();
            if (creditAmount == null) {
                return null;
            } else {
                return Localiser.bitcoinValueToFriendlyString(creditAmount, false, true);
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
        walletData = multiBitModel.createWalletData();
        this.fireTableDataChanged();
    }

    public void createHeaders() {
        headers = new Vector<String>();
        for (int j = 0; j < WalletData.COLUMN_HEADER_KEYS.length; j++) {
            headers.add(controller.getLocaliser().getString(WalletData.COLUMN_HEADER_KEYS[j]));
        }
    }
}
