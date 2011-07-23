package org.multibit.viewsystem.swing;

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import org.multibit.Localiser;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletData;

import com.google.bitcoin.core.Wallet;

public class WalletTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012854496208L;

    private Vector<String> headers = new Vector<String>();

    /**
     * the MultiBit model
     */
    private MultiBitModel multiBitModel;

    SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy hh:mm");

    public WalletTableModel(MultiBitModel model, Localiser localiser) {
        this.multiBitModel = model;

        for (int j = 0; j < WalletData.COLUMN_HEADER_KEYS.length; j++) {
            headers.add(localiser.getString(WalletData.COLUMN_HEADER_KEYS[j]));
        }
    }

    public int getColumnCount() {
        return WalletData.COLUMN_HEADER_KEYS.length;
    }

    public int getRowCount() {
        return multiBitModel.getFakeWalletData().size();
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        WalletData walletData = null;
        if (row >= 0 && row < multiBitModel.getFakeWalletData().size()) {
            walletData = multiBitModel.getFakeWalletData().get(row);
        }
        if (walletData == null) {
            return null;
        }

        switch (column) {
        case 0:
            return walletData.getStatus();
        case 1:
            return dateFormatter.format(walletData.getDate());
        case 2:
            return walletData.getDescription();
        case 3:
            BigInteger debitAmount = walletData.getDebit();
            if (debitAmount == null) {
                return null;
            } else {
                return debitAmount.toString();
            }
        case 4:
            BigInteger creditAmount = walletData.getCredit();
            if (creditAmount == null) {
                return null;
            } else {
                return creditAmount.toString();
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

    /**
     * put a new wallet into the table
     */
    public void setWallet(Wallet wallet) {
        multiBitModel.setWallet(wallet);
        this.fireTableDataChanged();
    }
}
