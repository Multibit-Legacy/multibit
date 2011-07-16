package org.multibit;

import java.math.BigInteger;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;

public class WalletTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012854496208L;

    private Vector<String> headers = new Vector<String>();

    private final String[] tableHeaderKeys = new String[] { "walletTableModel.statusColumnHeader",
            "walletTableModel.dateColumnHeader", "walletTableModel.descriptionColumnHeader",
            "walletTableModel.debitColumnHeader", "walletTableModel.creditColumnHeader" };

    /**
     * the Wallet backing the WalletTableModel
     */
    private Wallet wallet;

    private Vector<WalletData> walletDataVector;
    SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy hh:mm");

    public WalletTableModel(Wallet wallet, Localiser localiser) {
        this.wallet = wallet;

        for (int j = 0; j < tableHeaderKeys.length; j++) {
            headers.add(localiser.getString(tableHeaderKeys[j]));
        }

        walletDataVector = createFakeWalletData();
    }

    public int getColumnCount() {
        return tableHeaderKeys.length;
    }

    public int getRowCount() {
        return walletDataVector.size();
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        WalletData walletData = null;
        if (row >= 0 && row < walletDataVector.size()) {
            walletData = walletDataVector.get(row);
        }
        if (walletData == null) {
            return null;
        }

        switch (column) {
        case 0:
            return walletData.status;
        case 1:
            return dateFormatter.format(walletData.date);
        case 2:
            return walletData.description;
        case 3:
            BigInteger debitAmount = walletData.debit;
            if (debitAmount == null) {
                return null;
            } else {
                return debitAmount.toString();
            }
        case 4:
            BigInteger creditAmount = walletData.credit;
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

    private Vector<WalletData> createFakeWalletData() {
        walletDataVector = new Vector<WalletData>();

        SimpleDateFormat formatter = new SimpleDateFormat("ddMMMyyyy hh:mm");

        // fake data
        WalletData walletData;
        try {
            walletData = new WalletData(null, "", formatter.parse("06jul2011 10:11"),
                    "Received with: Mt Gox account (1x256f9e9)", null, new BigInteger("11"));
            walletDataVector.add(walletData);

            walletData = new WalletData(null, "", formatter.parse("05jul2011 18:40"),
                    "To: Jose Alvaro (1x3f45ed)", new BigInteger("14"), null);
            walletDataVector.add(walletData);

            walletData = new WalletData(null, "", formatter.parse("04jul2011 16:40"),
                    "To: Joseph Jacks (1xq90)", new BigInteger("4"), null);
            walletDataVector.add(walletData);

            walletData = new WalletData(null, "", formatter.parse("03jul2011 14:20"),
                    "To: Michelle Jones (1j8s2)", new BigInteger("16"), null);
            walletDataVector.add(walletData);

            walletData = new WalletData(null, "", formatter.parse("02jul2011 09:10"),
                    "Received with: Jim's Amazon account (1xqs2)", null, new BigInteger("25"));
            walletDataVector.add(walletData);

            walletData = new WalletData(null, "", formatter.parse("01jul2011 10:30"),
                    "Received with: Google dividend (15x31)", null, new BigInteger("17"));
            walletDataVector.add(walletData);
        } catch (ParseException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return walletDataVector;
    }

    /**
     * put a new wallet into the table
     */
    public void setWallet(Wallet wallet) {
        this.wallet = wallet;
        createFakeWalletData();
        this.fireTableDataChanged();
    }

    /**
     * class used to store the data in the table in a quick to access form
     */
    class WalletData {
        Transaction transaction;
        String status;
        Date date;
        String description;
        BigInteger debit;
        BigInteger credit;

        WalletData(Transaction transaction, String status, Date date, String description,
                BigInteger debit, BigInteger credit) {
            this.transaction = transaction;
            this.status = status;
            this.date = date;
            this.description = description;
            this.debit = debit;
            this.credit = credit;
        }
    }
}
