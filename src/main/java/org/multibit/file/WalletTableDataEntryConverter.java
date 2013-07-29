package org.multibit.file;

import java.math.BigInteger;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import org.joda.money.Money;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.model.bitcoin.WalletTableData;

import com.googlecode.jcsv.writer.CSVEntryConverter;

/**
 * Convert WalletTableData into single fields for use in a CSV file.
 */
public class WalletTableDataEntryConverter implements CSVEntryConverter<WalletTableData> {

    BitcoinController bitcoinController = null;
    DateFormat dateFormatter = null;

    @Override
    public String[] convertEntry(WalletTableData walletTableData) {
        String[] columns = new String[5];

        // Date.
        String formattedDate = "";
        if (walletTableData.getDate() != null) {
            if (walletTableData.getDate().getTime() != 0) {
                try {
                    formattedDate = dateFormatter.format(walletTableData.getDate());
                } catch (IllegalArgumentException iae) {
                    // ok
                }
            }
        }
        columns[0] = formattedDate;

        // Description.
        columns[1] = walletTableData.getDescription() == null ? "" : walletTableData.getDescription();

        // Amount in BTC.
        String amountBTC = "";
        BigInteger debitAmount = walletTableData.getDebit();
        BigInteger creditAmount = walletTableData.getCredit();
        if (debitAmount != null && debitAmount.compareTo(BigInteger.ZERO) > 0) {
            amountBTC = bitcoinController.getLocaliser().bitcoinValueToString(debitAmount.negate(), false, true);
        } else {
            if (creditAmount != null) {
                amountBTC = bitcoinController.getLocaliser().bitcoinValueToString(creditAmount, false, true);
            }
        }
        columns[2] = amountBTC;

        // Amount in fiat
        String amountFiat = "";
        if (CurrencyConverter.INSTANCE.isShowingFiat()) {
            if (walletTableData.getDebit() != null && walletTableData.getDebit().compareTo(BigInteger.ZERO) > 0) {
                Money debitAmountFiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(walletTableData.getDebit());
                if (debitAmountFiat != null) {
                    amountFiat = CurrencyConverter.INSTANCE.getFiatAsLocalisedString(debitAmountFiat.negated(), false, false);
                }
            } else {
                Money creditAmountFiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(walletTableData.getCredit());
                if (creditAmountFiat != null) {
                    amountFiat = CurrencyConverter.INSTANCE.getFiatAsLocalisedString(creditAmountFiat, false, false);
                }
            }
        }
        columns[3] = amountFiat;

        // Transaction hash.
        columns[4] = walletTableData.getTransaction() == null ? "" : walletTableData.getTransaction().getHashAsString();

        return columns;
    }

    public void setBitcoinController(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", bitcoinController.getLocaliser().getLocale());
    }
}
