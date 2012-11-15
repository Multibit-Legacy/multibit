package org.multibit.exchange;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.joda.money.format.MoneyAmountStyle;
import org.joda.money.format.MoneyFormatter;
import org.joda.money.format.MoneyFormatterBuilder;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;

public enum CurrencyConverter {
    INSTANCE;

    private MultiBitController controller;
    
    private Collection<CurrencyConverterListener> listeners;
    
    public static final BigInteger NUMBER_OF_SATOSHI_IN_ONE_BITCOIN = BigInteger.valueOf(100000000); // 8 zeros
   
    // This is the Bitcoin currency unit, denominated in satoshi with 0 decimal places.
    private static CurrencyUnit BITCOIN_CURRENCY_UNIT;
    
    /**
     * The currency unit for the currency being converted.
     */
    private CurrencyUnit currencyUnit = CurrencyUnit.of(TickerTableModel.DEFAULT_CURRENCY);

    /**
     * The exchange rate i.e the value of 1 BTC in the currency
     */
    private BigDecimal rate;
    
    public void initialise(MultiBitController controller) {
        this.controller = controller;
        
        // Initialise conversion currency.
        String currency1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY);
        if (currency1 != null && !"".equals(currency1)) {
            currencyUnit = CurrencyUnit.of(currency1);
        }
        
        // Exchange rate is unknown.
        rate = null;
        
        BITCOIN_CURRENCY_UNIT = CurrencyUnit.of("BTC");
        
        // Setup listeners
        listeners = new ArrayList<CurrencyConverterListener>();
    }

    /**
     * Convert a number of satoshis to fiat
     * @param bitcoinAmount in satoshis
     * @return equivalent fiat amount
     */
    public Money convertToFiat(BigInteger bitcoinAmountInSatoshi) {
        if (rate == null) {
            return null;
        } else {
            Money bitcoin = Money.of(BITCOIN_CURRENCY_UNIT, new BigDecimal(bitcoinAmountInSatoshi));
            
            Money fiatAmount = bitcoin.convertedTo(currencyUnit, rate.divide(new BigDecimal(CurrencyConverter.NUMBER_OF_SATOSHI_IN_ONE_BITCOIN)), RoundingMode.HALF_EVEN);
            
            return fiatAmount;
        }
    }
    
    public CurrencyUnit getCurrencyUnit() {
        return currencyUnit;
    }

    public void setCurrencyUnit(CurrencyUnit currencyUnit) {
        // If this is a new currency, blank the rate.
        // Thus you should set the currency unit first.
        if (this.currencyUnit != null && !this.currencyUnit.equals(currencyUnit)) {
            rate = null;
        }
        this.currencyUnit = currencyUnit;
    }

    public BigDecimal getRate() {
        return rate;
    }

    public void setRate(BigDecimal rate) {
        boolean fireFoundInsteadOfUpdated = (rate== null);
        this.rate = rate;
        
        if (fireFoundInsteadOfUpdated) {
            notifyFoundExchangeRate();
        } else {
            notifyUpdatedExchangeRate();
        }
    }
    
    private MoneyFormatter getMoneyFormatter() {
        MoneyFormatter moneyFormatter = new MoneyFormatterBuilder().appendCurrencyCode()
        .appendAmount().toFormatter();
        return moneyFormatter;
    }
    
    public String getMoneyAsString(Money money) {
        MoneyFormatter moneyFormatter = getMoneyFormatter();
        String result = moneyFormatter.print(money);
        result = result.replaceAll("USD", "\\$");
        result = result.replaceAll("GBP", "£");
        result = result.replaceAll("EUR", "\u20AC");
               
        return result;
    }
    
    public void addCurrencyConverterListener(CurrencyConverterListener listener) {
        if (listeners == null) {
            throw new IllegalStateException("You need to initialise the CurrencyConverter first");
        }
        listeners.add(listener);
    }
    
    public void removeCurrencyConverterListener(CurrencyConverterListener listener) {
        if (listeners == null) {
            throw new IllegalStateException("You need to initialise the CurrencyConverter first");
        }
        listeners.remove(listener);
    }
    
    private void notifyFoundExchangeRate() {
        if (listeners != null) {
            for (CurrencyConverterListener listener : listeners) {
                listener.foundExchangeRate(new ExchangeRate(currencyUnit, rate, new Date()));
            }
        }
    }
    
    private void notifyUpdatedExchangeRate() {
        if (listeners != null) {
            for (CurrencyConverterListener listener : listeners) {
                listener.updatedExchangeRate(new ExchangeRate(currencyUnit, rate, new Date()));
            }
        }
    }
}
