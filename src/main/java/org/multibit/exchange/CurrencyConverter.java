package org.multibit.exchange;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.swing.SwingUtilities;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.joda.money.format.MoneyAmountStyle;
import org.joda.money.format.MoneyFormatter;
import org.joda.money.format.MoneyFormatterBuilder;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.view.ticker.TickerTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public enum CurrencyConverter {
    INSTANCE;

    
    private static final Logger log = LoggerFactory.getLogger(CurrencyConverter.class);

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
     * The exchange rate i.e the value of 1 BTC in the currency.
     */
    private BigDecimal rate;
    
    /**
     * Map of currency code to currency info.
     */
    private Map<String, CurrencyInfo> currencyCodeToInfoMap;
    
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
        
        // Initialise currency info map.
        currencyCodeToInfoMap = new HashMap<String, CurrencyInfo>();
        currencyCodeToInfoMap.put("USD", new CurrencyInfo("USD", "$", true));
        currencyCodeToInfoMap.put("AUD", new CurrencyInfo("AUD", "AU$", true));
        currencyCodeToInfoMap.put("CAD", new CurrencyInfo("CAD", "CA$", true));
        currencyCodeToInfoMap.put("NZD", new CurrencyInfo("NZD", "NZ$", true));
        currencyCodeToInfoMap.put("SGD", new CurrencyInfo("SGD", "SG$", true));
        currencyCodeToInfoMap.put("HKD", new CurrencyInfo("HKD", "HK$", true));

        currencyCodeToInfoMap.put("GBP", new CurrencyInfo("GBP", "£", true));
        currencyCodeToInfoMap.put("EUR", new CurrencyInfo("EUR", "\u20AC", true));
        currencyCodeToInfoMap.put("CHF", new CurrencyInfo("CHF", "Fr.", true));
        currencyCodeToInfoMap.put("JPY", new CurrencyInfo("JPY", "\u00A5", true));
        currencyCodeToInfoMap.put("CNY", new CurrencyInfo("CNY", "\u5143", false));
        currencyCodeToInfoMap.put("RUB", new CurrencyInfo("RUB", "\u0440\u0443\u0431", false));
        currencyCodeToInfoMap.put("SEK", new CurrencyInfo("SEK", "\u006B\u0072", true));
        currencyCodeToInfoMap.put("DKK", new CurrencyInfo("DKK", "\u006B\u0072.", true));
        currencyCodeToInfoMap.put("THB", new CurrencyInfo("THB", "\u0E3F", true));
        currencyCodeToInfoMap.put("PLN", new CurrencyInfo("PLN", "\u007A\u0142", false));
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
    
    public Money convertToBTC(String fiat) {
        if (rate == null || rate.equals(BigDecimal.ZERO)) {
            return null;
        } else {    
            Money btcAmount = null;
            
            DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
            formatter.setParseBigDecimal(true);
            try {
                BigDecimal parsedFiat = (BigDecimal)formatter.parse(fiat);
                Money fiatMoney = Money.of(currencyUnit, parsedFiat);
                btcAmount = fiatMoney.convertedTo(BITCOIN_CURRENCY_UNIT, new BigDecimal(NUMBER_OF_SATOSHI_IN_ONE_BITCOIN).divide(rate, BITCOIN_CURRENCY_UNIT.getDecimalPlaces() + 2, RoundingMode.HALF_EVEN), RoundingMode.HALF_EVEN);
            } catch (ParseException pe) {
                log.debug(pe.toString());
            }
            return btcAmount;
        }
    }
    
    public Money convertToMoney(String fiat) {
        if (rate == null || rate.equals(BigDecimal.ZERO)) {
            return null;
        } else {    
            DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
            formatter.setParseBigDecimal(true);
            try {
                BigDecimal parsedFiat = (BigDecimal)formatter.parse(fiat);
                Money fiatMoney = Money.of(currencyUnit, parsedFiat);
                return fiatMoney;
            } catch (ParseException pe) {
                log.debug(pe.toString());
            }
            return null;
        }
    }
    
    public boolean isShowingFiat() {
        return !Boolean.FALSE.toString().equals(controller.getModel().getUserPreference(MultiBitModel.SHOW_BITCOIN_CONVERTED_TO_FIAT));
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
    
    private MoneyFormatter getMoneyFormatter(boolean addCurrencySymbol) {
        MoneyFormatter moneyFormatter;
        
        // Suffix currency codes.
        String currencyCode = currencyUnit.getCurrencyCode();
        CurrencyInfo currencyInfo = currencyCodeToInfoMap.get(currencyCode);
        if (currencyInfo == null) {
            currencyInfo = new CurrencyInfo(currencyCode, currencyCode, true);
        }

        DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
        char separator = formatter.getDecimalFormatSymbols().getDecimalSeparator();
        MoneyAmountStyle moneyAmountStyle;
        if ('.' == separator) {
            moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_POINT_GROUP3_COMMA;
        } else {
            moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_COMMA_GROUP3_DOT;
        }
        
        if (currencyInfo.isPrefix()) {
            // Prefix currency code.
            if (addCurrencySymbol) {
                moneyFormatter = new MoneyFormatterBuilder().appendLiteral(currencyInfo.getCurrencySymbol()).appendAmount(moneyAmountStyle).toFormatter(controller.getLocaliser().getLocale());
            } else {
                moneyFormatter = new MoneyFormatterBuilder().appendAmount(moneyAmountStyle).toFormatter(controller.getLocaliser().getLocale());
            }
        } else {
             // Postfix currency code.
            if (addCurrencySymbol) {
                moneyFormatter = new MoneyFormatterBuilder().appendAmount(moneyAmountStyle).appendLiteral(currencyInfo.getCurrencySymbol()).toFormatter(controller.getLocaliser().getLocale());
            } else {
                moneyFormatter = new MoneyFormatterBuilder().appendAmount(moneyAmountStyle).toFormatter(controller.getLocaliser().getLocale());
            }
        }
        return moneyFormatter;
    }
    
    public String getMoneyAsString(Money money) {
        return getMoneyAsString(money, true);
    }
    
    public String getMoneyAsString(Money money, boolean addCurrencySymbol) {
        if (money == null) {
            return "";
        }
        MoneyFormatter moneyFormatter = getMoneyFormatter(addCurrencySymbol);
        
        DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
        char groupingSeparator = formatter.getDecimalFormatSymbols().getGroupingSeparator();

        String toReturn =  moneyFormatter.print(money);
        
        // Get rid of negative sign followed by thousand separator
        toReturn = toReturn.replaceAll("-" + groupingSeparator, "-");
        return toReturn;
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
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                if (listeners != null) {
                    for (CurrencyConverterListener listener : listeners) {
                        listener.foundExchangeRate(new ExchangeRate(currencyUnit, rate, new Date()));
                    }
                }
            }
        });
    }
    
    private void notifyUpdatedExchangeRate() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                if (listeners != null) {
                    for (CurrencyConverterListener listener : listeners) {
                        listener.updatedExchangeRate(new ExchangeRate(currencyUnit, rate, new Date()));
                    }
                }
            }
        });
    }

    public Map<String, CurrencyInfo> getCurrencyCodeToInfoMap() {
        return currencyCodeToInfoMap;
    }
}
