package org.multibit.exchange;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.joda.money.format.MoneyAmountStyle;
import org.joda.money.format.MoneyFormatter;
import org.joda.money.format.MoneyFormatterBuilder;
import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
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
    public static final int NUMBER_OF_DECIMAL_POINTS_IN_A_BITCOIN = 8;
    
    // Extra digits used in calculation.
    public static final int ADDITIONAL_CALCULATION_DIGITS = 16;
   
    // This is the Bitcoin currency unit, denominated in satoshi with 0 decimal places.
    public static CurrencyUnit BITCOIN_CURRENCY_UNIT  = CurrencyUnit.of("BTC");
    
    /**
     * The currency unit for the currency being converted.
     */
    private CurrencyUnit currencyUnit = CurrencyUnit.of(TickerTableModel.DEFAULT_CURRENCY);

    /**
     * The exchange rate i.e the value of 1 BTC in the currency.
     */
    private BigDecimal rate;
    
    /**
     * A label onto which to put error messages
     */
    private JLabel notificationLabel;
    
    /**
     * Map of currency code to currency info.
     */
    private Map<String, CurrencyInfo> currencyCodeToInfoMap;

    public void initialise(MultiBitController controller, JLabel notificationLabel) {
        // Initialise conversion currency.
        String currencyCode = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY);
        initialise(controller, currencyCode, notificationLabel);
    }
    
    public void initialise(MultiBitController controller, String currencyCode, JLabel notificationLabel) {
       this.controller = controller;
       this.notificationLabel = notificationLabel;
        
       if (currencyCode != null && !"".equals(currencyCode)) {
           currencyUnit = CurrencyUnit.of(currencyCode);
       }         
        // Exchange rate is unknown.
        rate = null;
        
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

        currencyCodeToInfoMap.put("GBP", new CurrencyInfo("GBP", "\u00A3", true));
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
    public Money convertFromBTCToFiat(BigInteger bitcoinAmountInSatoshi) {
        if (rate == null) {
            return null;
        } else {
            Money bitcoin = Money.of(BITCOIN_CURRENCY_UNIT, new BigDecimal(bitcoinAmountInSatoshi));
            
            Money fiatAmount = bitcoin.convertedTo(currencyUnit, rate.divide(new BigDecimal(CurrencyConverter.NUMBER_OF_SATOSHI_IN_ONE_BITCOIN)), RoundingMode.HALF_EVEN);
            
            return fiatAmount;
        }
    }
    
    public Money convertFromFiatToBTC(String fiat) {
        if (rate == null || rate.equals(BigDecimal.ZERO)) {
            return null;
        } else {  
            
            if (fiat == null || fiat.trim().equals("")) {
                return null;    
            }
            
            Money btcAmount = null;
            
            DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
            formatter.setParseBigDecimal(true);
            
            // Convert spaces to non breakable space.
            fiat = fiat.replaceAll(" ", "\u00A0");

            try {
                BigDecimal parsedFiat = (BigDecimal)formatter.parse(fiat);
                Money fiatMoney = Money.of(currencyUnit, parsedFiat);
                btcAmount = fiatMoney.convertedTo(BITCOIN_CURRENCY_UNIT, new BigDecimal(NUMBER_OF_SATOSHI_IN_ONE_BITCOIN).divide(rate, BITCOIN_CURRENCY_UNIT.getDecimalPlaces() + ADDITIONAL_CALCULATION_DIGITS, RoundingMode.HALF_EVEN), RoundingMode.HALF_EVEN);
                setNotification("");
            } catch (ParseException pe) {
                log.debug("convertFromFiatToBTC: " + pe.getClass().getName() + " "  + pe.getMessage());
                setNotification(controller.getLocaliser().getString("currencyConverter.couldNotUnderstandAmount",
                        new Object[]{fiat}));
            } catch (ArithmeticException ae) {
                log.debug("convertFromFiatToBTC: " + ae.getClass().getName() + " "  + ae.getMessage());
                String currencyString = currencyUnit.getCurrencyCode();
                if (currencyCodeToInfoMap.get(currencyString) != null) {
                    currencyString = currencyCodeToInfoMap.get(currencyString).getCurrencySymbol();
                }
                setNotification(controller.getLocaliser().getString("currencyConverter.fiatCanOnlyHaveSetDecimalPlaces",
                        new Object[]{currencyString, currencyUnit.getDecimalPlaces()}));
            }
            log.debug("For locale " + controller.getLocaliser().getLocale().toString() +  ", '" + fiat + "' fiat converts to " + btcAmount);

            return btcAmount;
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
        char decimalSeparator = formatter.getDecimalFormatSymbols().getDecimalSeparator();
        char groupingSeparator = formatter.getDecimalFormatSymbols().getGroupingSeparator();
        MoneyAmountStyle moneyAmountStyle;
        if ('.' == decimalSeparator) {
            if (',' == groupingSeparator) {
                moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_POINT_GROUP3_COMMA;
            } else if (' ' == groupingSeparator || '\u00A0' == groupingSeparator) {
                moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_POINT_GROUP3_SPACE;
            } else {
                moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_POINT_NO_GROUPING;
            }
        } else {
            if (',' == decimalSeparator) {
                if ('.' == groupingSeparator) {
                    moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_COMMA_GROUP3_DOT;
                } else if (' ' == groupingSeparator || '\u00A0' == groupingSeparator) {
                    moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_COMMA_GROUP3_SPACE;
                } else {
                    moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_COMMA_NO_GROUPING;
                }
            } else {
                // Do not really know - keep it simple.
                moneyAmountStyle = MoneyAmountStyle.ASCII_DECIMAL_POINT_NO_GROUPING;                
            }
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
    
    public String getFiatAsLocalisedString(Money money) {
        return getFiatAsLocalisedString(money, true, false);
    }
    
    public String getFiatAsLocalisedString(Money money, boolean addCurrencySymbol, boolean addParenthesis) {
        if (money == null) {
            return "";
        }
        MoneyFormatter moneyFormatter = getMoneyFormatter(addCurrencySymbol);
        
        DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
        String groupingSeparator = String.valueOf(formatter.getDecimalFormatSymbols().getGroupingSeparator());

        String toReturn =  moneyFormatter.print(money);
        
        // Get rid of negative sign followed by thousand separator
        if (".".equals(groupingSeparator)) {
             // Escape regex.
            groupingSeparator = "\\.";
        }
        toReturn = toReturn.replaceAll("-" + groupingSeparator, "-");
        
        if (addParenthesis) {
            toReturn = "  (" + toReturn + ")";
        }
        return toReturn;
    }
    
    public String getBTCAsLocalisedString(Money btcMoney) {
        DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
        formatter.setMaximumFractionDigits(NUMBER_OF_DECIMAL_POINTS_IN_A_BITCOIN);
        String btcString = formatter.format(btcMoney.getAmount().divide(new BigDecimal(NUMBER_OF_SATOSHI_IN_ONE_BITCOIN)));
        return btcString;
    }
    
    public Money parseToFiat(String fiat) {
        DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(controller.getLocaliser().getLocale());
        formatter.setParseBigDecimal(true);

        // Convert spaces to non breakable space.
        fiat = fiat.replaceAll(" ", "\u00A0");

        try {
            BigDecimal parsedFiat = (BigDecimal) formatter.parse(fiat);
            Money fiatMoney = Money.of(currencyUnit, parsedFiat);
            setNotification("");
            return fiatMoney;
        } catch (ParseException pe) {
            log.debug("convertToMoney: " + pe.getClass().getName() + " " + pe.getMessage());
            setNotification(controller.getLocaliser().getString("currencyConverter.couldNotUnderstandAmount",
                    new Object[]{fiat}));
        } catch (ArithmeticException ae) {
            log.debug("convertToMoney: " + ae.getClass().getName() + " " + ae.getMessage());
            String currencyString = currencyUnit.getCurrencyCode();
            if (currencyCodeToInfoMap.get(currencyString) != null) {
                currencyString = currencyCodeToInfoMap.get(currencyString).getCurrencySymbol();
            }
            setNotification(controller.getLocaliser().getString("currencyConverter.fiatCanOnlyHaveSetDecimalPlaces",
                    new Object[]{currencyString, currencyUnit.getDecimalPlaces()}));
        }
        return null;
    }
    
    /**
     * Parse a localised string and returns a Money denominated in Satoshi
     * @param btcString
     * @return
     */
    public Money parseToBTC(String btcString) {
        return parseToBTC(btcString, controller.getLocaliser().getLocale());
    }
    
    /**
     * Parse a non localised string and returns a Money denominated in Satoshi
     * @param btcString
     * @return
     */
    public Money parseToBTCNotLocalised(String btcString) {
        return parseToBTC(btcString, Locale.ENGLISH);
    }

    private Money parseToBTC(String btcString, Locale locale) {
        if (btcString == null || btcString.equals("")) {
            return null;
        }
        
        // Convert spaces to non breakable space.
        btcString = btcString.replaceAll(" ", "\u00A0");
        
        Money btcAmount = null;
        
        DecimalFormat formatter = (DecimalFormat) DecimalFormat.getInstance(locale);
        formatter.setParseBigDecimal(true);
        try {
            BigDecimal parsedBTC = ((BigDecimal)formatter.parse(btcString)).movePointRight(NUMBER_OF_DECIMAL_POINTS_IN_A_BITCOIN);
            log.debug("For locale " + controller.getLocaliser().getLocale().toString() +  ", '" + btcString + "' parses to " + parsedBTC.toPlainString());
            btcAmount = Money.of(BITCOIN_CURRENCY_UNIT, parsedBTC);
            setNotification("");
        } catch (ParseException pe) {
            log.debug("parseToBTC: " + pe.getClass().getName() + " " + pe.getMessage());
            setNotification(controller.getLocaliser().getString("currencyConverter.couldNotUnderstandAmount",
                    new Object[]{btcString}));
        } catch (ArithmeticException ae) {
            log.debug("parseToBTC: " + ae.getClass().getName() + " " + ae.getMessage());
            setNotification(controller.getLocaliser().getString("currencyConverter.btcCanOnlyHaveEightDecimalPlaces"));
        }
        return btcAmount;
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
    
    private void setNotification(final String notification) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (notificationLabel != null) {
                    notificationLabel.setText(notification);
                }
            }
        });
    }

    public JLabel getNotificationLabel() {
        return notificationLabel;
    }

    public void setNotificationLabel(JLabel notificationLabel) {
        this.notificationLabel = notificationLabel;
    }
}
