package org.multibit.exchange;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
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
        System.out.println("BTC currency unit d.p.= " + BITCOIN_CURRENCY_UNIT.getDecimalPlaces());
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
        this.currencyUnit = currencyUnit;
    }

    public BigDecimal getRate() {
        return rate;
    }

    public void setRate(BigDecimal rate) {
        this.rate = rate;
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
    
//    private void test() {
//    CurrencyUnit canadianFrench = CurrencyUnit.getInstance(Locale.CANADA_FRENCH);
//    CurrencyUnit canadianEnglish = CurrencyUnit.getInstance(Locale.CANADA);
//
//    MoneyFormatter canadianFrenchFormat = new MoneyFormatterBuilder().
//            appendAmount(MoneyAmountStyle.ASCII_DECIMAL_COMMA_GROUP3_DOT).
//            appendLiteral(" $").
//            toFormatter();
//    MoneyFormatter canadianEnglishFormat = new MoneyFormatterBuilder().
//            appendLiteral("$").
//            appendAmount(MoneyAmountStyle.ASCII_DECIMAL_POINT_GROUP3_COMMA).
//            toFormatter();
//
//    System.out.println(canadianFrenchFormat.print(Money.of(canadianFrench, 123456789.99)));
//    System.out.println(canadianEnglishFormat.print(Money.of(canadianEnglish, 123456789.99)));
//    }
}
