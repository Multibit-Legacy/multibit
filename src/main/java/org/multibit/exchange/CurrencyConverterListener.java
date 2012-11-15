package org.multibit.exchange;

public interface CurrencyConverterListener {

    /**
     * The exchange rate is no longer valid as it is out of date
     */
    public void lostExchangeRate(ExchangeRate exchangeRate);
    
    /**
     * The exchange rate is now valid (for the first time)
     * @param exchangeRate
     */
    public void foundExchangeRate(ExchangeRate exchangeRate);
    
    /**
     * An updated value of the exchange rate is available
     * @param exchangeRate
     */
    public void updatedExchangeRate(ExchangeRate exchangeRate);
}
