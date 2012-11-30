package org.multibit.exchange;

import java.math.BigDecimal;
import java.util.Date;

import org.joda.money.CurrencyUnit;


public class ExchangeRate {
    /**
     * The currency unit of the exchange rate.
     */
    CurrencyUnit currencyUnit;
    
    /**
     * The value of one BTC in the currency.
     */
    BigDecimal rate;
    
    /**
     * The date at which the rate was valid.
     */
    Date rateDate;

    public ExchangeRate(CurrencyUnit currencyUnit, BigDecimal rate, Date rateDate) {
        super();
        this.currencyUnit = currencyUnit;
        this.rate = rate;
        this.rateDate = rateDate;
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

    public Date getRateDate() {
        return rateDate;
    }

    public void setRateDate(Date rateDate) {
        this.rateDate = rateDate;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((currencyUnit == null) ? 0 : currencyUnit.hashCode());
        result = prime * result + ((rate == null) ? 0 : rate.hashCode());
        result = prime * result + ((rateDate == null) ? 0 : rateDate.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof ExchangeRate))
            return false;
        ExchangeRate other = (ExchangeRate) obj;
        if (currencyUnit == null) {
            if (other.currencyUnit != null)
                return false;
        } else if (!currencyUnit.equals(other.currencyUnit))
            return false;
        if (rate == null) {
            if (other.rate != null)
                return false;
        } else if (!rate.equals(other.rate))
            return false;
        if (rateDate == null) {
            if (other.rateDate != null)
                return false;
        } else if (!rateDate.equals(other.rateDate))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "ExchangeRate [currencyUnit=" + currencyUnit + ", rate=" + rate + ", rateDate=" + rateDate + "]";
    }
}

