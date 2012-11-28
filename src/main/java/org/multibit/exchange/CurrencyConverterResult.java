package org.multibit.exchange;

import org.joda.money.Money;

/**
 * A pojo to store the result of a currency conversion.
 * 
 * The fiat and btc are kept separate just to avoid mixing them up accidentally.
 */
public class CurrencyConverterResult {
    
    private boolean fiatMoneyValid;
    
    private Money fiatMoney;
    
    private String fiatMessage;
    
    private boolean btcMoneyValid;
    
    private Money btcMoney;
    
    private String btcMessage;

    public CurrencyConverterResult() {
        fiatMoneyValid = false;
        fiatMoney = null;
        fiatMessage = null;
        
        btcMoneyValid = false;
        btcMoney = null;
        btcMessage = null;
    }

    public boolean isFiatMoneyValid() {
        return fiatMoneyValid;
    }

    public void setFiatMoneyValid(boolean fiatMoneyValid) {
        this.fiatMoneyValid = fiatMoneyValid;
    }

    public Money getFiatMoney() {
        return fiatMoney;
    }

    public void setFiatMoney(Money fiatMoney) {
        this.fiatMoney = fiatMoney;
    }

    public String getFiatMessage() {
        return fiatMessage;
    }

    public void setFiatMessage(String fiatMessage) {
        this.fiatMessage = fiatMessage;
    }

    public boolean isBtcMoneyValid() {
        return btcMoneyValid;
    }

    public void setBtcMoneyValid(boolean btcMoneyValid) {
        this.btcMoneyValid = btcMoneyValid;
    }

    public Money getBtcMoney() {
        return btcMoney;
    }

    public void setBtcMoney(Money btcMoney) {
        this.btcMoney = btcMoney;
    }

    public String getBtcMessage() {
        return btcMessage;
    }

    public void setBtcMessage(String btcMessage) {
        this.btcMessage = btcMessage;
    }
}
