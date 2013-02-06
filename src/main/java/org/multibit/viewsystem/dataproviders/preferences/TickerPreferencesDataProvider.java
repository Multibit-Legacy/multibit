package org.multibit.viewsystem.dataproviders.preferences;


/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface TickerPreferencesDataProvider extends PreferencesDataProvider { 

    public boolean getPreviousShowCurrency();
    public boolean getNewShowCurrency();
    
    public boolean getPreviousShowRate();
    public boolean getNewShowRate();
    
    public boolean getPreviousShowBid();
    public boolean getNewShowBid();
    
    public boolean getPreviousShowAsk();
    public boolean getNewShowAsk();
    
    public boolean getPreviousShowExchange();
    public boolean getNewShowExchange();
    
    public String getPreviousExchange1();
    public String getNewExchange1();
    
    public String getPreviousCurrency1();
    public String getNewCurrency1();
 
    public boolean getPreviousShowSecondRow();
    public boolean getNewShowSecondRow();

    public String getPreviousExchange2();
    public String getNewExchange2();

    public String getPreviousCurrency2();
    public String getNewCurrency2();
    
    boolean isTickerVisible();

    boolean getPreviousShowTicker();
    boolean getNewShowTicker();
    
    boolean getPreviousShowBitcoinConvertedToFiat();
    boolean getNewShowBitcoinConvertedToFiat();
}
