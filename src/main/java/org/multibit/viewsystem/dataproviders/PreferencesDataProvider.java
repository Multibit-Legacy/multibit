package org.multibit.viewsystem.dataproviders;

import java.awt.Font;


/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface PreferencesDataProvider extends DataProvider { 
    /**
     * Get the previous 'undo changes' text
     */
    public String getPreviousUndoChangesText();
    
    
//    /**
//     * Get the previous send fee
//     */
//    public String getPreviousSendFee();
//    
//    /**
//     * Get the new send fee
//     */
//    public String getNewSendFee();
    
    
    /**
     * Get the previous user language code
     */
    public String getPreviousUserLanguageCode();
    
    /**
     * Get the new user language code
     */
    public String getNewUserLanguageCode();
    
      
    /**
     * Get the new open URI dialog text
     */
    public String getOpenUriDialog();
    
       
    /**
     * Get the new open URI use URI text
     */
    public String getOpenUriUseUri();
    
    
    /**
     * get the previous font name
     */
    public String getPreviousFontName();
    
    /**
     * get the new font name
     */
    public String getNewFontName();
    
    
    /**
     * get the previous font style
     */
    public String getPreviousFontStyle();
    
    /**
     * get the new font style
     */
    public String getNewFontStyle();
   
    /**
     * get the previous font size
     */
    public String getPreviousFontSize();
    
    /**
     * get the new font size
     */
    public String getNewFontSize();
    
    /**
     * get the new font
     */
    public Font getSelectedFont();

    /**
     * ticker information
     * @return
     */
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
    
    String getPreviousLookAndFeel();
    String getNewLookAndFeel();


    boolean getNewShowBitcoinConvertedToFiat();
    boolean getPreviousShowBitcoinConvertedToFiat();


    String getNewOpenExchangeRatesApiCode();


    String getPreviousOpenExchangeRatesApiCode();

    boolean getNewMinimizeToTray();
}
