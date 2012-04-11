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
    
    
    /**
     * Get the previous send fee
     */
    public String getPreviousSendFee();
    
    /**
     * Get the new send fee
     */
    public String getNewSendFee();
    
    
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
     * Get if the previous value for if midpoint currency rate is to be shown by the ticker
     * @returns true If rate to be shown, false if bid and ask are to be shown
     */
    public boolean getPreviousShowRate();
    
    /**
     * Get if the new value for if midpoint currency rate is to be shown by the ticker
     * @returns true If rate to be shown, false if bid and ask are to be shown
     */
    public boolean getNewShowRate();
    
    
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
}
