package org.multibit.viewsystem.dataproviders;

import java.awt.Font;


/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface PreferencesDataProvider { 
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
     * Get the previous open URI dialog text
     */
    public String getPreviousOpenUriDialog();
    
    /**
     * Get the new open URI dialog text
     */
    public String getNewOpenUriDialog();
    
    
    /**
     * Get the previous open URI use URI text
     */
    public String getPreviousOpenUriUseUri();
    
    /**
     * Get the new open URI use URI text
     */
    public String getNewOpenUriUseUri();
    
    
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
}
