package org.multibit.viewsystem.dataproviders;

import java.awt.Font;


/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface PreferencesDataProvider extends DataProvider{ 
    /**
     * Get the previous 'undo changes' text
     */
    public String getPreviousUndoChangesText();
    
    
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
    
    String getPreviousLookAndFeel();
    String getNewLookAndFeel();
}
