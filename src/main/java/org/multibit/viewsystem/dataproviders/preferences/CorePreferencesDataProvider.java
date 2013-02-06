package org.multibit.viewsystem.dataproviders.preferences;

import java.awt.Font;


/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface CorePreferencesDataProvider extends PreferencesDataProvider { 
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
