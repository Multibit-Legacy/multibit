package org.multibit.viewsystem.dataproviders.preferences;



/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface BitcoinPreferencesDataProvider extends PreferencesDataProvider { 

    /**
     * Get the previous send fee
     */
    public String getPreviousSendFee();
    
    /**
     * Get the new send fee
     */
    public String getNewSendFee();
    
    /**
     * Get the new open URI dialog text
     */
    public String getOpenUriDialog();
       
    /**
     * Get the new open URI use URI text
     */
    public String getOpenUriUseUri();
    
}
