package org.multibit.viewsystem.dataproviders.bitcoin;

import org.multibit.viewsystem.dataproviders.*;
import java.awt.Font;


/**
 * DataProvider for the preferences screen and actions
 * @author jim
 *
 */
public interface BitcoinPreferencesDataProvider extends DataProvider { 
    /**
     * Get the previous send fee
     */
    public String getPreviousSendFee();
    
    /**
     * Get the new send fee
     */
    public String getNewSendFee();
}
