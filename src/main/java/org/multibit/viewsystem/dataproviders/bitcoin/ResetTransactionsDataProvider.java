package org.multibit.viewsystem.dataproviders.bitcoin;

import java.util.Date;
import org.multibit.viewsystem.dataproviders.DataProvider;

/**
 * DataProvider for Reset blockchain and transactions action
 * @author jim
 *
 */
public interface ResetTransactionsDataProvider extends DataProvider{

    /**
     * Is the reset to be from the date of the first transaction ?
     */
    public boolean isResetFromFirstTransaction();
    
    /**
     * Get the date from which to do the reset
     */
    public Date getResetDate();
}
