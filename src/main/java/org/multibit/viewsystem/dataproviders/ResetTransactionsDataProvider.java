package org.multibit.viewsystem.dataproviders;

import java.util.Date;

/**
 * DataProvider for Reset blockchain and transactions action
 * @author jim
 *
 */
public interface ResetTransactionsDataProvider {

    /**
     * Is the reset to be from the date of the first transaction ?
     */
    public boolean isResetFromFirstTransaction();
    
    /**
     * Get the date from which to do the reset
     */
    public Date getResetDate();
}
