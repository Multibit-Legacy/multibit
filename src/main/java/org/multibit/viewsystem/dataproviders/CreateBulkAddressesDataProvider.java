package org.multibit.viewsystem.dataproviders;

/**
 * DataProvider for create bulk addresses action
 * @author jim
 *
 */
public interface CreateBulkAddressesDataProvider extends DataProvider {

    /**
     * Get the filename to output the addresses to
     */
    public String getOutputFilename();
    
    /**
     * Get the number of addresses to add to the file
     */
    public int getNumberOfAddresses();
}
