package org.multibit.viewsystem.dataproviders;


/**
 * DataProvider for send bitcoin and send bitcoin confirm action
 * @author jim
 *
 */
public interface BitcoinFormDataProvider { 
    /**
     * Get the address
     */
    public String getAddress();
    
    /**
     * Get the label
     */
    public String getLabel();
    
    /**
     * Get the amount
     */
    public String getAmount();
}
