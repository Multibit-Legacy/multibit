package org.multibit.viewsystem.dataproviders;


/**
 * DataProvider for show open URI and cancel actions
 * @author jim
 *
 */
public interface ShowUriDialogDataProvider extends BitcoinFormDataProvider { 
    
    /**
     * Get the boolean dictating whether to show the open URI dialog or not
     */
    public boolean isShowUriDialog();
}
