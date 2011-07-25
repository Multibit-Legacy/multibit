package org.multibit.viewsystem;


/**
 * an interface describing a collection of views that are used to render the MultiBit application
 * @author jim
 *
 */
public interface ViewSystem {  
    /**
     * display the view specified
     * @param view to display - one of the View constants
     */   
    public void displayView(int viewToDisplay);
    
    /**
     * navigate away from a view - gives the view the opportunity to tidy up/ disappear etc
     * @param viewToNavigateAwayFrom - current view to navigate away from -one of the View constants
     * @param nextView - next view - one of the View constants
     */   
    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextView);
    
    /**
     * display a message to the user - using the current localiser
     * @param messageKey the key to localise for the message
     * @param messageData the data used in the messag
     * @param titleKey the key to localise for the title
     */   
    public void displayMessage(String messageKey, Object[] messageData, String titleKey);
    
    /**
     * tells the view system to recreate all views e.g. after a language change
     */   
    public void recreateAllViews();
}
