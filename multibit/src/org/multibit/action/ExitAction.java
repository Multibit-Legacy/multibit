package org.multibit.action;

import org.multibit.model.DataProvider;



/**
 * exit the application
 * @author jim
 *
 */
public class ExitAction implements Action{   
    public ExitAction() {    
    }
    
    public void execute(DataProvider dataProvider) {
       System.exit(0);     
    }
    
    public String getDisplayText() {
        // TODO localise
        return "exit";
    }
}


