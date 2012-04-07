package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;

import javax.swing.Icon;

import org.multibit.Localiser;
import org.multibit.viewsystem.commandline.ObseleteCommandLineViewSystem;

/**
 * the command line view for send bitcoin
 * 
 * @author jim
 * 
 */
public class SendBitcoinConfirmView extends AbstractView {
    public SendBitcoinConfirmView(Localiser localiser, String viewDescription, InputStream inputStream, PrintStream printStream) {
        super(localiser, viewDescription, inputStream, printStream);
    }

    /**
     * display the view to the user
     */
    @Override
    public void displayView() {
        printStream.println(ObseleteCommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX + ObseleteCommandLineViewSystem.DISPLAY_VIEW_PREFIX + description);        
 
        // show menu of actions and process response
        displayActionsAndProcessResponse();
   }

    @Override
    public void navigateAwayFromView() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Icon getViewIcon() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getViewTitle() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getViewTooltip() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int getViewId() {
        // TODO Auto-generated method stub
        return 0;
    }
}
