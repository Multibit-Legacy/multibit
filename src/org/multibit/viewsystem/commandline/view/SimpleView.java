package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;

import org.multibit.Localiser;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;

/**
 * a simple implementation of a view for the command line view system
 * 
 * @author jim
 * 
 */
public class SimpleView extends AbstractView {
    public SimpleView(Localiser localiser, String viewDescription, InputStream inputStream, PrintStream printStream) {
        super(localiser, viewDescription, inputStream, printStream);
    }

    /**
     * display the view to the user
     */
    @Override
    public void displayView() {
        printStream.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX + CommandLineViewSystem.DISPLAY_VIEW_PREFIX + description);        
 
        // show menu of actions and process response
        displayActionsAndProcessResponse();
   }
}
