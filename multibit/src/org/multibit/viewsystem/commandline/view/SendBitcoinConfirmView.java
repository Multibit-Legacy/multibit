package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;

import org.multibit.Localiser;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;

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
        printStream.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX + CommandLineViewSystem.DISPLAY_VIEW_PREFIX + description);        
 
        // show menu of actions and process response
        displayActionsAndProcessResponse();
   }
}
