package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;

import org.multibit.Localiser;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;

/**
 * the command line view for the address book open at the sending addresses
 * 
 * @author jim
 * 
 */
public class AddressBookSendingView extends AbstractView {
    public AddressBookSendingView(Localiser localiser, String viewDescription, InputStream inputStream, PrintStream printStream) {
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
