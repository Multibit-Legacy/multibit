package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;

import org.multibit.Localiser;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;

/**
 * help contents view for command line view system
 * 
 * @author jim
 * 
 */
public class HelpContentsView extends AbstractView {
   public HelpContentsView(Localiser localiser, String viewDescription, InputStream inputStream,
            PrintStream printStream) {
        super(localiser, viewDescription, inputStream, printStream);
    }

    /**
     * display the view to the user
     */
    @Override
    public void displayView() {
        String helpContentsMessage = localiser.getString("helpContentsView.todo", null);

        printStream.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
                        + CommandLineViewSystem.DISPLAY_VIEW_PREFIX + description + "\n"
                        + helpContentsMessage);

        // if there was a form it would go here

        // show menu of actions and process response
        displayActionsAndProcessResponse();
    }
}
