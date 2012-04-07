package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;

import javax.swing.Icon;

import org.multibit.Localiser;
import org.multibit.viewsystem.commandline.ObseleteCommandLineViewSystem;

/**
 * the help about command line view 
 * 
 * @author jim
 * 
 */
public class HelpAboutView extends AbstractView {
    public HelpAboutView(Localiser localiser, String viewDescription, InputStream inputStream,
            PrintStream printStream) {
        super(localiser, viewDescription, inputStream, printStream);
        
    }

    /**
     * display the view to the user
     */
    @Override
    public void displayView() {
        String versionNumber = localiser.getVersionNumber();

        String helpAboutMessage = localiser.getString("helpAboutAction.messageText",
                new Object[] { versionNumber });

        printStream
                .println(ObseleteCommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
                        + ObseleteCommandLineViewSystem.DISPLAY_VIEW_PREFIX + description + "\n"
                        + helpAboutMessage);

        // if there was a form it would go here

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
