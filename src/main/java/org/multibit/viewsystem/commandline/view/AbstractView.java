package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Collection;

import javax.swing.Action;

import org.multibit.Localiser;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;
import org.multibit.viewsystem.commandline.ConsoleReadingThread;

public abstract class AbstractView implements CommandLineView {
    protected String description;

    protected Localiser localiser;

    protected InputStream inputStream;

    protected PrintStream printStream;

    protected Collection<Action> possibleActions;

    protected static ConsoleReadingThread consoleReader;

    public AbstractView(Localiser localiser, String viewDescription, InputStream inputStream,
            PrintStream printStream) {
        this.localiser = localiser;
        this.description = viewDescription;
        this.inputStream = inputStream;
        this.printStream = printStream;
        
        if (consoleReader == null) {
            consoleReader = new ConsoleReadingThread(inputStream, printStream, possibleActions);
            consoleReader.start();           
        }
    }

    /**
     * display the view to the user
     */
    abstract public void displayView();

    public String getDescription() {
        return description;
    }

    /**
     * navigate away from this view
     * 
     * @param nextView
     *            - the next view being viewed - this is given so that the view
     *            can custom its behaviour - it may present a nested view or pop
     *            to a parent view (this is left to the view)
     * @param relationshipOfNewViewToPrevious - one of the ViewSystem relationship constants
     * @see org.multibit.viewsystem.View#navigateAwayFromView()
     */
    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        // switch off the reader
        if (consoleReader != null) {
            consoleReader.enableReaderFiring(false);
            consoleReader.setPossibleActions(null);
            
            // output a new line to move off the input prompt
            //printStream.print("\nPing 1 " + (new Date()).toString() + " ");
        }
        //printStream.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
        //        + CommandLineViewSystem.NAVIGATE_AWAY_FROM_VIEW_PREFIX + description);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        String localisedMessage = localiser.getString(messageKey, messageData);
        printStream.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
                + CommandLineViewSystem.MESSAGE_PREFIX + localisedMessage);
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        this.possibleActions = possibleActions;
    }

    protected void displayActionsAndProcessResponse() {
        assert possibleActions != null : getDescription() + " has no possible actions";

        if (possibleActions != null) {
            // prompt for input and execute subsequent action
            consoleReader.setPossibleActions(possibleActions);
            consoleReader.enableReaderFiring(true);

            printStream.print(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
                    + CommandLineViewSystem.DISPLAY_MENU_OPTIONS_PREFIX);
            for (Action possibleAction : possibleActions) {
                System.out.print(possibleAction.getValue(Action.NAME) + CommandLineViewSystem.SEPARATOR);
            }
            printStream.print("\n" + CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
                    + CommandLineViewSystem.MENU_CHOOSE_PREFIX);

        }
    }
}
