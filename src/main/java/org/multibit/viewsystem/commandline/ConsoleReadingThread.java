package org.multibit.viewsystem.commandline;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.Collection;

import javax.swing.Action;

public class ConsoleReadingThread extends Thread implements org.multibit.viewsystem.dataproviders.DataProvider {

    private BufferedReader reader;
    private PrintStream printStream;

    private Collection<Action> possibleActions;

    private boolean enableReaderFiring;

    public ConsoleReadingThread(InputStream inputStream, PrintStream printStream,
            Collection<Action> possibleActions) {
        this.possibleActions = possibleActions;
        this.printStream = printStream;
        reader = new BufferedReader(new InputStreamReader(inputStream));

        enableReaderFiring = true;
    }

    synchronized public void setPossibleActions(Collection<Action> possibleActions) {
        this.possibleActions = possibleActions;
    }

    @Override
    public void run() {
        getHoldOfTheConsole();
    }

    /**
     * enable whethere reader will fire on actions
     */
    public void enableReaderFiring(boolean enable) {
        enableReaderFiring = enable;
    }

    public void getHoldOfTheConsole() {
        while (true) {
            try {
                //printStream.print("|");
                String inputLine = reader.readLine();
                if (enableReaderFiring) {
                        if (possibleActions != null) {
                        for (Action possibleAction : possibleActions) {
                            // if the text entered matches the start of an action - fire the first encountered
                            if (inputLine != null && inputLine.length() > 0 
                                    && ((String)possibleAction.getValue(Action.NAME)).toLowerCase().startsWith(inputLine.toLowerCase())) {
                                // fire the action
                                if (enableReaderFiring) {
                                    enableReaderFiring = false;
                                    possibleAction.actionPerformed(null);
                                    //System.out.println("ConsoleReadingThread " + this.getName()
                                    //        + " ran an execute on "
                                    //        + possibleAction.getDisplayText());
                                 }
                                break;
                            }
                        }
                    }
                } else {
                    Thread.sleep(100);
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
}
