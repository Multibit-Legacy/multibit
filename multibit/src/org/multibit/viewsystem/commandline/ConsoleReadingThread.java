package org.multibit.viewsystem.commandline;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.Collection;

import org.multibit.action.Action;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;

public class ConsoleReadingThread extends Thread implements DataProvider {

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
                //JOptionPane.showMessageDialog(null, "ConsoleReadingThread PING 1" + this.getName()
                //        + " saw '" + inputLine + "'.");
                if (enableReaderFiring) {
                    //JOptionPane.showMessageDialog(null, "ConsoleReadingThread PING 2");
                        if (possibleActions != null) {
                        for (Action possibleAction : possibleActions) {
                            // if the text entered matches the start of an action - fire the first encountered
                            if (inputLine != null && inputLine.length() > 0 
                                    && possibleAction.getDisplayText().toLowerCase().startsWith(inputLine.toLowerCase())) {
                                // fire the action
                                if (enableReaderFiring) {
                                    //JOptionPane.showMessageDialog(null, "ConsoleReadingThread PING 3");
                                    enableReaderFiring = false;
                                    possibleAction.execute(this);
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

    public Data getData() {
        // TODO return any data input in forms/ selections etc
        return null;
    }
}
