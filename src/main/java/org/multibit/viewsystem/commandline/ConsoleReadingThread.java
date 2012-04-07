package org.multibit.viewsystem.commandline;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.Collection;

import javax.swing.Action;

public class ConsoleReadingThread extends Thread {

    private BufferedReader reader;
    private PrintStream printStream;

    private MultiBitShell multiBitTool;

    public ConsoleReadingThread(InputStream inputStream, PrintStream printStream, MultiBitShell multiBitTool) {
        this.printStream = printStream;
        reader = new BufferedReader(new InputStreamReader(inputStream));
        this.multiBitTool = multiBitTool;

    }

    @Override
    public void run() {
        getHoldOfTheConsole();
    }

    public void getHoldOfTheConsole() {
        while (true) {
            try {
                printStream.print("MB > ");
                String inputLine = reader.readLine();

                // pass the line read in to the multibit tool command processor
                multiBitTool.processLine(inputLine);
                Thread.sleep(100);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
}
