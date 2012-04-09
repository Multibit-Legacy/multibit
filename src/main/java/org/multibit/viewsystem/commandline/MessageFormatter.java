package org.multibit.viewsystem.commandline;

import java.io.OutputStream;
import java.io.PrintStream;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

/**
 * Utility class for formatting messages
 * 
 * @author jim
 * 
 */
public class MessageFormatter {

    public static final String OUTPUT_PREFIX = "MB:";
    public static final String INPUT_PREFIX = "MB> ";

    public static final String MESSAGE_ID_PREFIX = "[";
    public static final String MESSAGE_ID_SUFFIX = "] ";
    
    public static final String MESSAGE_PREFIX = "";
    public static final String ERROR_PREFIX = "Error:";

    private MessageFormat formatter;
    
    private PrintStream printStream;
 
    public MessageFormatter() {
        formatter = new MessageFormat("");
        printStream = System.out;
    }

    public void printMessage(Message message) {
        printStream.println(formatMessage(message, null));
    }

    /**
     * Outputs a message, given the message id and any required context data
     */
    public void printMessage(Message message, Object[] parameters) {
        printStream.println(formatMessage(message, parameters));
    }

    public String formatMessage(Message message, Object[] parameters) {
        String pattern = message.getValue();
        formatter.applyPattern(pattern);
        return OUTPUT_PREFIX + MESSAGE_PREFIX + MESSAGE_ID_PREFIX + message.getMessageId() + MESSAGE_ID_SUFFIX + formatter.format(parameters);
    }
    
    public String formatMessage(Message message) {
        return OUTPUT_PREFIX + MESSAGE_PREFIX + MESSAGE_ID_PREFIX + message.getMessageId() + MESSAGE_ID_SUFFIX + message.getValue();
    }
    
    public void printError(ErrorMessage message) {
        printStream.println(formatError(message, null));
    }

    /**
     * Outputs an error message, given the message id and any required context data
     */
    public void printError(ErrorMessage message, Object[] parameters) {
        printStream.println(formatError(message, parameters));
    }

    public String formatError(ErrorMessage message, Object[] parameters) {
        String pattern = message.getValue();
        formatter.applyPattern(pattern);
        return OUTPUT_PREFIX + ERROR_PREFIX + MESSAGE_ID_PREFIX + message.getMessageId() + MESSAGE_ID_SUFFIX + formatter.format(parameters);
    }
    
    public String formatError(ErrorMessage message) {
        return OUTPUT_PREFIX + ERROR_PREFIX + MESSAGE_ID_PREFIX + message.getMessageId() + MESSAGE_ID_SUFFIX + message.getValue();
    }

    public PrintStream getPrintStream() {
        return printStream;
    }

    public void setPrintStream(PrintStream printStream) {
        this.printStream = printStream;
    }
    
    /**
     * Used in parsing to match the expected start of output text for a message
     * @param message
     * @return expected initial output text for the specified message
     */
    public static String expectedStartOfOutput(Message message) {
        return OUTPUT_PREFIX + MESSAGE_ID_PREFIX + message.getMessageId()  + MESSAGE_ID_SUFFIX;
    }
}
