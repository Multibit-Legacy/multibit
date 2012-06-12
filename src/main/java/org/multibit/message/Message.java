package org.multibit.message;

/**
 * POJO containing message data.
 * @author jim
 *
 */
public class Message {
    
    public static final int NOT_RELEVANT_PERCENTAGE_COMPLETE = -1;
    /**
     * The text of the message.
     */
    private String text;

    /**
     * Whether the message should be cleared automatically.
     */
    private boolean clearAutomatically;
    
    /**
     * The percentage complete required for any progress bar.
     * Should be in the range 0 to 100.
     * 'Null' value i.e. not relevant is -1
     */
    private double percentComplete;

    public Message(String text) {
        this(text, true, NOT_RELEVANT_PERCENTAGE_COMPLETE);
    }

    public Message(String text, boolean clearAutomatically) {
        this(text, clearAutomatically, NOT_RELEVANT_PERCENTAGE_COMPLETE);
    }

    public Message(String text, double percentComplete) {
        this(text, true, percentComplete);
    }

    private Message(String text, boolean clearAutomatically, double percentComplete) {
        this.text = text;
        this.clearAutomatically = clearAutomatically;
        this.percentComplete = percentComplete;
    }
    
    public void setText(String text) {
        this.text = text;
    }
    
    public String getText() {
        return text;
    }

    public boolean isClearAutomatically() {
        return clearAutomatically;
    }

    public void setClearAutomatically(boolean clearAutomatically) {
        this.clearAutomatically = clearAutomatically;
    }

    public double getPercentComplete() {
        return percentComplete;
    }

    public void setPercentComplete(int percentComplete) {
        this.percentComplete = percentComplete;
    }
}
