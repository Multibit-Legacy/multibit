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
     * Whether the message is to appear in the statusbar.
     * true = show in statusbar, false = do not.
     */
    private boolean showInStatusBar = true;
    
    /**
     * Whether the message is to appear on the messages tab.
     * true = show in messages tab, false = do not.
     */
    private boolean showInMessagesTab = true;
    
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

    public Message(String text, boolean clearAutomatically, double percentComplete) {
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

    public void setPercentComplete(double percentComplete) {
        this.percentComplete = percentComplete;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (clearAutomatically ? 1231 : 1237);
        long temp;
        temp = Double.doubleToLongBits(percentComplete);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + ((text == null) ? 0 : text.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Message other = (Message) obj;
        if (clearAutomatically != other.clearAutomatically)
            return false;
        if (Double.doubleToLongBits(percentComplete) != Double.doubleToLongBits(other.percentComplete))
            return false;
        if (text == null) {
            if (other.text != null)
                return false;
        } else if (!text.equals(other.text))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "Message [text=" + text + ", clearAutomatically=" + clearAutomatically + ", percentComplete=" + percentComplete
                + "]";
    }

    public boolean isShowInStatusBar() {
        return showInStatusBar;
    }

    public void setShowInStatusBar(boolean showInStatusBar) {
        this.showInStatusBar = showInStatusBar;
    }

    public boolean isShowInMessagesTab() {
        return showInMessagesTab;
    }

    public void setShowInMessagesTab(boolean showInMessagesTab) {
        this.showInMessagesTab = showInMessagesTab;
    }
}
