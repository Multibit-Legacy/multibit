package org.multibit.viewsystem.commandline;

public enum Message {
    MULTIBIT_SHELL_HAS_STOPPED (1, "MultiBitShell has stopped."),
    START_OF_HELP (2, "Start help."),
    END_OF_HELP (3, "End help."),
    WALLET_CREATED (4, "Wallet \"{0}\" created."),
    WALLET_DELETED (5, "Wallet \"{0}\" deleted."),
    CANNOT_HANDLE (6, "{0} was specified but MultiBitShell did not how to do that."),
    PICKED_WALLET (7, "Picked wallet \"{0}\"."),
    START_OF_LIST_OF_WALLETS (8, "Start list."),
    END_OF_LIST_OF_WALLETS (9, "End list.")
    ;
       
    private int messageId;
    private String value;
    private Message(int messageId, String value) {
        this.messageId = messageId;
        this.value = value;
    }
    
    public int getMessageId() {
        return messageId;
    }
    
    public String getValue() {
        return value;
    }
}
