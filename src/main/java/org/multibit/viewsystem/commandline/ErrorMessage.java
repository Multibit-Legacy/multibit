package org.multibit.viewsystem.commandline;

public enum ErrorMessage {
    PICK_START_CHARACTER_WRONG(10001, "A \"pick\" argument must start with $ (for pick-by-item) or @ (for pick-by-search)."),
    COULD_NOT_FIND_MATCHING_WALLET(10002, "Could not find wallet matching pick choice of \"{0}\""),
    WRONG_NUMBER_OF_ARGUMENTS_FOR_PICK(100003, "Wrong number of arguments for \"pick\"")
    ;
       
    private int messageId;
    private String value;
    private ErrorMessage(int messageId, String value) {
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
