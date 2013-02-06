package org.multibit.model.bitcoin;

public enum StatusEnum {
    ONLINE("multiBitFrame.onlineText"),
    CONNECTING("multiBitFrame.offlineText"),
    ERROR("multiBitFrame.errorText");
    
    private String localisationKey;
    
    private StatusEnum(String localisationKey) {
        this.localisationKey = localisationKey;
      }

    public String getLocalisationKey() {
        return localisationKey;
    }         
}