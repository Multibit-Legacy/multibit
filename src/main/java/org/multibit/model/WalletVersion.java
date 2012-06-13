package org.multibit.model;

/**
 * Enum denoting whether wallet is Java serialised or protobuf.
 * @author jim
 *
 */
public enum WalletVersion {
    
    /**
     * Wallet version for Java serialised wallets.
     */
    SERIALIZED("1", "walletVersion.1"), 
    
    /**
     * Wallet version for protobuf wallets.
     */
    PROTOBUF("2", "walletVersion.2");
    
    /**
     * How the wallet version is represented in the wallet info files.
     */
    private String walletVersionString;
        
    /**
     * The key to use in localisation to describe the wallet version.
     */
    private String localisationKey;
    
    private WalletVersion(String walletVersionString, String localisationKey) {
      this.walletVersionString = walletVersionString;
      this.localisationKey = localisationKey;
    }
        
    public String getWalletVersionString() {
      return walletVersionString;
    }
    
    public String getLocalisationKey() {
        return localisationKey;
    }
    
    public String toString() {
        return walletVersionString;
    }
}
