package org.multibit.model;

/**
 * Enum denoting whether wallet is Java serialised or protobuf.
 * @author jim
 *
 */
public enum WalletMajorVersion {
    
    /**
     * Wallet major version for Java serialised wallets.
     */
    SERIALIZED(1, "walletVersion.1"), 
    
    /**
     * Wallet major version for unencrypted protobuf wallets.
     */
    PROTOBUF(2, "walletVersion.2"),
    
    /**
     * Wallet major version for encrypted protobuf wallets.
     */
    PROTOBUF_ENCRYPTED(3, "walletVersion.3"),
    
    /**
     * Wallet major version future wallets - used in testing only.
     */
    FUTURE(1000000, "walletVersion.1000000");
    
    /**
     * How the wallet version is represented in the wallet info files.
     */
    private int walletVersionAsInt;
        
    /**
     * The key to use in localisation to describe the wallet version.
     */
    private String localisationKey;
    
    private WalletMajorVersion(int walletVersionAsInt, String localisationKey) {
      this.walletVersionAsInt = walletVersionAsInt;
      this.localisationKey = localisationKey;
    }
        
    public String getWalletVersionString() {
      return "" + walletVersionAsInt;
    }
    
    public int getWalletVersionAsInt() {
        return walletVersionAsInt;
    }
    
    public String getLocalisationKey() {
        return localisationKey;
    }
    
    public String toString() {
        return getWalletVersionString();
    }
}
