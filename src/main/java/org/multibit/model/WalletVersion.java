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
    SERIALIZED((byte)0x01, "walletVersion.1"), 
    
    /**
     * Wallet version for unencrypted protobuf wallets.
     */
    PROTOBUF((byte)0x02, "walletVersion.2"),
    
    /**
     * Wallet version for encrypted protobuf wallets.
     */
    PROTOBUF_ENCRYPTED((byte)0x03, "walletVersion.3"),
    
    /**
     * Wallet version future wallets - used in testing only.
     */
    FUTURE((byte)0xff, "walletVersion.1000000");
    
    /**
     * How the wallet version is represented in the wallet info files.
     */
    private byte walletVersionByte;
        
    /**
     * The key to use in localisation to describe the wallet version.
     */
    private String localisationKey;
    
    private WalletVersion(byte walletVersionByte, String localisationKey) {
      this.walletVersionByte = walletVersionByte;
      this.localisationKey = localisationKey;
    }
        
    public String getWalletVersionString() {
      return Byte.toString(walletVersionByte);
    }
    
    public byte getWalletVersionByte() {
        return walletVersionByte;
    }
    
    public String getLocalisationKey() {
        return localisationKey;
    }
    
    public String toString() {
        return Byte.toString(walletVersionByte);
    }
}
