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
    SERIALIZED((byte)0x01, "walletVersion.1"), 
    
    /**
     * Wallet major version for unencrypted protobuf wallets.
     */
    PROTOBUF((byte)0x02, "walletVersion.2"),
    
    /**
     * Wallet major version for encrypted protobuf wallets.
     */
    PROTOBUF_ENCRYPTED((byte)0x03, "walletVersion.3"),
    
    /**
     * Wallet major version future wallets - used in testing only.
     */
    FUTURE((byte)0xff, "walletVersion.1000000");
    
    /**
     * How the wallet major version is represented in the wallet info files.
     */
    private byte walletMajorVersionByte;
        
    /**
     * The key to use in localisation to describe the wallet major version.
     */
    private String localisationKey;
    
    private WalletMajorVersion(byte walletMajorVersionByte, String localisationKey) {
      this.walletMajorVersionByte = walletMajorVersionByte;
      this.localisationKey = localisationKey;
    }
        
    public String getWalletMajorVersionString() {
      return Byte.toString(walletMajorVersionByte);
    }
    
    public byte getWalletMajorVersionByte() {
        return walletMajorVersionByte;
    }
    
    public String getLocalisationKey() {
        return localisationKey;
    }
    
    public String toString() {
        return Byte.toString(walletMajorVersionByte);
    }
}
