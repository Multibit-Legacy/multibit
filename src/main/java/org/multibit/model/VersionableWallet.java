package org.multibit.model;

import com.google.bitcoin.core.Wallet;

/**
 * A class encapsulating a Wallet together with a major and minor version.
 * 
 * This class (when serialised using protobuf) is intentionally not readable
 * by earlier versions of MultiBit in order to avoid loss of the encrypted 
 * private key data.
 * 
 * @author jim
 *
 */
public class VersionableWallet {

    private Wallet wallet;
    
    private int majorVersion;
 
    private int minorVersion;

    public VersionableWallet(Wallet wallet, int majorVersion, int minorVersion) {
        super();
        this.wallet = wallet;
        this.majorVersion = majorVersion;
        this.minorVersion = minorVersion;
    }
 
    public Wallet getWallet() {
        return wallet;
    }

    public void setWallet(Wallet wallet) {
        this.wallet = wallet;
    }

    public int getMajorVersion() {
        return majorVersion;
    }

    public void setMajorVersion(int majorVersion) {
        this.majorVersion = majorVersion;
    }

    public int getMinorVersion() {
        return minorVersion;
    }

    public void setMinorVersion(int minorVersion) {
        this.minorVersion = minorVersion;
    }

    @Override
    public String toString() {
        return "VersionableWallet [wallet=" + wallet + ", majorVersion=" + majorVersion + ", minorVersion=" + minorVersion + "]";
    }
    
}
