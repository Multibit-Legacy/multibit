package org.multibit.model;

import com.google.bitcoin.core.Wallet;

/**
 * A class encapsulating a Wallet.
 * This class (when serialised using protobuf) is intentionally not readable
 * by earlier versions of MultiBit in order to avoid loss of the encrypted 
 * private key data.
 * 
 * @author jim
 *
 */
public class WrappedWallet {

    private Wallet wallet;

    public WrappedWallet(Wallet wallet) {
        super();
        this.wallet = wallet;
    }
 
    public Wallet getWallet() {
        return wallet;
    }

    public void setWallet(Wallet wallet) {
        this.wallet = wallet;
    }
}
