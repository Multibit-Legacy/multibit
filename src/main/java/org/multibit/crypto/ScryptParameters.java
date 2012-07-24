package org.multibit.crypto;

/**
 * Parameters for use by the EncrypterDecrypterScrypt class
 * 
 * @author jim
 *
 */
public class ScryptParameters {
    public static final int SALT_LENGTH = 8;
    
    // Salt to use.
    private byte[] salt;
    
    // CPU cost parameter for scrypt algorithm.
     private int n;
    
    // Memory cost parameter for scrypt algorithm.
    private int r;
    
    // Parallelization parameter for scrypt algorithm.
    private int p;
 
    /**
     * Encryption/ Decryption using default parameters.
     * 
     * @param salt Salt to use
     */
    public ScryptParameters(byte[] salt) {
        this(salt, 16384, 8, 1);
    }

    /**
     * Encryption/ Decryption using user specified parameters.
     * 
     * @param salt Salt to use
     * @param n CPU cost parameter
     * @param r Memory cost parameter
     * @param p Parallelization parameter
     */
    public ScryptParameters(byte[] salt, int n, int r, int p) {
        this.salt= salt;
        this.n = n;
        this.r = r;
        this.p = p;
    }

    public byte[] getSalt() {
        return salt;
    }

    public void setSalt(byte[] salt) {
        this.salt = salt;
    }

    public int getN() {
        return n;
    }

    public void setN(int n) {
        this.n = n;
    }

    public int getR() {
        return r;
    }

    public void setR(int r) {
        this.r = r;
    }

    public int getP() {
        return p;
    }

    public void setP(int p) {
        this.p = p;
    }
}
