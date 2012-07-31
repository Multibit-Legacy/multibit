package org.multibit.crypto;

import java.io.Serializable;
import java.util.Arrays;

/**
 * Parameters for use by the EncrypterDecrypterScrypt class
 * 
 * @author jim
 *
 */
public class ScryptParameters implements Serializable {

    private static final long serialVersionUID = -7951947032904886087L;

    public static final int SALT_LENGTH = 8;
    
    public static int DEFAULT_N = 16384;
    
    public static int DEFAULT_R = 8;
    
    public static int DEFAULT_P = 1;
    
    
    
    // Salt to use.
    private byte[] salt;
    
    // CPU cost parameter for scrypt algorithm.
    private int n;
    
    // Memory cost parameter for scrypt algorithm.
    private int r;
    
    // Parallelization parameter for scrypt algorithm.
    private int p;

    /**
     * Encryption/ Decryption using default parameters and blank salt.
     */
    public ScryptParameters() {
        this(new byte[0]);
    }

    /**
     * Encryption/ Decryption using default parameters.
     * 
     * @param salt Salt to use
     */
    public ScryptParameters(byte[] salt) {
        this(salt, DEFAULT_N, DEFAULT_R, DEFAULT_P);
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

    @Override
    public String toString() {
        return "ScryptParameters [salt=" + Arrays.toString(salt) + ", n=" + n + ", r=" + r + ", p=" + p + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (int) (prime * result + n);
        result = prime * result + p;
        result = prime * result + r;
        result = prime * result + Arrays.hashCode(salt);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ScryptParameters other = (ScryptParameters) obj;
        if (n != other.n)
            return false;
        if (p != other.p)
            return false;
        if (r != other.r)
            return false;
        if (!Arrays.equals(salt, other.salt))
            return false;
        return true;
    }
}
