package org.multibit.crypto;

import java.util.Arrays;

public class EncryptedPrivateKey {

    // The initialisation vector for the AES encryption (16 bytes)
    private byte[] initialisationVector ;

    // The encrypted private key bytes
    private byte[] encryptedPrivateBytes;

    /**
     * Cloning constructor.
     * @param encryptedPrivateKey EncryptedPrivateKey to clone.
     */
    public EncryptedPrivateKey(EncryptedPrivateKey encryptedPrivateKey) {
        if (encryptedPrivateKey != null) {
            setInitialisationVector(encryptedPrivateKey.getInitialisationVector());
            setEncryptedPrivateBytes(encryptedPrivateKey.getEncryptedBytes());
        }
    }

    /**
     * @param iv
     * @param encryptedPrivateKeys
     */
    public EncryptedPrivateKey(byte[] initialisationVector, byte[] encryptedPrivateKeys) {
        setInitialisationVector(initialisationVector);
        setEncryptedPrivateBytes(encryptedPrivateKeys);
    }

    public byte[] getInitialisationVector() {
        return initialisationVector;
    }

    /**
     * Set the initialisationVector, cloning the bytes.
     * 
     * @param initialisationVector
     */
    public void setInitialisationVector(byte[] initialisationVector) {
        if (initialisationVector == null) {
            this.initialisationVector = null;
            return;
        }

        byte[] cloneIV = new byte[initialisationVector.length];
        System.arraycopy(initialisationVector, 0, cloneIV, 0, initialisationVector.length);

        this.initialisationVector = cloneIV;
    }

    public byte[] getEncryptedBytes() {
        return encryptedPrivateBytes;
    }

    /**
     * Set the encrypted private key bytes, cloning them.
     * 
     * @param encryptedPrivateBytes
     */
    public void setEncryptedPrivateBytes(byte[] encryptedPrivateBytes) {
        if (encryptedPrivateBytes == null) {
            this.encryptedPrivateBytes = null;
            return;
        }
        
        byte[] cloneEncryptedPrivateBytes = new byte[encryptedPrivateBytes.length];
        System.arraycopy(encryptedPrivateBytes, 0, cloneEncryptedPrivateBytes, 0, encryptedPrivateBytes.length);

        this.encryptedPrivateBytes = encryptedPrivateBytes;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Arrays.hashCode(encryptedPrivateBytes);
        result = prime * result + Arrays.hashCode(initialisationVector);
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
        EncryptedPrivateKey other = (EncryptedPrivateKey) obj;
        if (!Arrays.equals(encryptedPrivateBytes, other.encryptedPrivateBytes))
            return false;

        if (!Arrays.equals(initialisationVector, other.initialisationVector))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "EncryptedPrivateKey [initialisationVector=" + Arrays.toString(initialisationVector) + ", encryptedPrivateKey=" + Arrays.toString(encryptedPrivateBytes) + "]";
    }       
}
