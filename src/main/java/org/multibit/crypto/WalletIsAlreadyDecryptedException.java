package org.multibit.crypto;

/**
 * <p>Exception to provide the following to {@link EncrypterDecrypterOpenSSL}:</p>
 * <ul>
 * <li>Indicates user is trying to decrypt an already decrypted wallet.</li>
 * </ul>
 */
public class WalletIsAlreadyDecryptedException extends EncrypterDecrypterException {
    private static final long serialVersionUID = -4441989609992681377L;

    public WalletIsAlreadyDecryptedException(String s) {
        super(s);
    }

    public WalletIsAlreadyDecryptedException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
