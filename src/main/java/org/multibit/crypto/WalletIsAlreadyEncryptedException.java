package org.multibit.crypto;

/**
 * <p>Exception to provide the following to {@link EncrypterDecrypterOpenSSL}:</p>
 * <ul>
 * <li>Indicates user is trying to encrypt an already encrypted wallet.</li>
 * </ul>
 */
public class WalletIsAlreadyEncryptedException extends EncrypterDecrypterException {
    private static final long serialVersionUID = -4441989609992681377L;

    public WalletIsAlreadyEncryptedException(String s) {
        super(s);
    }

    public WalletIsAlreadyEncryptedException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
