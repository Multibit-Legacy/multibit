package org.multibit.crypto;

/**
 * <p>Exception to provide the following to {@link EncrypterDecrypter}:</p>
 * <ul>
 * <li>Provision of encryption / decryption exception</li>
 * </ul>
 * <p>This base exception acts as a general failure mode not attributable to a specific cause (other than
 * that reported in the exception message). Since this is in English, it may not be worth reporting directly
 * to the user other than as part of a "general failure to parse" response.</p>
 *
 * @since 0.3.0
 */
public class EncrypterDecrypterException extends RuntimeException {
    private static final long serialVersionUID = -4441989608332681377L;

    public EncrypterDecrypterException(String s) {
        super(s);
    }

    public EncrypterDecrypterException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
