package org.multibit.qrcode;

/**
 * <p>Exception to provide the following to {@link BitcoinURI}:</p>
 * <ul>
 * <li>Provision of parsing error messages</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class BitcoinURIParseException extends RuntimeException {

    public BitcoinURIParseException(String s) {
        super(s);
    }

    public BitcoinURIParseException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
