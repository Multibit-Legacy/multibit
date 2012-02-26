package org.multibit.file;

/**
 * <p>Exception to provide the following to {@link PrivateKeysHandler}:</p>
 * <ul>
 * <li>Provision of parsing error messages</li>
 * </ul>
 * <p>This base exception acts as a general failure mode not attributable to a specific cause (other than
 * that reported in the exception message). Since this is in English, it may not be worth reporting directly
 * to the user other than as part of a "general failure to parse" response.</p>
 *
 * @since 0.3.0
 */
public class PrivateKeysHandlerException extends RuntimeException {

    private static final long serialVersionUID = 2372470341301293437L;

    public PrivateKeysHandlerException(String s) {
        super(s);
    }

    public PrivateKeysHandlerException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
