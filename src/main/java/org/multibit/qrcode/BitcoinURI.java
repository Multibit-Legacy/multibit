/*
 * Copyright 2010 the original author or authors.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Based on BitcoinURI from de.schildbach.wallet
 * 
 */

package org.multibit.qrcode;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Utils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TimeZone;

/**
 * <p>Provides a standard implementation of a Bitcoin URI with support for the following:</p>
 * <ul>
 * <li>URLEncoded URIs (as passed in by IE on the command line)</li>
 * <li>BIP21 names (including the "req-" prefix handling requirements)</li>
 * </ul>
 * <h2>Accepted formats</h2>
 * <p>The following input forms are accepted</p>
 * <ul>
 * <li>{@code bitcoin:<address>}</li>
 * <li>{@code bitcoin:<address>?<name1>=<value1>&<name2>=<value2>} with multiple additional name/value pairs</li>
 * </ul>
 * <p>The name/value pairs are processed as follows:</p>
 * <ul>
 * <li>URL encoding is stripped and treated as UTF-8</li>
 * <li>names prefixed with {@code req-} are treated as required and if unknown or conflicting cause a parse exception</li>
 * <li>Unknown names not prefixed with {@code req-} are silently ignored unless they are malformed</li>
 * <li>Known names not prefixed with {@code req-} are processed unless they are malformed</li>
 * </ul>
 * <p>The following names are known and have the following formats</p>
 * <ul>
 * <li>{@code amount} decimal value to 8 dp (e.g. 0.12345678) <b>Note that the exponent notation is not supported any more</b></li>
 * <li>{@code label} any alphanumeric</li>
 * <li>{@code message} any alphanumeric</li>
 * <li>{@code req-expires} an ISO8601 UTC timestamp (e.g. 2000-12-31T23:59:59Z)</li>
 * </ul>
 *
 * @author Andreas Schildbach (initial code)
 * @author Jim Burton (enhancements for MultiBit)
 * @author Gary Rowe (BIP21 support)
 * @see <a href="https://en.bitcoin.it/wiki/URI_Scheme">The Bitcoin URI specification</a>
 */
public class BitcoinURI {

    /**
     * Provides logging for this class
     */
    private static final Logger log = LoggerFactory.getLogger(BitcoinURI.class);

    // Not worth turning into an enum
    private static final String FIELD_REQ_EXPIRES = "req-expires";
    private static final String FIELD_MESSAGE = "message";
    private static final String FIELD_LABEL = "label";
    private static final String FIELD_AMOUNT = "amount";
    private static final String FIELD_ADDRESS = "address";

//    private static final Pattern FIELD_AMOUNT_PATTERN = Pattern.compile("([\\d.]+)(?:X(\\d+))?");

    public static final String BITCOIN_SCHEME = "bitcoin";

    /**
     * Contains all the parameters in the order in which they were processed
     */
    private final Map<String, Object> parameterMap = new LinkedHashMap<String, Object>();

    /**
     * @param networkParameters The BitCoinJ network parameters that determine which network the URI is from
     * @param input             The raw URI data to be parsed (see class comments for accepted formats)
     * @throws BitcoinURIParseException If the input fails Bitcoin URI syntax and semantic checks
     */
    public BitcoinURI(NetworkParameters networkParameters, String input) {

        // Basic validation            
        if (networkParameters == null) {
            throw new IllegalArgumentException("networkParameters cannot be null");
        }
        if (input == null || "".equals(input.trim())) {
            throw new IllegalArgumentException("networkParameters cannot be null or empty");
        }

        log.debug("Attempting to parse '{}' for {}", input, networkParameters.port == 8333 ? "prodNet" : "testNet");

        // URI validation
        if (!input.startsWith(BITCOIN_SCHEME)) {
            throw new BitcoinURIParseException("Bad scheme - expecting '" + BITCOIN_SCHEME + "'");
        }

        // Attempt to form the URI (fail fast syntax checking to official standards)
        URI uri;
        try {
            uri = new URI(input);
        } catch (URISyntaxException e) {
            throw new BitcoinURIParseException("Bad URI syntax", e);
        }

        // Examine the scheme specific part in detail
        String schemeSpecificPart = uri.getSchemeSpecificPart();

        // Split the parameters using the known literals
        String[] tokens = schemeSpecificPart.split("[\\?\\&\\=]");
        if (tokens == null || tokens.length == 0) {
            throw new BitcoinURIParseException("Missing address");
        }

        // Attempt to parse the rest of the URI parameters
        parseParameters(networkParameters, tokens);

        // Apply non-null default values if not present
        if (!parameterMap.containsKey(FIELD_AMOUNT)) {
            parameterMap.put(FIELD_AMOUNT, BigInteger.ZERO);
        }

    }

    /**
     * @param networkParameters The network parameters
     * @param tokens            The tokens representing the parameters (assumed to be in pairs)
     */
    private void parseParameters(NetworkParameters networkParameters, String[] tokens) {
        // Attempt to parse the first entry as a Bitcoin address for this network
        Address address;
        try {
            address = new Address(networkParameters, tokens[0]);
            putWithValidation(FIELD_ADDRESS, address);
        } catch (final AddressFormatException e) {
            throw new BitcoinURIParseException("Bad address", e);
        }

        // Attempt to decode the rest of the tokens into a parameter map
        for (int i = 1; i < tokens.length; i++) {

            // Keep track of position for the "name" field
            boolean last = (i + 1 >= tokens.length);

            // Parse the amount/value pair
            if (FIELD_AMOUNT.equals(tokens[i].toLowerCase())) {
                // Check for a pair
                if (last) {
                    throw new OptionalFieldValidationException("'" + tokens[i] + "' does not have a value");
                }

                // Decode the amount (contains an optional decimal component to 8dp)
                BigInteger amount;
                try {
                    amount = Utils.toNanoCoins(tokens[i + 1]);
                } catch (NumberFormatException e) {
                    throw new OptionalFieldValidationException("'" + tokens[i] + "' value is not valid", e);
                }
                putWithValidation(tokens[i], amount);
                // Skip over the value
                i++;
                continue;
            }

            // Parse the label/value pair
            if (FIELD_LABEL.equals(tokens[i].toLowerCase())) {
                // Check for a pair
                if (last) {
                    throw new OptionalFieldValidationException("'" + tokens[i] + "' does not have a value");
                }
                putWithValidation(tokens[i], tokens[i + 1]);
                // Skip over the value
                i++;
                continue;
            }

            // Parse the message/value pair
            if (FIELD_MESSAGE.equals(tokens[i].toLowerCase())) {
                // Check for a pair
                if (last) {
                    throw new OptionalFieldValidationException("'" + tokens[i] + "' does not have a value");
                }
                putWithValidation(tokens[i], tokens[i + 1]);
                // Skip over the value
                i++;
                continue;
            }

            // Parse the req-expires/value pair
            if (FIELD_REQ_EXPIRES.equals(tokens[i].toLowerCase())) {
                // Check for a pair
                if (last) {
                    throw new OptionalFieldValidationException("'" + tokens[i] + "' does not have a value");
                }
                // Attempt to parse as a ISO8601 UTC date (always use new SDF to avoid threading issues)
                SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
                sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                Date reqExpires;
                try {
                    reqExpires = sdf.parse(tokens[i + 1]);
                } catch (ParseException e) {
                    throw new RequiredFieldValidationException("'" + tokens[i] + "' is not in the correct format (ISO8601 UTC)");
                }
                // Check for expiry
                if (reqExpires.before(new Date())) {
                    throw new RequiredFieldValidationException("'" + tokens[i] + "' has expired, this URI is not valid");
                }
                putWithValidation(tokens[i], reqExpires);
                // Skip over the value
                i++;
                continue;
            }

            // Must be unknown to be here

            // Check for required parameters
            if (tokens[i].startsWith("req-")) {
                throw new RequiredFieldValidationException("'" + tokens[i] + "' is required but not known, this URI is not valid");
            }

            // Must be unknown and optional to be here - assume name/value pairing

            // Avoid tripping over an isolated end token (be generous with input)
            if (!last) {
                putWithValidation(tokens[i], tokens[i + 1]);
                // Skip over the value
                i++;
                continue;
            } else {
                // Isolated parameter at the end so treat as itself and rely on external application to deal with it
                putWithValidation(tokens[i], tokens[i]);
            }
        }

    }

    /**
     * <p>Put the value against the key in the map checking for duplication This avoids address field overwrite etc.</p>
     *
     * @param key   The key for the map
     * @param value The value to store
     */
    private void putWithValidation(String key, Object value) {
        if (parameterMap.containsKey(key)) {
            throw new BitcoinURIParseException("'" + key + "' is duplicated, URI is invalid");
        } else {
            parameterMap.put(key, value);
        }
    }

    /**
     * @return The Bitcoin Address from the URI
     */

    public Address getAddress() {
        return (Address) parameterMap.get(FIELD_ADDRESS);
    }

    /**
     * @return The amount name encoded using a pure integer value based at 10,000,000 units is 1 BTC
     */
    public BigInteger getAmount() {
        return (BigInteger) parameterMap.get(FIELD_AMOUNT);
    }

    /**
     * @return The label from the URI
     */
    public String getLabel() {
        return (String) parameterMap.get(FIELD_LABEL);
    }

    /**
     * @return The message from the URI
     */
    public String getMessage() {
        return (String) parameterMap.get(FIELD_MESSAGE);
    }

    /**
     * @return The expiry date (UTC) from the URI
     */
    public Date getExpires() {
        return (Date) parameterMap.get(FIELD_REQ_EXPIRES);
    }

    /**
     * @return True if the Bitcoin URI has been parsed successfully
     * @deprecated Remove when ready since it is always true
     */
    @Deprecated
    public boolean isParsedOk() {
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("BitcoinURI[");
        boolean first = true;
        for (Map.Entry<String, Object> entry : parameterMap.entrySet()) {
            if (first) {
                first = false;
            } else {
                sb.append(",");
            }
            sb.append("'").append(entry.getKey()).append("'=").append("'").append(entry.getValue().toString()).append("'");
            ;
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * <p>Simple Bitcoin URI builder using known good fields</p>
     *
     * @param address The Bitcoin address
     * @param amount  A String representation of the amount in BTC (decimal)
     * @param label   The label
     * @return A String containing the Bitcoin URI
     */
    public static String convertToBitcoinURI(String address, String amount, String label) {
        return String.format("%s:%s?%s=%s&%s=%s",
                BITCOIN_SCHEME,
                address,
                FIELD_AMOUNT,
                amount,
                FIELD_LABEL,
                label);
    }
}
