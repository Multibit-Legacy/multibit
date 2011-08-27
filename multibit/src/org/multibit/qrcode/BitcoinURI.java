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

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.multibit.controller.MultiBitController;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;

/**
 * @author Andreas Schildbach
 * @author Jim Burton
 */
public class BitcoinURI {
    private Address address;
    private BigInteger amount;
    private String label;

    private static final Pattern AMOUNT_PATTERN = Pattern.compile("([\\d.]+)(?:X(\\d+))?");

    public BitcoinURI(MultiBitController controller, URI uri)  {
        String scheme = uri.getScheme();

        if ("bitcoin".equals(scheme)) {
            URI u;
            try {
                u = new URI("bitcoin://" + uri.getSchemeSpecificPart());
            } catch (URISyntaxException e) {
                throw new IllegalArgumentException("Cannot understand uri " + uri.toASCIIString());
            }

            String addressString =  u.getHost();
            try {
                address = new Address(controller.getMultiBitService().getNetworkParameters(), addressString);
            } catch (final AddressFormatException x) {
                x.printStackTrace();
            }

            String query = u.getQuery();
            StringTokenizer queryTokenizer = new StringTokenizer(query, "&");
            
            while(queryTokenizer.hasMoreTokens()) {
                // get the next nameValue pair - these are of format "<name>=<value>:
                String nameValuePair = queryTokenizer.nextToken();
                
                // parse nameValuePair
                StringTokenizer nameValueTokenizer = new StringTokenizer(nameValuePair, "=");
                String name = null;
                String value = null;
                
                if (nameValueTokenizer.hasMoreTokens()) {
                    name = nameValueTokenizer.nextToken();
                }
                if (nameValueTokenizer.hasMoreTokens()) {
                    value = nameValueTokenizer.nextToken();
                }
                if ("amount".equalsIgnoreCase(name)) {
                    // amount to parse
                    if (value != null) {
                        Matcher matcher = AMOUNT_PATTERN.matcher(value);
                        if (matcher.matches()) {
                            amount = Utils.toNanoCoins(matcher.group(1));
                            if (matcher.group(2) != null)
                                amount.multiply(BigInteger.valueOf(10).pow(Integer.parseInt(matcher.group(2)) - 8));
                        }
                    }
                } else if ("label".equalsIgnoreCase(name)) {
                    // label 
                    label = value;   
                }
                // we ignore any other name value pairs
            }
        } else {
            throw new IllegalArgumentException("unknown scheme: " + scheme);
        }
    }

    public static String convertToBitcoinURI(String address, String amount, String label) {
        if (address == null) {
            return "";
        }
        
        String uriToReturn = "bitcoin:" + address;
        
        boolean haveOutputQuestionMark = false;
        if (amount != null && amount.length() > 0) {
            uriToReturn = uriToReturn + "?amount=" + amount;
            haveOutputQuestionMark = true;
        }
        
        if (label != null && label.length() > 0) {
            if (haveOutputQuestionMark) {
                uriToReturn = uriToReturn + "&label=" + label;
                
            } else {
                uriToReturn = uriToReturn + "?label=" + label;
                
            }
        }
        return uriToReturn;
    }
    
    public Address getAddress() {
        return address;
    }

    public BigInteger getAmount() {
        return amount;
    }

    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {
        return "BitcoinURI[" + address + "," + amount + "," + label + "]";
    }
}
