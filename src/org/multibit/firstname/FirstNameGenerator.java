package org.multibit.firstname;

/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;

/**
 * class to generate firstnames from a target string
 * 
 * firstnames are of the format:
 * 
 * Jim Burton.lk2 - first NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME chars are used
 * The first bits address is created with the first characters of the target
 * string appended with the characters after the final dot
 * 
 * thus the first bits from this first name is '1jimlk2'
 * 
 * The target string (e.g. "Jim Burton") is lower cased and illegal chars mapped
 * to their legal equivalents
 * 
 * @author jim burton
 * 
 */
public class FirstNameGenerator {
    private static final String HEADER_TEXT = "-----------------------\nFirstNameGenerator v0.1\n-----------------------";
    private static final String FIRST_BITS_MAGIC_TEXT = " does not match any address in the chain.";
    private static final String FIRST_BITS_HTTP_PREFIX = "http://firstbits.com/?a=";
    
    private static final int IDEAL_NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME = 3;
    private static final int INITIAL_LENGTH_OF_FIRST_BITS_TO_TRY = 5;
    private static final int MAXIMUM_LENGTH_OF_FIRST_BITS_TO_TRY = 34;
    private static final String SEPARATOR = ".";
    private static final String WALLET_SUFFIX = ".wallet";

    private static final String VALID_FIRSTNAME_ALPHABET = "0123456789abcdefghijklmnopqrstuvwxyz";

    public FirstNameGenerator(String targetFirstNamePrefix, int keyLimit) {
        int numberOfCharactersToMatch = workOutNumberOfCharactersToMatch(targetFirstNamePrefix);
        
        boolean metCriteria = false;

        while (!metCriteria) {
            ECKey key = searchForKeyMatchingFirstName(targetFirstNamePrefix, numberOfCharactersToMatch, keyLimit);
            String firstName = createFirstNameFromKey(targetFirstNamePrefix, numberOfCharactersToMatch, key);

            if (firstName != null) {
                // createWalletAndSaveKey(key, firstName);
                metCriteria = true;
            }
        }
    }

    /**
     * work out how many characters in the target string to match. It is
     * normally NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME but if you have a short
     * first name e.g. 'Jo' it drops to 2. The extra first bits get added to the
     * right hand side of the SEPARATOR
     * 
     * @param targetFirstNamePrefix
     * @return numberOfCharctersToMatch
     */
    private int workOutNumberOfCharactersToMatch(String targetFirstNamePrefix) {
        int numberOfCharactersToMatch = 0;

        while (numberOfCharactersToMatch < IDEAL_NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME) {
            if (targetFirstNamePrefix.length() > numberOfCharactersToMatch) {
                String nextCharacter = targetFirstNamePrefix
                        .substring(numberOfCharactersToMatch, numberOfCharactersToMatch + 1).toLowerCase();
                if (VALID_FIRSTNAME_ALPHABET.indexOf(nextCharacter) > -1) {
                    // valid firstname character
                    numberOfCharactersToMatch++;
                } else {
                    // invalid character - stop here
                    break;
                }
            } else {
                // no more targetFirstName - stop with what we have
                break;
            }
        }

        System.out.println("Matching for first " + numberOfCharactersToMatch + " characters.");
        return numberOfCharactersToMatch;
    }

    /**
     * search for a keypair that, when you lowercase the address, matches the targetFirstNamePrefix 
     * for the first numberOfCharctersToMatch characters
     * 
     * @param targetFirstNamePrefix
     * @param numberOfCharactersToMatch
     * @param keyLimit
     * @return
     */
    private ECKey searchForKeyMatchingFirstName(String targetFirstNamePrefix, int numberOfCharactersToMatch, int keyLimit) {
        boolean foundKeyMatchingTarget = false;
        int numberOfKeysTried = 0;

        NetworkParameters productionNet = NetworkParameters.prodNet();

        String addressPrefixToFind = targetFirstNamePrefix.toLowerCase();

        // zeroes are mapped to 'o' as they are not in the Base58 alphabet
        addressPrefixToFind = addressPrefixToFind.replaceAll("0", "o");
        addressPrefixToFind = addressPrefixToFind.substring(0, numberOfCharactersToMatch);

        while (!foundKeyMatchingTarget && numberOfKeysTried <= keyLimit) {
            // create a new key
            ECKey key = new ECKey();
            Address address = key.toAddress(productionNet);
            String addressString = address.toString();

            // see if the first numberOfCharactersToMatch letters of
            // the key (after the initial 1) match the target
            String addressCharactersToMatch = addressString.substring(1, numberOfCharactersToMatch + 1);
            addressCharactersToMatch = addressCharactersToMatch.toLowerCase();

            if (addressPrefixToFind.equals(addressCharactersToMatch)) {
                // we have found a suitable key
                System.out.println("SUCCESS - found key : " + key);
                System.out.println("address = " + key.toAddress(productionNet).toString() + "\n");
                return key;
            }

            numberOfKeysTried++;

            if (numberOfKeysTried % 1000 == 0) {
                System.out.println(numberOfKeysTried + " keys tried.");
            }
        }
        System.out.println("FAILURE - no suitable key was found.\n");

        return null;
    }

    /**
     * work out what the firstname is for a key
     * 
     * the main thing to determine is the length of the firstbits - this is scraped from firstbit.com
     * 
     * @param targetFirstNamePrefix
     * @param numberOfCharactersToMatch
     * @param key
     * @return firstname
     */
    public String createFirstNameFromKey(String targetFirstNamePrefix, int numberOfCharactersToMatch, ECKey key) {
        if (key == null) {
            // no key was found
            return null;
        }

        // get the address and lowercase it
        Address address = key.toAddress(NetworkParameters.prodNet());
        String addressString = address.toString();
        String addressStringLowerCase = addressString.toLowerCase();

        int numberOfFirstBitsToTry = INITIAL_LENGTH_OF_FIRST_BITS_TO_TRY;

        boolean foundFirstBits = false;

        while (!foundFirstBits && numberOfFirstBitsToTry <= addressStringLowerCase.length()
                && numberOfFirstBitsToTry <= MAXIMUM_LENGTH_OF_FIRST_BITS_TO_TRY) {
            String firstBitsToTry = addressStringLowerCase.substring(0, numberOfFirstBitsToTry);
            System.out.println("numberOfFirstBitsToTry = " + numberOfFirstBitsToTry);
            if (checkIfFirstBitsIsAvailable(firstBitsToTry)) {
                // success
                foundFirstBits = true;
                break;
            }
            numberOfFirstBitsToTry++;
        }

        if (foundFirstBits) {
            String firstName = targetFirstNamePrefix + SEPARATOR
                    + addressStringLowerCase.substring(numberOfCharactersToMatch + 1, numberOfFirstBitsToTry);
            System.out.println("\nSUCCESS - found firstname of : " + firstName);
            System.out.println("To claim this firstname you need to send some bitcoin\nto this address " + addressString
                    + " to get it onto the blockchain.");
            String firstBitsEquivalent = addressStringLowerCase.substring(0, numberOfFirstBitsToTry);
            System.out.println("The firstbits equivalent to this firstname is '" + firstBitsEquivalent + "'.");
            java.awt.Toolkit.getDefaultToolkit().beep();
            return firstName;
        } else {
            System.out.println("\nFAILURE - could not find firstname that was short enough.");
            return null;
        }
    }

    /**
     * check against firstBits.com if the key found is available
     * 
     * @param firstBits
     *            to try
     * @return true if firstBits is available
     */
    private boolean checkIfFirstBitsIsAvailable(String firstBits) {
        StringBuffer urlContents = new StringBuffer();

        URL firstBitsUrl;
        try {
            firstBitsUrl = new URL(FIRST_BITS_HTTP_PREFIX + firstBits);
            System.out.println("Checking against firstbits.com with URL of " + firstBitsUrl);

            BufferedReader in = new BufferedReader(new InputStreamReader(firstBitsUrl.openStream()));

            String inputLine;

            while ((inputLine = in.readLine()) != null) {
                urlContents.append(inputLine + "\n");
            }
            in.close();
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        // see if the magic text '<our firstbits> does not match any address in
        // the chain.' is on the page returned
        // if so then our address is ripe for becoming a first bits - we just
        // need to spend some cash
        String searchString = firstBits + FIRST_BITS_MAGIC_TEXT;
        // System.out.println("\n" + urlContents.toString() +"\n");
        int index = urlContents.toString().indexOf(searchString);

        if (index == -1) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * create a wallet and save the key for the firstname found
     * 
     * you can open this wallet using bitcoinj/ MultiBit
     * 
     * @param key
     * @param firstName
     */
    private void createWalletAndSaveKey(ECKey key, String firstName) {
        String walletFilename = firstName + WALLET_SUFFIX;
        File walletFile = new File(walletFilename);
        Wallet wallet = new Wallet(NetworkParameters.prodNet());

        wallet.keychain.add(key);

        try {
            wallet.saveToFile(walletFile);
            System.out.println("Saved key for firstname to wallet named :" + walletFile.getAbsolutePath());

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        System.out.println(HEADER_TEXT);
        System.out.print("Input the name you want to find a firstname for.\n> ");
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        boolean goodInput = false;
        while (!goodInput) {
            try {
                String targetFirstNamePrefix = reader.readLine();

                if (targetFirstNamePrefix != null && targetFirstNamePrefix.length() > 0) {
                    goodInput = true;
                    FirstNameGenerator firstNameGenerator = new FirstNameGenerator(targetFirstNamePrefix, 500000);
                } else {
                    System.out.print("FirstNameGenerator needs a non-empty string to work with.   Please try again.\n> ");
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
