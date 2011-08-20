package org.multibit.firstname;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;

/**
 * class to generate firstnames
 * 
 * firstnames are of the format:
 * 
 * Jim Burton.lk2 
 * ^^^ first NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME chars are used in first bits address +
 * chars after the final dot
 * 
 * thus the first bits is jimlk2 
 * lower cased and illegal chars mapped to their
 * legal equivalents
 * 
 * @author jim burton
 * 
 */
public class FirstNameGenerator {
    private static final int NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME = 3;
    private static final int INITIAL_LENGTH_OF_FIRST_BITS_TO_TRY = 5;
    private static final String SEPARATOR = ".";

    public FirstNameGenerator(String targetFirstNamePrefix, int keyLimit) {
        ECKey key = searchForKeyMatchingFirstName(targetFirstNamePrefix, keyLimit);
        createFirstNameFromKey(targetFirstNamePrefix, key);
    }

    private ECKey searchForKeyMatchingFirstName(String targetFirstNamePrefix, int keyLimit) {
        System.out.println("targetFirstNamePrefix = " + targetFirstNamePrefix);
        System.out.println("keyLimit = " + keyLimit);

        boolean foundKeyMatchingTarget = false;
        int numberOfKeysTried = 0;

        NetworkParameters productionNet = NetworkParameters.prodNet();

        String addressPrefixToFind = targetFirstNamePrefix.toLowerCase();

        // zeroes are mapped to 'o' as they are not in the Base58 alphabet
        addressPrefixToFind = addressPrefixToFind.replace('0', 'o');

        if (addressPrefixToFind.length() < NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME) {
            throw new IllegalStateException("targetFirstNamePrefix must be " + NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME
                    + " characters or more");
        } else {
            addressPrefixToFind = addressPrefixToFind.substring(0, NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME);
        }

        while (!foundKeyMatchingTarget && numberOfKeysTried <= keyLimit) {
            // create a new key
            ECKey key = new ECKey();
            Address address = key.toAddress(productionNet);
            String addressString = address.toString();

            // see if the first NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME letters of the key (after the initial 1) match
            // the target
            String addressCharactersToMatch = addressString.substring(1, NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME + 1);
            addressCharactersToMatch = addressCharactersToMatch.toLowerCase();

            if (addressPrefixToFind.equals(addressCharactersToMatch)) {
                // we have found a suitable key
                System.out.println("SUCCESS - found key : " + key);
                System.out.println("address = "
                        + key.toAddress(productionNet).toString());
                java.awt.Toolkit.getDefaultToolkit().beep();

                return key;
            }

            numberOfKeysTried++;

            if (numberOfKeysTried % 1000 == 0) {
                System.out.println(numberOfKeysTried + " keys tried.");
            }
        }
        System.out.println("FAILURE - no suitable key was found.");
        java.awt.Toolkit.getDefaultToolkit().beep();

        return null;
    }

    public void createFirstNameFromKey(String targetFirstNamePrefix, ECKey key) {
        if (key == null) {
            // no key found
            return;
        }
        
        // get the address and lowercase it
        Address address = key.toAddress(NetworkParameters.prodNet());
        String addressString = address.toString().toLowerCase();
        
        int numberOfFirstBitsToTry = INITIAL_LENGTH_OF_FIRST_BITS_TO_TRY;
        
        boolean foundFirstBits = false;
        
        while (!foundFirstBits && numberOfFirstBitsToTry <= addressString.length()) {
            String firstBitsToTry = addressString.substring(0, numberOfFirstBitsToTry);
            if (checkIfFirstBitsIsAvailable(firstBitsToTry)) {
                // success
                foundFirstBits = true;
                break;
            } 
            numberOfFirstBitsToTry++;
            System.out.println("numberOfFirstBitsToTry = " + numberOfFirstBitsToTry);
        }

        if (foundFirstBits) {
            String firstName = targetFirstNamePrefix + SEPARATOR
            + addressString.substring(NUMBER_OF_CHARACTERS_TO_MATCH_IN_NAME + 1, numberOfFirstBitsToTry);
            System.out.println("SUCCESS - found FirstName of : " + firstName);
        }
    }

    /**
     * check against firstBits.com if the key found is available
     * 
     * if it is, firstbits will not return an address for the firstbits we
     * calculate from the firstname
     * 
     * @param firstBits to try
     * @return true if firstBits is available
     */
    private boolean checkIfFirstBitsIsAvailable(String firstBits) {
        StringBuffer urlContents = new StringBuffer();

        URL firstBitsUrl;
        try {
            firstBitsUrl = new URL("http://firstbits.com/?a=" + firstBits);
            System.out.println("Checking against firstbits.com with URL of "
                    + firstBitsUrl);

            BufferedReader in = new BufferedReader(new InputStreamReader(firstBitsUrl.openStream()));

            String inputLine;

            while ((inputLine = in.readLine()) != null) {
                urlContents.append(inputLine + "\n");
            }
            in.close();
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // see if the magic text '<our firstbits> does not match any address in
        // the chain.' is on the page returned
        // if so then our address is ripe for becoming a first bits - we just
        // need to spend some cash
        String searchString = firstBits + " does not match any address in the chain.";
        // System.out.println("\n" + urlContents.toString() +"\n");
        int index = urlContents.toString().indexOf(searchString);

        if (index == -1) {
            return false;
        } else {
            return true;
        }
    }
    
    public static void main(String[] args) {
        String targetFirstNamePrefix = "google";
        int keyLimit = 100000;

        FirstNameGenerator firstNameGenerator = new FirstNameGenerator(targetFirstNamePrefix, keyLimit);
    }
}
