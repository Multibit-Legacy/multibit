package org.multibit.firstname;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.net.MalformedURLException;
import java.net.URL;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Base58;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;

/**
 * class to generate firstnames
 * 
 * @author jim burton
 * 
 */
public class FirstNameGenerator {

    private static final BigInteger BASE27 = BigInteger.valueOf(27);
    private static final String BASE27_ALPHABET = "abcdefghijklmnopqrstuvwxyz.";

    private static final BigInteger BASE58 = BigInteger.valueOf(58);
    private static final String BASE58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

    public FirstNameGenerator(String targetFirstNamePrefix, BigInteger targetOffsetLimit, int keyLimit) {
        FirstNameData firstNameData = searchForFirstName(targetFirstNamePrefix, targetOffsetLimit, keyLimit);

        checkIfFirstBitsIsAvailable(firstNameData);

    }

    private FirstNameData searchForFirstName(String targetFirstNamePrefix, BigInteger targetOffsetLimit, int keyLimit) {
        // convert targetFirstNamePrefix to base 27
        BigInteger targetCode = convertToBase27(targetFirstNamePrefix);

        // work out how many characters of an address we will need to get
        // 'close' to this integer value (plus one because first char is always
        // '1')
        int numberOfAddressCharactersInBase58 = (int) Math.ceil(Math.log(targetCode.doubleValue()) / Math.log(58)) + 1;

        System.out.println("FirstNameGenerator#searchForFirstName - targetFirstNamePrefix = " + targetFirstNamePrefix);
        System.out.println("FirstNameGenerator#searchForFirstName - targetCode = " + targetCode);
        System.out.println("FirstNameGenerator#searchForFirstName - targetOffsetLimit = " + targetOffsetLimit);
        System.out.println("FirstNameGenerator#searchForFirstName - keyLimit = " + keyLimit);
        System.out.println("FirstNameGenerator#searchForFirstName - looking for a first bits of length = "
                + numberOfAddressCharactersInBase58);

        boolean foundFirstName = false;
        BigInteger bestOffsetSoFar = null;
        FirstNameData bestFirstNameSoFar = null;

        int numberOfKeysTried = 0;

        while (!foundFirstName && numberOfKeysTried <= keyLimit) {
            // create a new key
            ECKey key = new ECKey();
            Address address = key.toAddress(NetworkParameters.prodNet());
            String addressString = address.toString();

            // chop down to numberOfAddressCharactersInBase58 - this is our
            // candidate first bits address
            String candidateString = addressString.substring(0, numberOfAddressCharactersInBase58);

            try {
                // convert candidate string to an int in base 58
                BigInteger candidateCode = Base58.decodeToBigInteger(candidateString);

                // see what the offset is
                BigInteger offset = candidateCode.add(targetCode.negate());

                if (offset.compareTo(BigInteger.ZERO) < 0) {
                    // we are not interested in a candidate with code less than
                    // the target
                    continue;
                } else {
                    if (bestOffsetSoFar == null || offset.compareTo(bestOffsetSoFar) < 0) {
                        String firstName = targetFirstNamePrefix + offset.toString();
                        FirstNameData firstNameData = new FirstNameData();
                        ;
                        firstNameData.key = key;
                        firstNameData.firstName = firstName;
                        firstNameData.firstBits = convertFirstNameToFirstBits(firstName);
                        outputCurrentStatus(firstNameData);
                        bestOffsetSoFar = offset;
                        bestFirstNameSoFar = firstNameData;
                    }

                    if (offset.compareTo(targetOffsetLimit) < 0) {
                        foundFirstName = true;
                        System.out.println("FirstNameGenerator#searchForFirstName - Done.");
                    }
                }

                if (numberOfKeysTried % 100 == 0) {
                    System.out.println("FirstNameGenerator#searchForFirstName - " + numberOfKeysTried + " keys tried.");
                }

                if (numberOfKeysTried == keyLimit) {
                    System.out
                            .println("FirstNameGenerator#searchForFirstName - Stopping - limit of keys tried.\nBest FirstName found was:\n");
                    outputCurrentStatus(bestFirstNameSoFar);
                }
                numberOfKeysTried++;

            } catch (AddressFormatException e) {
                e.printStackTrace();
            }
        }

        return bestFirstNameSoFar;
    }

    /**
     * check against firstBits.com if the key found is available
     * 
     * if it is, firstbits will not return an address for the firstbits we
     * calculate from the firstname
     * 
     * @param key
     */
    private void checkIfFirstBitsIsAvailable(FirstNameData firstNameData) {
        // http://firstbits.com/?a=1qbcva
        StringBuffer urlContents = new StringBuffer();

        URL firstBitsUrl;
        try {
            firstBitsUrl = new URL("http://firstbits.com/?a=" + firstNameData.firstBits);
            //firstBitsUrl = new URL("http://firstbits.com/?a=15x81");
            System.out.println("\\FirstNameGenerator#searchForFirstName - Checking against firstbits.com with URL of " + firstBitsUrl);

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
        
        // see if the magic text '<our firstbits> does not match any address in the chain.' is on the page returned
        // if so then our address is ripe for becoming a first bits - we just need to spend some cash
        String searchString = firstNameData.firstBits + " does not match any address in the chain.";
        //System.out.println("\n" + urlContents.toString() +"\n");
        int index = urlContents.toString().indexOf(searchString);
        
        if (index == -1) {
            System.out.println("FirstNameGenerator#searchForFirstName - FAILURE. firstbits.com informs us that the firstbits of " + firstNameData.firstBits + " is already on the blockchain");                   
        } else {
            System.out.println("FirstNameGenerator#searchForFirstName - SUCCESS. firstbits.com does not yet have the firstbits of " + firstNameData.firstBits);
            System.out.println("FirstNameGenerator#searchForFirstName - spending bitcoin using this address will get you the firstName of " + firstNameData.firstName);
        }
    }

    private void outputCurrentStatus(FirstNameData firstNameData) {
        System.out.println("\nFirstNameGenerator#searchForFirstName - address = "
                + firstNameData.key.toAddress(NetworkParameters.prodNet()));
        System.out.println("FirstNameGenerator#searchForFirstName - keyPair = " + firstNameData.key);
        System.out.println("FirstNameGenerator#searchForFirstName - firstName = " + firstNameData.firstName);
        System.out.println("FirstNameGenerator#searchForFirstName - firstBits = " + firstNameData.firstBits);
    }

    private String convertFirstNameToFirstBits(String firstName) {
        // split the name into the alpha part and the numeric part;
        String alphaPart = "";
        String numericPart = "";

        for (int i = 0; i < firstName.length(); i++) {
            int alphaIndex = BASE27_ALPHABET.indexOf(firstName.charAt(i));
            if (alphaIndex == -1) {
                numericPart = numericPart + firstName.charAt(i);
            } else {
                alphaPart = alphaPart + firstName.charAt(i);
            }
        }

        BigInteger address = convertToBase27(alphaPart).add(new BigInteger(numericPart));
        String addressAsString = ("1" + encodeToBase58(address)).toLowerCase();
        return addressAsString;
    }

    private BigInteger convertToBase27(String inputString) {
        inputString = inputString.toLowerCase();

        int inputStringLength = inputString.length();
        BigInteger convertedCode = BigInteger.ZERO;
        for (int i = 0; i < inputStringLength; i++) {
            int alphaIndex = BASE27_ALPHABET.indexOf(inputString.charAt(i));
            if (alphaIndex == -1) {
                throw new IllegalArgumentException("Cannot convert string '" + inputString.charAt(i) + "'");
            }
            convertedCode = convertedCode.multiply(BASE27).add(new BigInteger(alphaIndex + ""));
        }
        return convertedCode;
    }

    /**
     * encode a BigInteger to a Base58 String
     * 
     * @param input
     * @return
     */
    private String encodeToBase58(BigInteger input) {
        StringBuffer stringBuffer = new StringBuffer();
        while (input.compareTo(BASE58) >= 0) {
            BigInteger mod = input.mod(BASE58);
            stringBuffer.insert(0, BASE58_ALPHABET.charAt(mod.intValue()));
            input = input.subtract(mod).divide(BASE58);
        }
        stringBuffer.insert(0, BASE58_ALPHABET.charAt(input.intValue()));

        return stringBuffer.toString();
    }

    public static void main(String[] args) {
        // get the target firstname prefix - arg 0
        String targetFirstNamePrefix = "burton";

        BigInteger targetOffsetLimit = new BigInteger("9999");
        int keysLimit = 10000;

        FirstNameGenerator firstNameGenerator = new FirstNameGenerator(targetFirstNamePrefix, targetOffsetLimit, keysLimit);
    }

    class FirstNameData {
        public ECKey key;
        public String firstName;
        public String firstBits;
    }
}
