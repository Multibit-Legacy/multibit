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
 * in this variant the format of the firstName is:
 * 
 * <targetString><offset><length of firstBits in hex>
 * 
 * e.g. firstName = burton71185 firstBits = 13eqpu
 * 
 * @author jim burton
 * 
 */
public class FirstNameGenerator3 {

    private static final BigInteger BASE27 = BigInteger.valueOf(27);
    private static final String BASE27_ALPHABET = "abcdefghijklmnopqrstuvwxyz.";

    private static final BigInteger BASE58 = BigInteger.valueOf(58);
    private static final String BASE58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

    private static final String PRODUCTION_NET_ADDRESS_PREFIX = "1";

    private static final int INITIAL_FIRST_BITS_LENGTH = 5;
    private static final int MAXiMUM_FIRST_BITS_LENGTH = 9;
    
    private static final BigInteger OFFSET_SCALING_FACTOR = BigInteger.valueOf(10);   // how much the offset increases when you go to a longer first bits length
    

    public FirstNameGenerator3(String targetFirstNamePrefix, BigInteger targetOffsetLimit, int keyLimit,
            int maxScrapesPerFirstBitsLength) {
        try {
            searchForFirstName(targetFirstNamePrefix, targetOffsetLimit, keyLimit, maxScrapesPerFirstBitsLength);
        } catch (AddressFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private void searchForFirstName(String targetFirstNamePrefix, BigInteger targetOffsetLimit, int keyLimitPerFirstBitsLength,
            int maxScrapesPerFirstBitsLength) throws AddressFormatException {

        int firstBitsLength = INITIAL_FIRST_BITS_LENGTH;

        System.out.println("FirstNameGenerator#searchForFirstName - targetFirstNamePrefix = " + targetFirstNamePrefix);
        System.out
                .println("FirstNameGenerator#searchForFirstName - keyLimitPerFirstBitsLength = " + keyLimitPerFirstBitsLength);

        NetworkParameters productionNet = NetworkParameters.prodNet();

        boolean foundFirstName = false;
        FirstNameData successfulFirstName = null;

        while (!foundFirstName && firstBitsLength <= MAXiMUM_FIRST_BITS_LENGTH) {
            // convert targetFirstNamePrefix to base 27 target code
            if (targetFirstNamePrefix.length() < firstBitsLength) {
                // we have searched the whole length of the taget string - give up
                break;
            }
            BigInteger targetCode27 = convertToBase27(targetFirstNamePrefix);
            String targetAddressLong = PRODUCTION_NET_ADDRESS_PREFIX + encodeToBase58(targetCode27);
            String targetAddressShort = targetAddressLong.substring(0, firstBitsLength);
            BigInteger targetCode = Base58.decodeToBigInteger(targetAddressShort);

            System.out.println("FirstNameGenerator#searchForFirstName - looking for a first bits of length = "
                    + firstBitsLength);
            System.out.println("FirstNameGenerator#searchForFirstName - targetCode = " + targetCode);
            System.out.println("FirstNameGenerator#searchForFirstName - targetOffsetLimit = " + targetOffsetLimit);

            BigInteger bestOffsetSoFar = null;
            FirstNameData bestFirstNameSoFar = null;

            int numberOfKeysTriedForThisFirstBitLength = 0;
            int numberOfScrapesTriedForThisFirstBitLength = 0;
            String previousFirstBitsCheckedAtThisFirstBitLength = null;

            while (!foundFirstName && numberOfKeysTriedForThisFirstBitLength <= keyLimitPerFirstBitsLength) {
                // create a new key
                ECKey key = new ECKey();
                Address address = key.toAddress(productionNet);
                String addressString = address.toString();

                // chop down to numberOfAddressCharactersInBase58 - this is our
                // candidate first bits address
                String candidateString = addressString.substring(0, firstBitsLength);

                try {
                    // convert candidate string to an int in base 58
                    BigInteger candidateCode = Base58.decodeToBigInteger(candidateString);

                    // see what the offset is
                    BigInteger offset = candidateCode.add(targetCode.negate());

                    // we are not interested in a candidate with code less than
                    // the target
                    if (offset.compareTo(BigInteger.ZERO) > 0) {
                        FirstNameData firstNameData = new FirstNameData();
                        String firstName = targetFirstNamePrefix + offset.toString() + firstBitsLength;
                        firstNameData.key = key;
                        firstNameData.firstName = firstName;
                        firstNameData.firstBits = convertFirstNameToFirstBits(firstName);
                        firstNameData.firstBitsLength = firstBitsLength;

                        if (offset.compareTo(targetOffsetLimit) < 0) {
                            // if this firstBits has already been checked we have run out of precision on this first bits - give up this level
                            if (firstNameData.firstBits.equals(previousFirstBitsCheckedAtThisFirstBitLength)) {
                                break;
                            }
                            previousFirstBitsCheckedAtThisFirstBitLength = firstNameData.firstBits;
                            
                            // check if this firstbits is available
                            boolean isAvailable = checkIfFirstBitsIsAvailable(firstNameData.firstBits);

                            numberOfScrapesTriedForThisFirstBitLength++;

                            if (isAvailable) {
                                foundFirstName = true;
                                bestOffsetSoFar = offset;
                                bestFirstNameSoFar = firstNameData;
                                successfulFirstName = bestFirstNameSoFar;
                                System.out.println("FirstNameGenerator#searchForFirstName - Done.");
                                break;
                            }

                            if (numberOfScrapesTriedForThisFirstBitLength >= maxScrapesPerFirstBitsLength) {
                                // we give up at this first bits length - cannot
                                // find an available first bits
                                break;
                            }
                        } else {
                            // this key is the best so far but is not a winner
                            if (bestOffsetSoFar == null || offset.compareTo(bestOffsetSoFar) < 0) {
                                outputCurrentStatus(firstNameData);
                                bestOffsetSoFar = offset;
                                bestFirstNameSoFar = firstNameData;
                            }

                        }
                    }

                    if (numberOfKeysTriedForThisFirstBitLength % 100 == 0) {
                        System.out.println("FirstNameGenerator#searchForFirstName - " + numberOfKeysTriedForThisFirstBitLength
                                + " keys tried at first bits length = " + firstBitsLength);
                    }
                    numberOfKeysTriedForThisFirstBitLength++;

                } catch (AddressFormatException e) {
                    e.printStackTrace();
                }
            }
            firstBitsLength++;
            targetOffsetLimit = targetOffsetLimit.multiply(OFFSET_SCALING_FACTOR);
        }

        if (foundFirstName) {
            System.out.println("FirstNameGenerator#searchForFirstName - SUCCESS - found key as follows:");
            outputCurrentStatus(successfulFirstName);
            System.out
                    .println("FirstNameGenerator#searchForFirstName - sending bitcoin to this address will get you the firstName of "
                            + successfulFirstName.firstName);
        } else {
            System.out
                    .println("FirstNameGenerator#searchForFirstName - FAILURE. Could not find a firstName meeting your criteria");
        }

    }

    /**
     * check against firstBits.com if the key found is available
     * 
     * if it is, firstbits will not return an address for the firstbits we
     * calculate from the firstname
     * 
     * @param key
     * @return true if firstBits is available, false if already used
     */
    private boolean checkIfFirstBitsIsAvailable(String firstBits) {
        StringBuffer urlContents = new StringBuffer();

        URL firstBitsUrl;
        try {
            firstBitsUrl = new URL("http://firstbits.com/?a=" + firstBits);
            System.out.println("FirstNameGenerator#searchForFirstName - Checking against firstbits.com with URL of "
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

    private void outputCurrentStatus(FirstNameData firstNameData) {
        System.out.println("\nFirstNameGenerator#searchForFirstName - address = "
                + firstNameData.key.toAddress(NetworkParameters.prodNet()));
        System.out.println("FirstNameGenerator#searchForFirstName - keyPair = " + firstNameData.key);
        System.out.println("FirstNameGenerator#searchForFirstName - firstName = " + firstNameData.firstName);
        System.out.println("FirstNameGenerator#searchForFirstName - firstBits = " + firstNameData.firstBits);
        System.out.println("FirstNameGenerator#searchForFirstName - firstBitsLength = " + firstNameData.firstBitsLength);
    }

    private String convertFirstNameToFirstBits(String firstName) throws AddressFormatException {
        // split the name into the alpha part and the numeric part;
        firstName = firstName.toLowerCase();
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

        // the last digit of the numeric part is the firstbits length
        int firstBitsLength = Integer.parseInt(numericPart.substring(numericPart.length() - 1, numericPart.length()));
        
        numericPart = numericPart.substring(0, numericPart.length() - 1);       
        BigInteger address = convertToBase27(alphaPart);
        String addressAsString = PRODUCTION_NET_ADDRESS_PREFIX + encodeToBase58(address);
        addressAsString = addressAsString.substring(0, firstBitsLength);
        
        BigInteger truncatedAddressCode = Base58.decodeToBigInteger(addressAsString);

        // see what the offset is
        BigInteger fitstBitsAddressCode = truncatedAddressCode.add(BigInteger.valueOf(Long.parseLong(numericPart)));
        String firstBitsAddressAsString = encodeToBase58(fitstBitsAddressCode);

        firstBitsAddressAsString = (PRODUCTION_NET_ADDRESS_PREFIX + firstBitsAddressAsString).toLowerCase();
        firstBitsAddressAsString = firstBitsAddressAsString.substring(0, firstBitsLength);
        return firstBitsAddressAsString;
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
        String targetFirstNamePrefix = "MultiBit";

        BigInteger targetOffsetLimit = new BigInteger("100000");
        int keyLimitPerFirstBitsLength = 8000;
        int maxScrapesPerFirstBitsLength = 6;

        FirstNameGenerator3 firstNameGenerator = new FirstNameGenerator3(targetFirstNamePrefix, targetOffsetLimit,
                keyLimitPerFirstBitsLength, maxScrapesPerFirstBitsLength);
    }

    class FirstNameData {
        public ECKey key;
        public String firstName;
        public String firstBits;
        public int firstBitsLength;
    }
}
