package org.multibit.qrcode;

import com.google.bitcoin.core.NetworkParameters;
import org.junit.Test;

import java.math.BigInteger;

import static org.junit.Assert.*;

public class BitcoinURITest {

    private BitcoinURI testObject = null;

    private static final String PRODNET_GOOD_ADDRESS = "1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH";

    /**
     * Handles a badly formatted amount field
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testConvertToBitcoinURI() throws BitcoinURIParseException {

        assertEquals("bitcoin:abc123?amount=12.34&label=Hello", BitcoinURI.convertToBitcoinURI("abc123", "12.34", "Hello"));

    }

    /**
     * Test the simplest well-formed URI
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Simple() throws BitcoinURIParseException {

        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS);
        assertNotNull(testObject);
        assertNull("Unexpected amount", testObject.getAmount());
        assertNull("Unexpected label", testObject.getLabel());
        assertEquals("Unexpected label", 20, testObject.getAddress().getHash160().length);

    }

    /**
     * Test missing constructor parameters
     */
    @Test
    public void testBad_Constructor() {

        try {
            testObject = new BitcoinURI(null, "blimpcoin:" + PRODNET_GOOD_ADDRESS);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("networkParameters"));
        }

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("networkParameters"));
        }

    }

    /**
     * Test a broken URI (bad scheme)
     */
    @Test
    public void testBad_Scheme() {

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), "blimpcoin:" + PRODNET_GOOD_ADDRESS);
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("Bad scheme"));
        }
    }

    /**
     * Test a broken URI (bad syntax)
     */
    @Test
    public void testBad_BadSyntax() {

        // Various illegal characters
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + "|" + PRODNET_GOOD_ADDRESS);
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("Bad URI syntax"));
        }

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "\\");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("Bad URI syntax"));
        }

        // Separator without field
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("Bad URI syntax"));
        }
    }

    /**
     * Test a broken URI (missing address)
     */
    @Test
    public void testBad_Address() {

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME);
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("Bad address"));
        }
    }

    /**
     * Test a broken URI (missing address)
     */
    @Test
    public void testBad_IncorrectAddressType() {

        try {
            testObject = new BitcoinURI(NetworkParameters.testNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS);
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("Bad address"));
        }
    }

    /**
     * Handles a simple amount
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Amount() throws BitcoinURIParseException {

        // Test the decimal parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=9876543210.12345678");
        assertEquals("987654321012345678", testObject.getAmount().toString());

        // Test the decimal parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=.12345678");
        assertEquals("12345678", testObject.getAmount().toString());

        // Test the integer parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=9876543210");
        assertEquals("987654321000000000", testObject.getAmount().toString());

    }

    /**
     * Handles a simple label
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Label() throws BitcoinURIParseException {

        // Test the integer parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?label=Hello%20World");
        assertEquals("Hello World", testObject.getLabel());

    }

    /**
     * Handles a simple message
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Message() throws BitcoinURIParseException {
        // Test the integer parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?message=Hello%20World");
        assertEquals("Hello World", testObject.getMessage());

    }

    /**
     * Handles a simple message with no URL encoding (early MultiBit URLs did not encode spaces)
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Message_Wrong_Space_Encoding() throws BitcoinURIParseException {
        // Test the integer parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?message=Hello World");
        assertEquals("Hello World", testObject.getMessage());

    }

    /**
     * Handles a simple expiry (far future)
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Expires() throws BitcoinURIParseException {

        // Test the integer parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?req-expires=2100-01-01T23:59:59Z");
        assertEquals(4102531199000L, testObject.getExpires().getTime());

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?req-expires=2000-01-01T23:59:59Z");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("req-expires"));
        }
    }

    /**
     * Handles various well-formed combinations
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Combinations() throws BitcoinURIParseException {

        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=9876543210&label=Hello%20World&message=Be%20well");
        assertEquals("BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','amount'='987654321000000000','label'='Hello World','message'='Be well']", testObject.toString());

    }

    /**
     * Handles various badly-formed combinations
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testBad_Combinations() throws BitcoinURIParseException {

        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=9876543210&label=Hello%20World&message=Be%20well");
        assertEquals("BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','amount'='987654321000000000','label'='Hello World','message'='Be well']", testObject.toString());

    }

    /**
     * Handles a badly formatted amount field
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testBad_Amount() throws BitcoinURIParseException {

        // Missing
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("amount"));
        }

        // Non-decimal (BIP 21)
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?amount=12X4");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("amount"));
        }
    }

    /**
     * Handles a badly formatted label field
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testBad_Label() throws BitcoinURIParseException {

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?label=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("label"));
        }

    }

    /**
     * Handles a badly formatted message field
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testBad_Message() throws BitcoinURIParseException {

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?message=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("message"));
        }

    }

    /**
     * Handles a badly formatted expires field
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testBad_Expires() throws BitcoinURIParseException {

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?req-expires=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("req-expires"));
        }

        // Format
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?req-expires=01-01-2000");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("req-expires"));
        }

    }

    /**
     * Handles duplicated fields (sneaky address overwrite attack)
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testBad_Duplicated() throws BitcoinURIParseException {

        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?address=aardvark");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("address"));
        }

    }

    /**
     * Handles unknown fields (required and not required)
     *
     * @throws BitcoinURIParseException If something goes wrong
     */
    @Test
    public void testGood_Unknown() throws BitcoinURIParseException {

        // Unknown not required field
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?aardvark=true");
        assertEquals("BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','aardvark'='true']", testObject.toString());

        // Unknown not required field (isolated)
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?aardvark");
        assertEquals("BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','aardvark'='aardvark']", testObject.toString());

        // Unknown and required field
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?req-aardvark=true");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("req-aardvark"));
        }

    }

}
