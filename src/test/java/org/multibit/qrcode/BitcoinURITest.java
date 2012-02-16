package org.multibit.qrcode;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import com.google.bitcoin.core.NetworkParameters;

public class BitcoinURITest {

    private BitcoinURI testObject = null;

    private static final String PRODNET_GOOD_ADDRESS = "1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH";

    /**
     * Tests conversion to Bitcoin URI
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testConvertToBitcoinURI() throws BitcoinURIParseException {
        assertEquals("bitcoin:abc123?amount=12.34&label=Hello&message=AMessage", BitcoinURI.convertToBitcoinURI("abc123", "12.34", "Hello", "AMessage"));
        
        // address null
        try {
            BitcoinURI.convertToBitcoinURI(null, "0.1", "hope", "glory");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("address"));
        }
        
        // address empty string
        try {
            BitcoinURI.convertToBitcoinURI("", "0.1", "hope", "glory");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertTrue(e.getMessage().contains("address"));
        }

        // no amount, label present, message present
        assertEquals("bitcoin:abc123?label=Hello&message=glory", BitcoinURI.convertToBitcoinURI("abc123", null, "Hello", "glory"));
        assertEquals("bitcoin:abc123?label=Hello&message=glory", BitcoinURI.convertToBitcoinURI("abc123", "", "Hello", "glory"));
        
        // amount present, no label, message present
        assertEquals("bitcoin:abc123?amount=0.1&message=glory", BitcoinURI.convertToBitcoinURI("abc123", "0.1", null, "glory"));
        assertEquals("bitcoin:abc123?amount=0.1&message=glory", BitcoinURI.convertToBitcoinURI("abc123", "0.1", "", "glory"));

        // amount present, label present, no message
        assertEquals("bitcoin:abc123?amount=0.2&label=Hello", BitcoinURI.convertToBitcoinURI("abc123", "0.2", "Hello", null));
        assertEquals("bitcoin:abc123?amount=0.2&label=Hello", BitcoinURI.convertToBitcoinURI("abc123", "0.2", "Hello", ""));
        
        
        // amount present, no label, no message
        assertEquals("bitcoin:abc123?amount=0.1", BitcoinURI.convertToBitcoinURI("abc123", "0.1", null, null));
        assertEquals("bitcoin:abc123?amount=0.1", BitcoinURI.convertToBitcoinURI("abc123", "0.1", "", ""));
        
        // no amount, label present, no message
        assertEquals("bitcoin:abc123?label=Hello", BitcoinURI.convertToBitcoinURI("abc123", null, "Hello", null));
        assertEquals("bitcoin:abc123?label=Hello", BitcoinURI.convertToBitcoinURI("abc123", "", "Hello", ""));
        
        // no amount, no label, message present
        assertEquals("bitcoin:abc123?message=Agatha", BitcoinURI.convertToBitcoinURI("abc123", null, null, "Agatha"));
        assertEquals("bitcoin:abc123?message=Agatha", BitcoinURI.convertToBitcoinURI("abc123", "", "", "Agatha"));
 
        
        // no amount, no label, no message
        assertEquals("bitcoin:abc123", BitcoinURI.convertToBitcoinURI("abc123", null, null, null));
        assertEquals("bitcoin:abc123", BitcoinURI.convertToBitcoinURI("abc123", "", "", ""));
    }

    /**
     * Test the simplest well-formed URI
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
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
            assertTrue(e.getMessage().contains("input"));
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
            assertTrue(e.getMessage().contains("Missing address"));
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
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_Amount() throws BitcoinURIParseException {
        // Test the decimal parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?amount=9876543210.12345678");
        assertEquals("987654321012345678", testObject.getAmount().toString());

        // Test the decimal parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?amount=.12345678");
        assertEquals("12345678", testObject.getAmount().toString());

        // Test the integer parsing
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?amount=9876543210");
        assertEquals("987654321000000000", testObject.getAmount().toString());
    }

    /**
     * Handles a simple label
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_Label() throws BitcoinURIParseException {
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?label=Hello%20World");
        assertEquals("Hello%20World", testObject.getLabel());
    }

    /**
     * Handles a simple label with an embedded ampersand
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_LabelWithAmpersand() throws BitcoinURIParseException {
        String encodedLabel = BitcoinURI.encodeURLString("Hello Earth & Mars");
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?label="
                + encodedLabel);
        assertEquals(encodedLabel, testObject.getLabel());
    }

    /**
     * Handles a Russian label (Unicode test)
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_LabelWithRussian() throws BitcoinURIParseException {
        String encodedLabel = BitcoinURI.encodeURLString("\u041c\u043e\u0441\u043a\u0432\u0430"); // Moscow
                                                                                                  // in
                                                                                                  // Russian
                                                                                                  // in
                                                                                                  // Cyrillic
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS + "?label="
                + encodedLabel);
        assertEquals(encodedLabel, testObject.getLabel());
    }

    /**
     * Handles a simple message
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_Message() throws BitcoinURIParseException {
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?message=Hello%20World");
        assertEquals("Hello%20World", testObject.getMessage());
    }

    /**
     * Handles a simple message with no URL encoding (early MultiBit URLs did
     * not encode spaces so it is lenient)
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_Message_Wrong_Space_Encoding() throws BitcoinURIParseException {
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?message=Hello World");
        assertEquals("Hello%20World", testObject.getMessage());
    }

    /**
     * Handles various well-formed combinations
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testGood_Combinations() throws BitcoinURIParseException {
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?amount=9876543210&label=Hello%20World&message=Be%20well");
        assertEquals(
                "BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','amount'='987654321000000000','label'='Hello%20World','message'='Be%20well']",
                testObject.toString());
    }

    /**
     * Handles various badly-formed combinations
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testBad_Combinations() throws BitcoinURIParseException {
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?amount=9876543210&label=Hello%20World&message=Be%20well");
        assertEquals(
                "BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','amount'='987654321000000000','label'='Hello%20World','message'='Be%20well']",
                testObject.toString());
    }

    /**
     * Handles a badly formatted amount field
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testBad_Amount() throws BitcoinURIParseException {
        // Missing
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?amount=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("amount"));
        }

        // Non-decimal (BIP 21)
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?amount=12X4");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("amount"));
        }
    }

    /**
     * Handles a badly formatted label field
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testBad_Label() throws BitcoinURIParseException {
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?label=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("label"));
        }
    }

    /**
     * Handles a badly formatted message field
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testBad_Message() throws BitcoinURIParseException {
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?message=");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("message"));
        }
    }

    /**
     * Handles duplicated fields (sneaky address overwrite attack)
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testBad_Duplicated() throws BitcoinURIParseException {
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?address=aardvark");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("address"));
        }
    }

    /**
     * Handles unknown fields (required and not required)
     * 
     * @throws BitcoinURIParseException
     *             If something goes wrong
     */
    @Test
    public void testUnknown() throws BitcoinURIParseException {
        // Unknown not required field
        testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                + "?aardvark=true");
        assertEquals("BitcoinURI['address'='1KzTSfqjF2iKCduwz59nv2uqh1W2JsTxZH','aardvark'='true']", testObject.toString());

        assertEquals("true", (String) testObject.getParameterByName("aardvark"));

        // Unknown not required field (isolated)
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?aardvark");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("does not have a value"));
        }

        // Unknown and required field
        try {
            testObject = new BitcoinURI(NetworkParameters.prodNet(), BitcoinURI.BITCOIN_SCHEME + ":" + PRODNET_GOOD_ADDRESS
                    + "?req-aardvark=true");
            fail("Expecting BitcoinURIParseException");
        } catch (BitcoinURIParseException e) {
            assertTrue(e.getMessage().contains("req-aardvark"));
        }
    }
}
