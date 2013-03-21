/**
 * Copyright 2013 multibit.org
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
package org.multibit.network;

import java.security.SignatureException;
import java.util.List;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Utils;

public class AlertManagerTest extends TestCase {
    @Test
    public void testAlertManagerURIToGet() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new TestLocaliser();
        MultiBitModel model = new MultiBitModel(controller);
        
        controller.setLocaliser(localiser);
        controller.setModel(model);   
        
        // Initialise and check
        AlertManager alertManager = AlertManager.INSTANCE;
        assertNotNull(alertManager);
        
        alertManager.initialise(controller, null);
        
        // Check the default source of the version.txt is correct.
        assertEquals("Wrong default version test uri", "https://www.multibit.org/version.txt", alertManager.getVersionUrlToGet());
    }
    
    @Test
    public void testAlertManagerVersion() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new TestLocaliser();
        MultiBitModel model = new MultiBitModel(controller);
        
        controller.setLocaliser(localiser);
        controller.setModel(model);   
        
        // Initialise and check
        AlertManager alertManager = AlertManager.INSTANCE;
        assertNotNull(alertManager);
        
        alertManager.initialise(controller, null);
        
        // Set up a test "remote version" string buffer.
        StringBuffer versionText;
        
        // Set version text to be just a version number (no message).
        
        // Local version is the same as the "remote" version
        versionText = new StringBuffer();
        versionText.append("0.4.23");       
        ParseResult parseResult = alertManager.parseAndCheckVersionText(versionText.toString());
        assertNotNull(parseResult);
        assertEquals("Case 1", false, parseResult.isNewVersionIsAvailable());
        assertEquals("0.4.23", parseResult.getLocalVersion());
        assertEquals("0.4.23", parseResult.getVersionOnServer());
        
        // Local version is behind of the "remote" version - we would want to upgrade
        versionText = new StringBuffer();
        versionText.append("0.5.1");       
        parseResult = alertManager.parseAndCheckVersionText(versionText.toString());
        assertNotNull(parseResult);
        assertEquals("Case 2", true, parseResult.isNewVersionIsAvailable());
        assertEquals("0.4.23", parseResult.getLocalVersion());
        assertEquals("0.5.1", parseResult.getVersionOnServer());
        
        // Local version is ahead of the "remote" version.
        versionText = new StringBuffer();
        versionText.append("0.4.22");       
        parseResult = alertManager.parseAndCheckVersionText(versionText.toString());
        assertNotNull(parseResult);
        assertEquals("Case 3", false, parseResult.isNewVersionIsAvailable());
        assertEquals("0.4.23", parseResult.getLocalVersion());
        assertEquals("0.4.22", parseResult.getVersionOnServer());
    }

    @Test
    public void testAlertManagerMessage() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new TestLocaliser();
        MultiBitModel model = new MultiBitModel(controller);
        
        controller.setLocaliser(localiser);
        controller.setModel(model);   
        
        // Initialise and check
        AlertManager alertManager = AlertManager.INSTANCE;
        assertNotNull(alertManager);
        
        alertManager.initialise(controller, null);
        
        // Set up a test "remote version" string buffer.
        StringBuffer versionText;
        
        // Set version text to be a version number and a message.
        
        // Local version is the same as the "remote" version
        versionText = new StringBuffer();
        versionText.append("0.4.23\n");   
        versionText.append("message first Alice\n");
        versionText.append("message second Bob\n");
        versionText.append("message third Carol\n");
        
        ParseResult parseResult = alertManager.parseAndCheckVersionText(versionText.toString());
        assertNotNull(parseResult);
        assertEquals(false, parseResult.isNewVersionIsAvailable());
        List<String> serverMessages = parseResult.getMessages();
        assertNotNull(serverMessages);
        assertEquals("Wrong number of messages", 3, serverMessages.size());
        assertEquals("first Alice", serverMessages.get(0));
        assertEquals("second Bob", serverMessages.get(1));
        assertEquals("third Carol", serverMessages.get(2));
    }
    
    @Test
    public void testAlertManagerSignature() throws Exception {
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new TestLocaliser();
        MultiBitModel model = new MultiBitModel(controller);
        
        controller.setLocaliser(localiser);
        controller.setModel(model);   
        
        // Initialise and check
        AlertManager alertManager = AlertManager.INSTANCE;
        assertNotNull(alertManager);
        
        alertManager.initialise(controller, null);
        
        // Set up a test "remote version" string buffer.
        StringBuffer versionText;
        
        // Set version text to be a version number and a message with a signature.
        // Local version is the same as the "remote" version
        ECKey signature1Key = new ECKey();
        String publicKey1AsHex = Utils.bytesToHexString(signature1Key.getPubKey());
        
        ECKey signature2Key = new ECKey();
        String publicKey2AsHex = Utils.bytesToHexString(signature2Key.getPubKey());
        
        versionText = new StringBuffer();
        versionText.append("0.4.23\n");   
        versionText.append("message first Alice\n");
        versionText.append("message second Bob\n");
        versionText.append("message third Carol\n");

        String textToSign = versionText.toString();
        String signature1 = signature1Key.signMessage(textToSign);
        versionText.append("signature " + publicKey1AsHex + " " + signature1 + "\n");
        
        String signature2 = signature2Key.signMessage(textToSign);
        versionText.append("signature " + publicKey2AsHex + " " + signature2 + "\n");
        
        System.out.println("------\n" + versionText.toString() + "\n------");
        
        ParseResult parseResult = alertManager.parseAndCheckVersionText(versionText.toString());
        assertNotNull(parseResult);
        assertEquals(false, parseResult.isNewVersionIsAvailable());
        List<String> serverMessages = parseResult.getMessages();
        assertNotNull(serverMessages);
        assertEquals("Wrong number of messages", 3, serverMessages.size());
        assertEquals("first Alice", serverMessages.get(0));
        assertEquals("second Bob", serverMessages.get(1));
        assertEquals("third Carol", serverMessages.get(2));
        
        List<Signature> parseResultSignatures = parseResult.getSignatures();
        assertNotNull("No signatures", parseResultSignatures);
        assertEquals("Wrong number of signatures", 2, parseResultSignatures.size());
        Signature parseResultSignature1 = parseResultSignatures.get(0);
        assertEquals(publicKey1AsHex, parseResultSignature1.getPublicKeyAsHex());
        assertEquals(signature1, parseResultSignature1.getSignatureText());
        ECKey verificationKey1 = new ECKey(null, Utils.parseAsHexOrBase58(publicKey1AsHex));
        try {
            verificationKey1.verifyMessage(textToSign, signature1);
            // Verified ok.
        } catch (SignatureException se) {
            fail("The signature 1 was not verified but should have been.");
        }
        
        Signature parseResultSignature2 = parseResultSignatures.get(1);
        assertEquals(publicKey2AsHex, parseResultSignature2.getPublicKeyAsHex());
        assertEquals(signature2, parseResultSignature2.getSignatureText());
        ECKey verificationKey2 = new ECKey(null, Utils.parseAsHexOrBase58(publicKey2AsHex));
        try {
            verificationKey2.verifyMessage(textToSign, signature2);
            // Verified ok.
        } catch (SignatureException se) {
            fail("The signature 2 was not verified but should have been.");
        }
        
        // See if the first verification key verifies the second signature - this should fail
        try {
            verificationKey1.verifyMessage(textToSign, signature2);
            fail("The first public key verified the second signature incorrectly");
        } catch (SignatureException se) {
            // Expected behaviour.
        }
        
        // See if the second verification key verifies the first signature - this should fail
        try {
            verificationKey2.verifyMessage(textToSign, signature1);
            fail("The second public key verified the first signature incorrectly");
        } catch (SignatureException se) {
            // Expected behaviour.
        }
    }

    class TestLocaliser extends Localiser {
        public String getVersionNumber() {
            return "0.4.23";
        }
    }
}
