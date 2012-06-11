/*
 * Copyright 2011-2012 the original author or authors.
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
 */

package com.piuk.blockchain;

import java.io.IOException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.spec.KeySpec;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import org.spongycastle.crypto.BufferedBlockCipher;
import org.spongycastle.crypto.CipherParameters;
import org.spongycastle.crypto.engines.AESEngine;
import org.spongycastle.crypto.modes.CBCBlockCipher;
import org.spongycastle.crypto.paddings.BlockCipherPadding;
import org.spongycastle.crypto.paddings.ISO10126d2Padding;
import org.spongycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.params.ParametersWithIV;
import org.spongycastle.util.encoders.Base64;
import org.spongycastle.util.encoders.Hex;
import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;

import com.google.bitcoin.core.Base58;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet; 

@SuppressWarnings("unchecked")
public class MyWallet {
	private static final int AESBlockSize = 4;
	private static final int PBKDF2Iterations = 10;
	public Map<String, Object> root;
	public String temporyPassword;
	public String temporySecondPassword;

	public static final NetworkParameters params = NetworkParameters.prodNet();

	public MyWallet(String base64Payload, String password) throws Exception {
		this.root = decryptPayload(base64Payload, password);

		if (root == null)
			throw new Exception("Error Decrypting Wallet");
	}

	public MyWallet(String base64Payload) throws Exception {
		this.root = parsePlainPayload(base64Payload);

		if (root == null)
			throw new Exception("Error Decrypting Wallet");
	}

	public static byte[] concat(byte[] first, byte[] second) {
		byte[] result = Arrays.copyOf(first, first.length + second.length);
		System.arraycopy(second, 0, result, first.length, second.length);
		return result;
	}

	public List<Map<String, Object>> getKeysMap() {
		return (List<Map<String, Object>>) root.get("keys");
	}

	public String[] getActiveAddresses() {
		List<String> list = new ArrayList<String>();
		for (Map<String, Object> map : getKeysMap()) {
			if (map.get("tag") == null || (Long)map.get("tag") == 0)
				list.add((String) map.get("addr"));
		} 
		return list.toArray(new String[list.size()]);
	}

	public String[] getArchivedAddresses() {
		List<String> list = new ArrayList<String>();
		for (Map<String, Object> map : getKeysMap()) {
			if (map.get("tag") != null && (Long)map.get("tag") == 2)
				list.add((String) map.get("addr"));
		}
		return list.toArray(new String[list.size()]);
	}

	public List<Map<String, Object>> getAddressBookMap() {
		return (List<Map<String, Object>>) root.get("address_book");
	}


	public boolean isDoubleEncrypted() {
		Object double_encryption = root.get("double_encryption");
		if (double_encryption != null)
			return (Boolean)double_encryption;
		else
			return false;
	}

	public String getGUID() {
		return (String)root.get("guid");
	}

	public String getSharedKey() {
		return (String)root.get("sharedKey");
	}

	public String getDPasswordHash() {
		return (String)root.get("dpasswordhash");
	}

	public void setTemporyPassword(String password) {
		this.temporyPassword = password;
	}

	public void setTemporySecondPassword(String secondPassword) {
		this.temporySecondPassword = secondPassword;
	}

	public String toJSONString() {
		ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally

		try {
			return mapper.writer().writeValueAsString(root);
		} catch (JsonGenerationException e) {
			e.printStackTrace();
		} catch (JsonMappingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return null;
	}
	public ECKey decodePK(String base58Priv) throws Exception {
		if (this.isDoubleEncrypted()) {

			if (this.temporySecondPassword == null)
				throw new Exception("You must provide a second password");

			base58Priv = decryptPK(base58Priv, getSharedKey(), this.temporySecondPassword);
		} 

		byte[] privBytes = Base58.decode(base58Priv);

		//Prppend a zero byte to make the biginteger unsigned
		byte[] appendZeroByte = concat(new byte[1], privBytes);

		ECKey ecKey = new ECKey(new BigInteger(appendZeroByte));

		return ecKey;
	}

	public Map<String, String> getLabelMap() {
		Map<String, String> _labelMap = new HashMap<String, String>();

		List<Map<String, Object>> addressBook = this.getAddressBookMap();

		if (addressBook != null) {
			for (Map<String, Object> addr_book : addressBook) {
				_labelMap.put((String)addr_book.get("addr"), (String)addr_book.get("label"));
			}
		}

		if (this.getKeysMap() != null) {
			for (Map<String, Object> key_map : this.getKeysMap()) {
				String label = (String)key_map.get("label");

				if (label != null)
					_labelMap.put((String)key_map.get("addr"), label);
			}
		}

		return _labelMap;
	}

	public Map<String, Object> findAddressBookEntry(String address) {
		List<Map<String, Object>> addressBook = this.getAddressBookMap();

		if (addressBook != null) {
			for (Map<String, Object> addr_book : addressBook) {
				if (addr_book.get("addr").equals(address))
					return addr_book;
			}
		}

		return null;
	}
	public Map<String, Object> findKey(String address) {
		for (Map<String, Object> key : this.getKeysMap()) {
			String addr = (String) key.get("addr");

			if (addr.equals(address))
				return key;
		}
		return null;
	}

	public boolean isMine(String address) {
		for (Map<String, Object> key : this.getKeysMap()) {
			String addr = (String) key.get("addr");

			if (addr.equals(address))
				return true;
		}

		return false;
	}

	public void setTag(String address, long tag) {
		if (this.isMine(address)) {
			findKey(address).put("tag", tag);
		}
	}

	public void addLabel(String address, String label) {
		if (this.isMine(address)) {
			findKey(address).put("label", label);
		} else {
			Map<String, Object> entry = findAddressBookEntry(address);
			if (entry != null) {
				entry.put("label", label);
			} else {
				List<Map<String, Object>> addressBook = this.getAddressBookMap();

				if (addressBook == null) {
					addressBook = new ArrayList<Map<String, Object>>();
					root.put("address_book", addressBook);
				}

				HashMap<String, Object> map = new HashMap<String, Object>();
				map.put("addr", address);
				map.put("label", label);

				addressBook.add(map);
			}
		}

	}

	protected void addKeysTobitoinJWallet(Wallet wallet) throws Exception {
		for (Map<String, Object> key : this.getKeysMap()) {
			String base58Priv = (String) key.get("priv");

			if (base58Priv == null) { //Watch only
				continue;
			}

			wallet.addKey(decodePK(base58Priv));
		}
	}

	public Wallet getBitcoinJWallet() throws Exception {
		//Construct a BitcoinJ wallet containing all our private keys
		Wallet keywallet = new Wallet(params);

		addKeysTobitoinJWallet(keywallet);

		return keywallet;
	}


	public boolean validateSecondPassword(String secondPassword) {
		try {
			MessageDigest md = MessageDigest.getInstance("SHA-256");

			//N Rounds of SHA256
			byte[] data = md.digest((getSharedKey() + secondPassword).getBytes("UTF-8"));
			for (int ii = 1; ii < PBKDF2Iterations; ++ii) {
				data = md.digest(data);
			}

			String dpasswordhash  = new String(Hex.encode(data));
			if (dpasswordhash.equals(getDPasswordHash()))
				return true;
		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}
	public static String decrypt(String ciphertext, String password) throws Exception {
    	byte[] cipherdata = Base64.decode(ciphertext);

		//Sperate the IV and cipher data
		byte[] iv = Arrays.copyOfRange(cipherdata, 0, AESBlockSize * 4);
		byte[] input = Arrays.copyOfRange(cipherdata, AESBlockSize * 4, cipherdata.length);
 
		SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
		KeySpec spec = new PBEKeySpec(password.toCharArray(), iv, PBKDF2Iterations, 256);
		SecretKey tmp = factory.generateSecret(spec);
		SecretKey secret = new SecretKeySpec(tmp.getEncoded(), "AES");

	    byte[] key = secret.getEncoded();
 
	    // setup cipher parameters with key and IV
	    KeyParameter keyParam = new KeyParameter(key);
	    CipherParameters params = new ParametersWithIV(keyParam, iv);

	    // setup AES cipher in CBC mode with PKCS7 padding
	    BlockCipherPadding padding = new ISO10126d2Padding();
	    BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESEngine()), padding);
	    cipher.reset();
	    cipher.init(false, params);

	    // create a temporary buffer to decode into (it'll include padding)
	    byte[] buf = new byte[cipher.getOutputSize(input.length)];
	    int len = cipher.processBytes(input, 0, input.length, buf, 0);
	    len += cipher.doFinal(buf, len);

	    // remove padding
	    byte[] out = new byte[len];
	    System.arraycopy(buf, 0, out, 0, len);

	    // return string representation of decoded bytes
	    return new String(out, "UTF-8");
    }

	//Decrypt a double encrypted private key
	public static String decryptPK(String key, String sharedKey, String password) throws Exception {
		return decrypt(key, sharedKey + password);
	}

	public static Map<String, Object> parsePlainPayload(String payload) throws Exception {       
		ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally

		TypeReference<HashMap<String,Object>> typeRef = new TypeReference<HashMap<String,Object>>() {}; 
		try {
			return (Map<String, Object>) mapper.readValue(payload, typeRef);
		} catch (Exception e) {
			System.out.println(payload + " " + e.getLocalizedMessage());

			throw e;
		}
	}

	//Decrypt a Wallet file and parse the JSON
	public static Map<String, Object> decryptPayload(String payload, String password) throws Exception {
		if (payload == null || payload.length() == 0 || password == null || password.length() == 0)
			return null;

		String decrypted = decrypt(payload, password);

		if (decrypted == null || decrypted.length() == 0)
			return null;

		return parsePlainPayload(decrypted);
	}

}