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

import com.google.bitcoin.core.Base58;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.spongycastle.crypto.BufferedBlockCipher;
import org.spongycastle.crypto.CipherParameters;
import org.spongycastle.crypto.PBEParametersGenerator;
import org.spongycastle.crypto.engines.AESEngine;
import org.spongycastle.crypto.generators.PKCS5S2ParametersGenerator;
import org.spongycastle.crypto.modes.CBCBlockCipher;
import org.spongycastle.crypto.paddings.BlockCipherPadding;
import org.spongycastle.crypto.paddings.ISO10126d2Padding;
import org.spongycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.params.ParametersWithIV;
import org.spongycastle.util.encoders.Base64;
import org.spongycastle.util.encoders.Hex;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.*;

@SuppressWarnings("unchecked")
public class MyWallet {
	private static final int AESBlockSize = 4;
    public static final int DefaultPBKDF2Iterations = 10;
    public static final int MaxPBKDF2Iterations = 20000;
    public static final int MinPBKDF2Iterations = 0;
    public static final double SupportedEncryptionVersion = 2.0;

	public Map<String, Object> root;
    public  Map<String, Object> rootContainer;

	public String temporyPassword;
	public String temporySecondPassword;

	public static final NetworkParameters params = NetworkParameters.prodNet();

    public MyWallet(String base64Payload, String password) throws Exception {
        if (base64Payload == null || base64Payload.length() == 0 || password == null || password.length() == 0)
            throw new Exception("Error Decrypting Wallet");

        String decrypted = decryptWallet(base64Payload, password);

        if (decrypted == null || decrypted.length() == 0)
            throw new Exception("Error Decrypting Wallet");

        ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally

        TypeReference<HashMap<String,Object>> typeRef = new TypeReference<HashMap<String,Object>>() {};
        try {
            this.root = mapper.readValue(decrypted, typeRef);
        } catch (Exception e) {
            throw e;
        }

        if (root == null)
            throw new Exception("Error Decrypting Wallet");

        temporyPassword = password;
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

	public boolean isDoubleEncrypted() {
		Object double_encryption = root.get("double_encryption");
		if (double_encryption != null)
			return (Boolean)double_encryption;
		else
			return false;
	}

	public String getSharedKey() {
		return (String)root.get("sharedKey");
	}

	public String getDPasswordHash() {
		return (String)root.get("dpasswordhash");
	}

	public void setTemporySecondPassword(String secondPassword) {
		this.temporySecondPassword = secondPassword;
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

	public static Map<String, Object> parsePlainPayload(String payload) throws Exception {       
		ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally

		TypeReference<HashMap<String,Object>> typeRef = new TypeReference<HashMap<String,Object>>() {}; 
		try {
			return mapper.readValue(payload, typeRef);
		} catch (Exception e) {
			throw e;
		}
	}

    @SuppressWarnings("unchecked")
    public Map<String, Object> getOptions() {
        Map<String, Object> options = (Map<String, Object>) root.get("options");

        if (options == null) {
            options = new HashMap<String, Object>();

            root.put("options", options);
        }

        return options;
    }

    public int getDoubleEncryptionPbkdf2Iterations() {
        Map<String, Object> options = getOptions();

        int iterations = DefaultPBKDF2Iterations;
        if (options.containsKey("pbkdf2_iterations")) {
            iterations = Integer.valueOf(options.get("pbkdf2_iterations").toString());
        }

        return iterations;
    }

    public boolean validateSecondPassword(String secondPassword) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");

            {
                // N Rounds of SHA256
                byte[] data = md.digest((getSharedKey() + secondPassword).getBytes("UTF-8"));

                for (int ii = 1; ii < this.getDoubleEncryptionPbkdf2Iterations(); ++ii) {
                    data = md.digest(data);
                }

                String dpasswordhash = new String(Hex.encode(data));
                if (dpasswordhash.equals(getDPasswordHash()))
                    return true;
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return false;
    }

    private String decryptWallet(String ciphertext, String password) throws Exception {
        ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally

        try {
            TypeReference<HashMap<String,Object>> typeRef = new TypeReference<HashMap<String,Object>>() {};

            Map<String, Object> obj = mapper.readValue(ciphertext, typeRef);

            String payload = (String) obj.get("payload");
            int pbkdf2_iterations = Integer.valueOf(obj.get("pbkdf2_iterations").toString());

            if (pbkdf2_iterations < MinPBKDF2Iterations || pbkdf2_iterations > MaxPBKDF2Iterations)
                throw new Exception("PBKDF2 Iterations not in supported range");

            double version = Integer.valueOf(obj.get("version").toString());

            if (version != SupportedEncryptionVersion)
                throw new Exception("Wallet version " + version + " not supported");

            String result = decrypt(payload, password, pbkdf2_iterations);

            rootContainer = obj;

            return result;
        } catch (Exception e) {
            return decrypt(ciphertext, password, DefaultPBKDF2Iterations);
        }
    }

    private static byte[] copyOfRange(byte[] source, int from, int to) {
        byte[] range = new byte[to - from];
        System.arraycopy(source, from, range, 0, range.length);

        return range;
    }

    // AES 256 PBKDF2 CBC iso10126 decryption
    // 16 byte IV must be prepended to ciphertext - Compatible with crypto-js
    public static String decrypt(String ciphertext, String password, final int PBKDF2Iterations) throws Exception {
        byte[] cipherdata = Base64.decode(ciphertext);

        //Seperate the IV and cipher data
        byte[] iv = copyOfRange(cipherdata, 0, AESBlockSize * 4);
        byte[] input = copyOfRange(cipherdata, AESBlockSize * 4, cipherdata.length);

        PBEParametersGenerator generator = new PKCS5S2ParametersGenerator();
        generator.init(PBEParametersGenerator.PKCS5PasswordToUTF8Bytes(password.toCharArray()), iv, PBKDF2Iterations);
        KeyParameter keyParam = (KeyParameter) generator.generateDerivedParameters(256);

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

    // Decrypt a double encrypted private key
    public static String decryptPK(String key, String sharedKey, String password, final int PBKDF2Iterations)
            throws Exception {
        return decrypt(key, sharedKey + password, PBKDF2Iterations);
    }

    public static ECKey decodeBase58PK(String base58Priv) throws Exception {
        byte[] privBytes = Base58.decode(base58Priv);

        // Prepend a zero byte to make the biginteger unsigned
        byte[] appendZeroByte = concat(new byte[1], privBytes);

        ECKey ecKey = new ECKey(new BigInteger(appendZeroByte));

        return ecKey;
    }

    public String decryptPK(String base58Priv) throws Exception {
        if (this.isDoubleEncrypted()) {

            if (this.temporySecondPassword == null || !this.validateSecondPassword(temporySecondPassword))
                throw new Exception("You must provide a second password");

            base58Priv = decryptPK(base58Priv, getSharedKey(), this.temporySecondPassword, this.getDoubleEncryptionPbkdf2Iterations());
        }

        return base58Priv;
    }

    public ECKey decodePK(String base58Priv) throws Exception {
        return decodeBase58PK(decryptPK(base58Priv));
    }
}