package org.multibit.viewsystem.swing.action;

import java.io.File;
import java.security.SecureRandom;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.protobuf.ByteString;

/**
 * Class containing utility methods for action tests.
 * @author jim
 *
 */
public class ActionTestUtils {
    
     private static SecureRandom secureRandom;

     public static final String LABEL_OF_ADDRESS_ADDED = "This is an address label";

     public static void createNewActiveWallet(BitcoinController controller, String descriptor, boolean encrypt, CharSequence walletPassword) throws Exception {
         if (secureRandom == null) {
             secureRandom = new SecureRandom();
         }
         
         byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
         secureRandom.nextBytes(salt);
         Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
         ScryptParameters scryptParameters = scryptParametersBuilder.build();
         KeyCrypter keyCrypter = new KeyCrypterScrypt(scryptParameters);

         Wallet wallet;
         ECKey ecKey;
         if (encrypt) {
             wallet = new Wallet(NetworkParameters.prodNet(), keyCrypter);
             ecKey = (new ECKey()).encrypt(keyCrypter, keyCrypter.deriveKey(walletPassword));
             wallet.addKey(ecKey);
         } else {
             wallet = new Wallet(NetworkParameters.prodNet());
             ecKey = new ECKey();
             wallet.addKey(ecKey);             
         }
         
         WalletData perWalletModelData = new WalletData();
         perWalletModelData.setWallet(wallet);
  
         // Save the wallet to a temporary directory.
         File multiBitDirectory = FileHandler.createTempDirectory("CreateAndDeleteWalletsTest");
         String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();
         String walletFile = multiBitDirectoryPath + File.separator + descriptor + ".wallet";
         
         // Put the wallet in the model as the active wallet.
         WalletInfoData walletInfoData = new WalletInfoData(walletFile, wallet, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
         walletInfoData.addReceivingAddress(new WalletAddressBookData(LABEL_OF_ADDRESS_ADDED, ecKey.toAddress(NetworkParameters.prodNet()).toString()), false);

         perWalletModelData.setWalletInfo(walletInfoData);
         perWalletModelData.setWalletFilename(walletFile);
         perWalletModelData.setWalletDescription(descriptor);
         
         // Save the wallet and load it up again, making it the active wallet.
         // This also sets the timestamp fields used in file change detection.
         FileHandler fileHandler = new FileHandler(controller);
         fileHandler.savePerWalletModelData(perWalletModelData, true);
         WalletData loadedPerWalletModelData = fileHandler.loadFromFile(new File(walletFile));
         
         controller.getModel().setActiveWalletByFilename(loadedPerWalletModelData.getWalletFilename());         
     }
}
