package org.multibit.viewsystem.swing.action;

import java.io.File;
import java.security.SecureRandom;
import java.util.Locale;

import org.multibit.Localiser;
import org.multibit.MultiBit;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.crypto.EncrypterDecrypter;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import org.multibit.controller.core.CoreController;

/**
 * Class containing utility methods for action tests.
 * @author jim
 *
 */
public class ActionTestUtils {

     public static void createNewActiveWallet(BitcoinController controller, String descriptor) throws Exception {
         Wallet wallet = new Wallet(NetworkParameters.prodNet());
         wallet.getKeychain().add(new ECKey());
  
         PerWalletModelData perWalletModelData = new PerWalletModelData();
         perWalletModelData.setWallet(wallet);
  
         // Save the wallet to a temporary directory.
         File multiBitDirectory = FileHandler.createTempDirectory("CreateAndDeleteWalletsTest");
         String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();
         String walletFile = multiBitDirectoryPath + File.separator + descriptor + ".wallet";
         
         // Put the wallet in the model as the active wallet.
         perWalletModelData.setWalletInfo(new WalletInfo(walletFile, WalletVersion.PROTOBUF));
         perWalletModelData.setWalletFilename(walletFile);
         perWalletModelData.setWalletDescription(descriptor);
         
         // Save the wallet and load it up again, making it the active wallet.
         // This also sets the timestamp fields used in file change detection.
         FileHandler fileHandler = new FileHandler(controller);
         fileHandler.savePerWalletModelData(perWalletModelData, true);
         PerWalletModelData loadedPerWalletModelData = fileHandler.loadFromFile(new File(walletFile));
     }
}
