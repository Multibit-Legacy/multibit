package org.multibit.viewsystem.swing.action;

import java.io.File;
import java.security.SecureRandom;
import java.util.Locale;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.multibit.Localiser;
import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.CoreModel;
import org.multibit.model.bitcoin.wallet.WalletData;
import org.multibit.model.bitcoin.wallet.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.ScryptParametersConstants;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.protobuf.ByteString;
import org.multibit.network.MultiBitService;


/**
 * Class containing utility methods for action tests.
 * @author jim
 *
 */
public class ActionTestUtils {
    
    private static SecureRandom secureRandom;

    public static MultiBitController createController() {
        MultiBitController controller = new MultiBitController();

        Localiser localiser = new Localiser(Locale.ENGLISH);
        CoreModel coreModel = new CoreModel();
        BitcoinModel bitcoinModel = new BitcoinModel(coreModel);

        controller.setCoreModel(coreModel);
        controller.setBitcoinModel(bitcoinModel);
        controller.setLocaliser(localiser);

        
        CurrencyConverter.INSTANCE.initialise(controller);

        MultiBit.setController(controller);
        return controller;
    }
     
     public static void createNewActiveWallet(MultiBitController controller, String descriptor, boolean encrypt, char[] walletPassword) throws Exception {
         if (secureRandom == null) {
             secureRandom = new SecureRandom();
         }
         
         byte[] salt = new byte[ScryptParametersConstants.SALT_LENGTH];
         secureRandom.nextBytes(salt);
         Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
         ScryptParameters scryptParameters = scryptParametersBuilder.build();
         KeyCrypter keyCrypter = new KeyCrypterScrypt(scryptParameters);

         Wallet wallet;
         if (encrypt) {
             wallet = new Wallet(NetworkParameters.prodNet(), keyCrypter);
             ECKey ecKey = (new ECKey()).encrypt(keyCrypter, keyCrypter.deriveKey(walletPassword));
             wallet.addKey(ecKey);
         } else {
             wallet = new Wallet(NetworkParameters.prodNet());
             ECKey ecKey = new ECKey();
             wallet.addKey(ecKey);             
         }
         
         WalletData perWalletModelData = new WalletData();
         perWalletModelData.setWallet(wallet);
  
         // Save the wallet to a temporary directory.
         File multiBitDirectory = FileHandler.createTempDirectory("CreateAndDeleteWalletsTest");
         String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();
         String walletFile = multiBitDirectoryPath + File.separator + descriptor + ".wallet";
         
         // Put the wallet in the model as the active wallet.
         perWalletModelData.setWalletInfo(new WalletInfoData(walletFile, MultiBitWalletVersion.PROTOBUF_ENCRYPTED));
         perWalletModelData.setWalletFilename(walletFile);
         perWalletModelData.setWalletDescription(descriptor);
         
         // Save the wallet and load it up again, making it the active wallet.
         // This also sets the timestamp fields used in file change detection.
         FileHandler fileHandler = new FileHandler(controller);
         fileHandler.savePerWalletModelData(perWalletModelData, true);
         WalletData loadedPerWalletModelData = fileHandler.loadFromFile(new File(walletFile));
         
         controller.getBitcoinModel().setActiveWalletByFilename(loadedPerWalletModelData.getWalletFilename());         
     }
}
