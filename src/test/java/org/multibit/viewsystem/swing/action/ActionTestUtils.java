package org.multibit.viewsystem.swing.action;

import java.io.File;
import java.util.Locale;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncryptableWallet;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;

/**
 * Class containing utility methods for action tests.
 * @author jim
 *
 */
public class ActionTestUtils {
    public static MultiBitController createController() {
        MultiBitController controller = new MultiBitController();
         
         Localiser localiser = new Localiser(Locale.ENGLISH);
         MultiBitModel model = new MultiBitModel(controller);
         
         controller.setLocaliser(localiser);
         controller.setModel(model);
         
         return controller;
     }
     
     public static void createNewActiveWallet(MultiBitController controller, String descriptor, boolean encrypt, char[] walletPassword) throws Exception {
         EncryptableWallet wallet = new EncryptableWallet(NetworkParameters.prodNet());
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
             
         if (encrypt) {
             ((EncryptableWallet)loadedPerWalletModelData.getWallet()).encrypt(walletPassword);
         }

         controller.getModel().addAndMakeActiveWallet(loadedPerWalletModelData);
     }
}
