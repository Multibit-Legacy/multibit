package org.multibit.utils;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Wallet;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.CreateControllers;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.network.MultiBitService;
import org.multibit.network.ReplayManager;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

/**
 *  <p>Utility to list Bitcoin addresses in a wallet to an text file. Used by BRIT Redeemers</p>
 */
public class ListAddresses {

  private static final Logger log = LoggerFactory.getLogger(ListAddresses.class);

  public static void main(String args[]) {
    // Open the wallet specified in the first argument
    if (args.length > 0) {
      try {
        // Set up MultiBit working environment

        // Date format is UTC with century, T time separator and Z for UTC timezone.
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

        File multiBitDirectory = createMultiBitRuntime();

        // Set the application data directory to be the one we just created.
        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);
        log.debug("applicationDataDirectoryLocator = " + applicationDataDirectoryLocator);

        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(applicationDataDirectoryLocator);

        log.debug("Creating Bitcoin service");
        // Create the MultiBitService that connects to the bitcoin network.
        MultiBitService multiBitService = new MultiBitService(controllers.bitcoinController);
        log.debug("multiBitService = " + multiBitService);

        controllers.bitcoinController.setMultiBitService(multiBitService);

        // Add the simple view system (no Swing).
        SimpleViewSystem simpleViewSystem = new SimpleViewSystem();
        controllers.coreController.registerViewSystem(simpleViewSystem);
        log.debug("simpleViewSystem = " + simpleViewSystem);

        ReplayManager.INSTANCE.initialise(controllers.bitcoinController, true);

        //
        // MultiBit runtime is now setup and running.
        //


        WalletData walletData = multiBitService.addWalletFromFilename(args[0]);

        // List out all the Bitcoin addresses
        if (walletData != null) {
          Wallet wallet = walletData.getWallet();
          System.out.println("# ----------------------------------");
          System.out.println("# ----- Bitcoin addresses for wallet '" + walletData.getWalletInfo().getWalletFilename() + "'");
          for (ECKey ecKey : wallet.getKeys()) {
            System.out.println(ecKey.toAddress(NetworkParameters.fromID(NetworkParameters.ID_MAINNET)));
          }
          System.out.println("# -----------------------------------");
        }
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }
    } else {
      System.out.println("Usage: Pass the filename you want a listing of the Bitcoin addresses for as a parameter.");
    }
  }

  /**
   * Create a working, portable runtime of MultiBit in a temporary directory.
   *
   * @return the temporary directory the multibit runtime has been created in
   */
  private static File createMultiBitRuntime() throws IOException {
    File multiBitDirectory = FileHandler.createTempDirectory("multibit");
    String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

    System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

    // Create an empty multibit.properties.
    File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multibit.properties");
    multibitProperties.createNewFile();
    multibitProperties.deleteOnExit();

    // Copy in the checkpoints stored in git - this is in source/main/resources/.
    File multibitCheckpoints = new File(multiBitDirectoryPath + File.separator + "multibit.checkpoints");
    FileHandler.copyFile(new File("./src/main/resources/multibit.checkpoints"), multibitCheckpoints);
    multibitCheckpoints.deleteOnExit();

    return multiBitDirectory;
  }
}