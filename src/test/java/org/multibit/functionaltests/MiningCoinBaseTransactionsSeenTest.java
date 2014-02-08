/**
 * Copyright 2012 multibit.org
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
package org.multibit.functionaltests;


import com.google.dogecoin.core.DumpedPrivateKey;
import com.google.dogecoin.core.NetworkParameters;
import com.google.dogecoin.core.Transaction;
import com.google.dogecoin.core.Wallet;
import com.google.dogecoin.core.Wallet.BalanceType;
import junit.framework.TestCase;
import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.network.MultiBitService;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.*;


/**
 * Functional test to check that Mining Coinbase Transactions can be seen.
 * 
 * See bug report: https://github.com/jim618/multibit/issues/21.
 * 
 * @author jim
 * 
 */
public class MiningCoinBaseTransactionsSeenTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(MiningCoinBaseTransactionsSeenTest.class);

    // The address for this private key is "1GqtGtn4fctXuKxsVzRPSLmYWN1YioLi9y".
    private static final String MINING_PRIVATE_KEY = "5JDxPrBRghF1EvSBjDigywqfmAjpHPmTJxYtQTYJxJRHLLQA4mG";

    private static final String START_OF_REPLAY_PERIOD = "2012-03-03T13:00:00Z";

    private static final BigInteger BALANCE_AT_START = BigInteger.ZERO;

    private SimpleDateFormat formatter;

    @Test
    public void testReplayMiningTransaction() throws Exception {
        // Get the system property runFunctionalTest to see if the functional tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            // Date format is UTC with century, T time separator and Z for UTC timezone.
            formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
            formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

            File multiBitDirectory = createMultiBitRuntime();

            // Set the application data directory to be the one we just created.
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);
            log.debug("applicationDataDirectoryLocator = " + applicationDataDirectoryLocator);
            
            // Create MultiBit controller.
            final CreateControllers.Controllers controllers = CreateControllers.createControllers(applicationDataDirectoryLocator);

            log.debug("Creating Bitcoin service");
            // Create the MultiBitService that connects to the dogecoin network.
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

            String miningWalletPath = multiBitDirectory.getAbsolutePath() + File.separator + "mining.wallet";

            // Create a new wallet.
            Wallet miningWallet = new Wallet(NetworkParameters.prodNet());

            // Add in the mining key that has the coinbase transactions.
            DumpedPrivateKey miningPrivateKey = new DumpedPrivateKey(NetworkParameters.prodNet(), MINING_PRIVATE_KEY);

            miningWallet.addKey(miningPrivateKey.getKey());
            WalletData perWalletModelData = new WalletData();
            perWalletModelData.setWalletInfo(new WalletInfoData(miningWalletPath, miningWallet, MultiBitWalletVersion.PROTOBUF));
            perWalletModelData.setWallet(miningWallet);
            perWalletModelData.setWalletFilename(miningWalletPath);
            perWalletModelData.setWalletDescription("testReplayMiningTransaction test");

            // Save the new wallet.
            controllers.bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelData, true);

            // Get the multibitService to load it up and hook it up to the blockchain.
            controllers.bitcoinController.getMultiBitService().addWalletFromFilename(miningWalletPath);
            controllers.bitcoinController.getModel().setActiveWalletByFilename(miningWalletPath);

            log.debug("Mining wallet = \n" + miningWallet.toString());

            assertEquals(BALANCE_AT_START, miningWallet.getBalance());

            // Wait for a peer connection.
            log.debug("Waiting for peer connection. . . ");
            while (!simpleViewSystem.isOnline()) {
                Thread.sleep(1000);
            }
            log.debug("Now online.");

            log.debug("Replaying blockchain");
            //multiBitService.replayBlockChain(formatter.parse(START_OF_REPLAY_PERIOD));
            List<WalletData> perWalletModelDataList = new ArrayList<WalletData>();
            perWalletModelDataList.add(controllers.bitcoinController.getModel().getActivePerWalletModelData());
            ReplayTask replayTask = new ReplayTask(perWalletModelDataList, formatter.parse(START_OF_REPLAY_PERIOD), ReplayTask.UNKNOWN_START_HEIGHT);
            ReplayManager.INSTANCE.offerReplayTask(replayTask);

            // Run for a while.
            log.debug("Twiddling thumbs for 90 seconds ...");
            Thread.sleep(90000);
            log.debug("... 90 seconds later.");

            // Check new balance on wallet - estimated balance should be at least the
            // expected (may have later tx too)..

            log.debug("Mining wallet estimated balance is:\n" + controllers.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED).toString());
            log.debug("Mining wallet spendable balance is:\n" + controllers.bitcoinController.getModel().getActiveWallet().getBalance().toString());
            log.debug("Mining wallet is:\n" + controllers.bitcoinController.getModel().getActiveWallet().toString());
            assertTrue("There were no transactions after replay", controllers.bitcoinController.getModel().getActiveWallet().getTransactions(true).size() > 0);

            // See if the first transaction is a coinbase.
            miningWallet = controllers.bitcoinController.getModel().getActiveWallet();
            
            Set<Transaction> transactions = miningWallet.getTransactions(true);
            assertTrue("Transactions are missing", !(transactions == null || transactions.isEmpty()));
            Transaction transaction = transactions.iterator().next();
            assertNotNull("First transaction is null", transaction);
            System.out.println("First transaction before roundtrip\n" + transaction);
            
            assertTrue("The first transaction in the wallet is not a coinbase but it should be", transaction.isCoinBase());
            
            // Force save the wallet, reload it and check the transaction is still coinbase.
            controllers.bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelData, true);
            
            WalletData rebornPerWalletModelData = controllers.bitcoinController.getFileHandler().loadFromFile(new File(miningWalletPath));
            assertNotNull("No reborn perWalletModelData", rebornPerWalletModelData);;
            assertNotNull("No reborn wallet", rebornPerWalletModelData.getWallet());

            Wallet rebornMiningWallet = rebornPerWalletModelData.getWallet();
            
            // See if the first transaction in the reborn wallet is a coinbase.
            Set<Transaction> rebornTransactions = rebornMiningWallet.getTransactions(true);
            assertTrue("No reborn transactions", ! (rebornTransactions == null || rebornTransactions.isEmpty()));
            Transaction rebornTransaction = rebornTransactions.iterator().next();
            assertNotNull("No reborn first transaction", rebornTransaction);
            System.out.println("First transaction after roundtrip\n" + rebornTransaction);
            
            assertTrue("The first transaction in the wallet is not a coinbase but it should be", rebornTransaction.isCoinBase());
            
            // Tidy up.
            multiBitService.getPeerGroup().stop();

            controllers.bitcoinController.getFileHandler().deleteWalletAndWalletInfo(controllers.bitcoinController.getModel().getActivePerWalletModelData());
        } else {
            log.debug("Not running functional test: MiningCoinBaseTransactionsSeenTest#testReplayMiningTransaction. Add '-DrunFunctionalTests=true' to run");
        }
    }

    /**
     * Create a working, portable runtime of MultiBit in a temporary directory.
     * 
     * @return the temporary directory the multibit runtime has been created in
     */
    private File createMultiBitRuntime() throws IOException {
        File multiBitDirectory = FileHandler.createTempDirectory("multibit");
        String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

        System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

        // Create an empty multibit.properties.
        File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multidoge.properties");
        multibitProperties.createNewFile();
        multibitProperties.deleteOnExit();

        // Copy in the checkpoints stored in git - this is in source/main/resources/.
        File multibitCheckpoints = new File(multiBitDirectoryPath + File.separator + "multidoge.checkpoints");
        FileHandler.copyFile(new File("./src/main/resources/multidoge.checkpoints"), multibitCheckpoints);
        multibitCheckpoints.deleteOnExit();

        return multiBitDirectory;
    }
}
