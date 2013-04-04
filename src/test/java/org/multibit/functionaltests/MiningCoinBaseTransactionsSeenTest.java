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

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Constants;
import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.network.MultiBitService;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.DumpedPrivateKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.Wallet.BalanceType;

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
    private static final int NUMBER_OF_BLOCKS_TO_REPLAY = 10;

    private static final BigInteger BALANCE_AT_START = BigInteger.ZERO;
    private static final BigInteger BALANCE_AFTER_REPLAY = BigInteger.valueOf(22223642);

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

            // Create the controller.
            final MultiBitController controller = new MultiBitController(applicationDataDirectoryLocator);
            MultiBit.setController(controller);

            // Create the model - gets hooked up to controller automatically.
            @SuppressWarnings("unused")
            MultiBitModel model = new MultiBitModel(controller);

            log.debug("Creating Bitcoin service");
            // Create the MultiBitService that connects to the bitcoin network.
            MultiBitService multiBitService = new MultiBitService(controller);
            controller.setMultiBitService(multiBitService);

            // Add the simple view system (no Swing).
            SimpleViewSystem simpleViewSystem = new SimpleViewSystem();
            controller.registerViewSystem(simpleViewSystem);
            
            ReplayManager.INSTANCE.initialise(controller);

            //
            // MultiBit runtime is now setup and running.
            //

            String miningWalletPath = multiBitDirectory.getAbsolutePath() + File.separator + "mining.wallet";

            // Create a new wallet.
            Wallet miningWallet = new Wallet(NetworkParameters.prodNet());

            // Add in the mining key that has the coinbase transactions.
            DumpedPrivateKey miningPrivateKey = new DumpedPrivateKey(NetworkParameters.prodNet(), MINING_PRIVATE_KEY);

            miningWallet.keychain.add(miningPrivateKey.getKey());
            PerWalletModelData perWalletModelData = new PerWalletModelData();
            perWalletModelData.setWalletInfo(new WalletInfo(miningWalletPath, MultiBitWalletVersion.PROTOBUF));
            perWalletModelData.setWallet(miningWallet);
            perWalletModelData.setWalletFilename(miningWalletPath);
            perWalletModelData.setWalletDescription("testReplayMiningTransaction test");

            // Save the new wallet.
            controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

            // Get the multibitService to load it up and hook it up to the blockchain.
            controller.getMultiBitService().addWalletFromFilename(miningWalletPath);
            controller.getModel().setActiveWalletByFilename(miningWalletPath);

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
            List<PerWalletModelData> perWalletModelDataList = new ArrayList<PerWalletModelData>();
            perWalletModelDataList.add(controller.getModel().getActivePerWalletModelData());
            ReplayTask replayTask = new ReplayTask(perWalletModelDataList, formatter.parse(START_OF_REPLAY_PERIOD), ReplayTask.UNKNOWN_START_HEIGHT);
            ReplayManager.INSTANCE.offerReplayTask(replayTask);

            // Wait for blockchain replay to download more than the required amount.
            log.debug("Waiting for blockchain replay to download more than " + NUMBER_OF_BLOCKS_TO_REPLAY + " blocks. . . ");
            while (simpleViewSystem.getNumberOfBlocksDownloaded() < NUMBER_OF_BLOCKS_TO_REPLAY) {
                Thread.sleep(1000);
                log.debug("Blocks downloaded =  " + simpleViewSystem.getNumberOfBlocksDownloaded());
            }

            // Check new balance on wallet - estimated balance should be at least the
            // expected (may have later tx too)..

            log.debug("Mining wallet estimated balance is:\n" + controller.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED).toString());
            log.debug("Mining wallet spendable balance is:\n" + controller.getModel().getActiveWallet().getBalance().toString());
            log.debug("Mining wallet is:\n" + controller.getModel().getActiveWallet().toString());
            assertTrue("Estimated balance of mining wallet is incorrect", BALANCE_AFTER_REPLAY.compareTo(controller.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED)) <= 0);
            //assertTrue("Available balance of mining wallet is incorrect", BigInteger.ZERO.compareTo(controller.getModel().getActiveWallet().getBalance()) == 0);

            // See if the first transaction is a coinbase.
            miningWallet = controller.getModel().getActiveWallet();
            
            Set<Transaction> transactions = miningWallet.getTransactions(true, true);
            assertTrue("Transactions are missing", !(transactions == null || transactions.isEmpty()));
            Transaction transaction = transactions.iterator().next();
            assertNotNull("First transaction is null", transaction);
            System.out.println("First transaction before roundtrip\n" + transaction);
            
            assertTrue("The first transaction in the wallet is not a coinbase but it should be", transaction.isCoinBase());
            
            // Force save the wallet, reload it and check the transaction is still coinbase.
            controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);
            
            PerWalletModelData rebornPerWalletModelData = controller.getFileHandler().loadFromFile(new File(miningWalletPath));
            assertNotNull("No reborn perWalletModelData", rebornPerWalletModelData);;
            assertNotNull("No reborn wallet", rebornPerWalletModelData.getWallet());

            Wallet rebornMiningWallet = rebornPerWalletModelData.getWallet();
            
            // See if the first transaction in the reborn wallet is a coinbase.
            Set<Transaction> rebornTransactions = rebornMiningWallet.getTransactions(true, true);
            assertTrue("No reborn transactions", ! (rebornTransactions == null || rebornTransactions.isEmpty()));
            Transaction rebornTransaction = rebornTransactions.iterator().next();
            assertNotNull("No reborn first transaction", rebornTransaction);
            System.out.println("First transaction after roundtrip\n" + rebornTransaction);
            
            assertTrue("The first transaction in the wallet is not a coinbase but it should be", rebornTransaction.isCoinBase());
            
            // Tidy up.
            multiBitService.getPeerGroup().stop();

            controller.getFileHandler().deleteWalletAndWalletInfo(controller.getModel().getActivePerWalletModelData());
        } else {
            log.debug("Not running functional test: MiningCoinBaseTransactionsSeenTest#testReplayMiningTransaction. Add '-DrunFunctionalTests=true' to run");
        }
    }

    /**
     * Create a working, portable runtime of MultiBit in a temporary directory
     * 
     * @return the temporary directory the multibit runtime has been created in
     */
    private File createMultiBitRuntime() throws IOException {
        File multiBitDirectory = FileHandler.createTempDirectory("multibit");
        String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

        System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

        // create an empty multibit.properties
        File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multibit.properties");
        multibitProperties.createNewFile();
        multibitProperties.deleteOnExit();

        // copy in the blockchain stored in git - this is in
        // source/main/resources/
        File multibitBlockchain = new File(multiBitDirectoryPath + File.separator + "multibit.blockchain");
        FileHandler.copyFile(new File("./src/main/resources/multibit.blockchain"), multibitBlockchain);
        multibitBlockchain.deleteOnExit();

        return multiBitDirectory;
    }
}
