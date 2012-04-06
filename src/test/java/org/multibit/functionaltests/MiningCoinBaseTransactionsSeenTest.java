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

import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.channels.FileChannel;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.AbstractPeerEventListener;
import com.google.bitcoin.core.DumpedPrivateKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Peer;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;

/**
 * functional test to check that Mining Coinbase Transactions can be seen.
 * 
 * See bug report: https://github.com/jim618/multibit/issues/21
 * 
 * @author jim
 * 
 */
public class MiningCoinBaseTransactionsSeenTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(MiningCoinBaseTransactionsSeenTest.class);

    private static final String MINING_ADDRESS = "1GqtGtn4fctXuKxsVzRPSLmYWN1YioLi9y";
    private static final String MINING_PRIVATE_KEY = "5JDxPrBRghF1EvSBjDigywqfmAjpHPmTJxYtQTYJxJRHLLQA4mG";

    private static final String START_OF_REPLAY_PERIOD = "2012-03-03T13:00:00Z";
    private static final int NUMBER_OF_BLOCKS_TO_REPLAY = 20;

    private static final BigInteger BALANCE_AT_START = BigInteger.ZERO;
    private static final BigInteger BALANCE_AFTER_REPLAY = BigInteger.valueOf(22223642);

    private SimpleDateFormat formatter;

    @Test
    public void testReplayMiningTransaction() throws Exception {
        // currently uses Swing hence check if running on server and abort
        if (GraphicsEnvironment.isHeadless()) {
            log.debug("Aborting MiningCoinBaseTransactionsSeenTest#testReplayMiningTransaction as running headless");
            return;
        }
        
        // date format is UTC with century, T time separator and Z for UTC
        // timezone
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

        File multiBitDirectory = createMultiBitRuntime();

        // set the application data directory to be the one we just created
        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);

        // create the controller
        final MultiBitController controller = new MultiBitController(applicationDataDirectoryLocator);

        // create the model - gets hooked up to controller automatically
        MultiBitModel model = new MultiBitModel(controller);

        log.debug("Creating Bitcoin service");
        // create the MultiBitService that connects to the bitcoin network
        MultiBitService multiBitService = new MultiBitService(false, controller);
        controller.setMultiBitService(multiBitService);

        // add the simple view system
        SimpleViewSystem simpleViewSystem = new SimpleViewSystem();
        controller.registerViewSystem(simpleViewSystem);

        ViewSystem swingViewSystem = new MultiBitFrame(controller, null);
        controller.registerViewSystem(swingViewSystem);

        //
        // MultiBit runtime is now setup and running
        //

        String miningWalletPath = multiBitDirectory.getAbsolutePath() + File.separator + "mining.wallet";

        // create a new wallet
        Wallet miningWallet = new Wallet(NetworkParameters.prodNet());

        // add in the mining key with the coinbase transactions
        DumpedPrivateKey miningPrivateKey = new DumpedPrivateKey(NetworkParameters.prodNet(), MINING_PRIVATE_KEY);

        miningWallet.keychain.add(miningPrivateKey.getKey());
        PerWalletModelData perWalletModelData = new PerWalletModelData();
        perWalletModelData.setWalletInfo(new WalletInfo(miningWalletPath));
        perWalletModelData.setWallet(miningWallet);
        perWalletModelData.setWalletFilename(miningWalletPath);
        perWalletModelData.setWalletDescription("testReplayMiningTransaction test");

        // save the new wallet
        controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

        // get the multibitService to load it up and hook it up to the
        // blockchain
        controller.getMultiBitService().addWalletFromFilename(miningWalletPath);
        controller.getModel().setActiveWalletByFilename(miningWalletPath);

        if (swingViewSystem instanceof MultiBitFrame) {
            ((MultiBitFrame) swingViewSystem).getWalletsView().initUI();
            ((MultiBitFrame) swingViewSystem).getWalletsView().displayView();
        }
        controller.fireDataChanged();

        log.debug("Mining wallet = \n" + miningWallet.toString());

        assertEquals(BALANCE_AT_START, miningWallet.getBalance());

        // wait for a peer connection
        log.debug("Waiting for peer connection. . . ");
        while (!simpleViewSystem.isOnline()) {
            Thread.sleep(1000);
        }
        log.debug("Now online.");
        
        log.debug("Replaying blockchain");
        multiBitService.replayBlockChain(formatter.parse(START_OF_REPLAY_PERIOD));

        // wait for blockchain replay to download more than the required amount
        log.debug("Waiting for blockchain replay to download more than " + NUMBER_OF_BLOCKS_TO_REPLAY + " blocks. . . ");
        while (simpleViewSystem.getNumberOfBlocksDownloaded() < NUMBER_OF_BLOCKS_TO_REPLAY) {
            Thread.sleep(1000);
            log.debug("Blocks downloaded =  " + simpleViewSystem.getNumberOfBlocksDownloaded());
        }

        // check new balance on wallet
        assertEquals(BALANCE_AFTER_REPLAY, controller.getModel().getActiveWallet().getBalance());

        // tidy up
        multiBitService.getPeerGroup().stop();

        controller.getFileHandler().deleteWalletAndWalletInfo(controller.getModel().getActivePerWalletModelData());

    }

    /**
     * create a working, portable runtime of MultiBit in a temporary directory
     * 
     * @return the temporary directory the multibit runtime has been created in
     */
    private File createMultiBitRuntime() throws IOException {
        File multiBitDirectory = createTempDirectory();
        String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

        System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

        // create an empty multibit.properties
        File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multibit.properties");
        multibitProperties.createNewFile();
        multibitProperties.deleteOnExit();

        // copy in the blockchain stored in git - this is in
        // source/main/resources/
        File multibitBlockchain = new File(multiBitDirectoryPath + File.separator + "multibit.blockchain");
        copyFile(new File("./src/main/resources/multibit.blockchain"), multibitBlockchain);
        multibitBlockchain.deleteOnExit();

        return multiBitDirectory;
    }

    private void copyFile(File sourceFile, File destinationFile) throws IOException {
        if (!destinationFile.exists()) {
            destinationFile.createNewFile();
        }
        FileInputStream fileInputStream = null;
        FileOutputStream fileOutputStream = null;
        FileChannel source = null;
        FileChannel destination = null;
        try {
            fileInputStream = new FileInputStream(sourceFile);
            source = fileInputStream.getChannel();
            fileOutputStream = new FileOutputStream(destinationFile);
            destination = fileOutputStream.getChannel();
            long transfered = 0;
            long bytes = source.size();
            while (transfered < bytes) {
                transfered += destination.transferFrom(source, 0, source.size());
                destination.position(transfered);
            }
        } finally {
            if (source != null) {
                source.close();
            } else if (fileInputStream != null) {
                fileInputStream.close();
            }
            if (destination != null) {
                destination.close();
            } else if (fileOutputStream != null) {
                fileOutputStream.close();
            }
        }
    }

    public static File createTempDirectory() throws IOException {
        final File temp;

        temp = File.createTempFile("multibit", Long.toString(System.currentTimeMillis()));

        if (!(temp.delete())) {
            throw new IOException("Could not delete temp file: " + temp.getAbsolutePath());
        }

        if (!(temp.mkdir())) {
            throw new IOException("Could not create temp directory: " + temp.getAbsolutePath());
        }

        temp.deleteOnExit();

        return temp;
    }

    /**
     * utility class just to show the number of peers connected in the log
     */
    class CountPeerEventListener extends AbstractPeerEventListener {
        int numberOfPeers = 0;

        public void onPeerDisconnected(Peer peer, int peerCount) {
            numberOfPeers = peerCount;
        }

        public void onPeerConnected(Peer peer, int peerCount) {
            numberOfPeers = peerCount;
        }

        public int getNumberOfPeers() {
            return numberOfPeers;
        }
    };
}
