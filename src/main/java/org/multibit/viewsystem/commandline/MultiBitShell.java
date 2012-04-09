/*
 * Copyright 2012 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * This class is a copy of WalletTool, by Mike Hearn, modified for the 
 * MultiBit object model
 * 
 */

package org.multibit.viewsystem.commandline;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogManager;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;
import joptsimple.util.DateConverter;

import org.bouncycastle.util.encoders.Hex;
import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.action.CreateWalletSubmitAction;
import org.multibit.viewsystem.swing.action.DeleteWalletSubmitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.AbstractWalletEventListener;
import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.DownloadListener;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerAddress;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.discovery.DnsDiscovery;
import com.google.bitcoin.store.BlockStoreException;
import com.google.bitcoin.store.BoundedOverheadBlockStore;
import com.google.bitcoin.utils.BriefLogFormatter;

/**
 * A command line tool for manipulating wallets and working with Bitcoin.
 * <p>
 */
public class MultiBitShell {
    private static final Logger log = LoggerFactory.getLogger(MultiBitShell.class);

    public static final String ACTION_PREFIX = "--action=";

    private MultiBitController controller;

    private static final String HELP_TEXT = "\nMultiBitShell: print and manipulate the MultiBit object model\n\n"
            +

            "Usage (Implemented):\n"
            +
            // ">>> GENERAL OPTIONS\n" +
            // "  --debuglog           Enables logging from the core library.\n"
            // +
            // "  --wallet=<file>      Specifies what wallet file to load and save.\n"
            // +
            // "  --chain=<file>       Specifies the name of the file that stores the block chain.\n"
            // +
            // "  --force              Overrides any safety checks on the requested action.\n"
            // +
            // "  --date               Provide a date in form YYYY/MM/DD to any action that requires one.\n"
            // +
            // "  --peer=1.2.3.4       Use the given IP address for connections instead of peer discovery.\n"
            // +

            "  help                                            Show this help text.\n"
            + "  exit                                            Exit this MultiBitShell session.\n"
            + "  create  -wallet -filename=<>                    Makes a new wallet in the file specified.\n"
            + "  delete  -wallet -filename=<>                    Delete the wallet with the file specified.\n"
            + "  show    -wallet                                 Show the details of the last picked (active) wallet.\n"
            + "  list    -wallet                                 Lists all open wallets in a format suitable for picking.\n"
            + "  pick     $<item number> or @<search term>       Pick from the last list either by item number or match by search term (for wallets)\n\n"

            + "\n(Not Implemented Yet):\n"
            + "  create  -sendingAddress -address=<> -label=<>   Makes a new sending address with the given address and label.\n"
            + "          -receivingAddress -label=<>             Makes a new receiving address with the given label.\n\n"
            + "  edit    -preferences                            Edit the MultiBit preferences.\n"
            + "          -sendingAddress -address=<> -label=<>   Edit the sending address matching the given address or label.\n"
            + "          -receivingAddress -address=<> -label=<> Edit the receiving address matching the given address or label.\n\n"
            + "  list    -sendingAddress                         Lists all sending addresses in a format suitable for picking.\n"
            + "          -receivingAddress                       Lists all receiving addresses in a format suitable for picking.\n"
            + "          -transaction                            Lists all transactions for the active wallet for picking.\n\n"
            + "  delete  -sendingAddress -address=<> -label=<>   Delete the sending address with the given address or label.\n"
            + "          -receivingAddress -address -label=<>    Delete the receiving address with the given address or label.\n\n"
            + "  send    -address=<> -amount=<>                  Send the specified amount of BTC to the address specified.\n\n"
            + "  reset   -date=<>                                Remove all transactions on or after the date, or all if no date specified.\n"
            + "  replay  -date=<>                                Replay the blockchain from the date specified.\n\n"
            + "  import  -filename=<> -password=<>               Import private keys from the filename specified, using the password.\n"
            + "  export  -filename=<> -password=<>               Export private keys from the filename specified, using the password.\n\n"
            + "  waitFor -online or -replayComplete              Wait for MultiBit to come online or for the blockchain replay to complete.\n\n"
            + "  show    -online                                 Show whether MultiBit is onine or not.\n"
            + "          -version                                Show the MultiBit version number.\n"
            + "          -ticker                                 Show the most recent ticker details.\n"
            + "          -message max=<number>                   Show the most recent <number> of messages.\n"
            + "          -transaction                            Show the details of the last picked transaction.\n";

    public MultiBitShell(MultiBitController controller) {
        this.controller = controller;
    }

    private static OptionSpec<String> filename;
    private static OptionSpec<ActionEnum> actionFlag;
    private static OptionSpec<Date> dateFlag;
    private static NetworkParameters params;
    private static File walletFile;
    private static OptionSet options;

    private static final boolean verbose = false;

    public enum ActionEnum {
        HELP, SHOW, CREATE, DELETE, LIST, PICK, ADD_KEY, DELETE_KEY, SYNC, RESET, EXIT
    };

    public void processLine(String inputLine) throws IOException {
        if (inputLine == null || "".equals(inputLine)) {
            return;
        }

        String[] args = inputLine.split(" "); // split on spaces
        // if nothing is passed, show the help
        if (args == null || args.length == 0 || "".equals(args[0])) {
            args = new String[] { "HELP" };
        }

        // Everything gets 'optionised' as an action option
        // this is just for the OptionParser.
        // args[0] is also uppercased so it matches the ActionEnum values.
        args[0] = MultiBitShell.ACTION_PREFIX + args[0].toUpperCase();

        if (verbose) {
            System.out.print("MultiBitShell#processLine args = ");
            if (args != null) {
                for (int i = 0; i < args.length; i++) {
                    System.out.print(args[i] + " ");
                }
            }
            System.out.println("\n");
        }

        OptionParser parser = new OptionParser();
        parser.accepts("force");
        parser.accepts("debuglog");
        parser.accepts("wallet").withOptionalArg();

        filename = parser.accepts("filename").withRequiredArg().defaultsTo("multibit.wallet");
        actionFlag = parser.accepts("action").withRequiredArg().ofType(ActionEnum.class).defaultsTo(ActionEnum.SHOW);
        dateFlag = parser.accepts("date").withRequiredArg().ofType(Date.class)
                .withValuesConvertedBy(DateConverter.datePattern("yyyy/MM/dd"));

        options = parser.parse(args);

        if (options.has("debuglog")) {
            BriefLogFormatter.init();
            log.info("Starting up ...");
        } else {
            // Disable logspam unless there is a flag.
            LogManager.getLogManager().getLogger("").setLevel(Level.SEVERE);
        }

        ActionEnum action = actionFlag.value(options);

        // What should we do?
        switch (action) {
        case HELP:
            System.out.println(HELP_TEXT);
            return;
        case EXIT:
            exit();
            break;
        case DELETE:
            delete();
            break;
        case CREATE:
            create();
            break;
        case SHOW:
            show();
            break;
        case LIST:
            list();
            break;
        case PICK:
            pick();
            break;
        }
    }

    private void exit() {
        System.out.println("MultiBitShell has stopped.");
        System.exit(0);
    }

    private void create() {
        if (options.has("wallet")) {
            CreateWalletSubmitAction createNewWalletAction = new CreateWalletSubmitAction(controller, null, null);
            createNewWalletAction.createNewWallet(filename.value(options));
        } else {
            cannotHandle("create");
        }
    }

    private void delete() throws IOException {
        if (options.has("wallet")) {
            DeleteWalletSubmitAction deleteWalletSubmitAction = new DeleteWalletSubmitAction(controller, null, null, null);
            deleteWalletSubmitAction.deleteWallet(filename.value(options));
            System.out
                    .println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX + " wallet '" + filename.value(options) + "' deleted.");
        } else {
            cannotHandle("delete");
        }
    }

    private void list() {
        if (options.has("wallet")) {
            int item = 1;

            int maxDescriptionWidth = 0;
            for (PerWalletModelData loopData : controller.getModel().getPerWalletModelDataList()) {
                maxDescriptionWidth = Math.max(maxDescriptionWidth, loopData.getWalletDescription() == null ? 0 : loopData
                        .getWalletDescription().length());
            }
            System.out.format("%-5s | %-" + maxDescriptionWidth + "s | %-20s\n", "$Item", "@Description", "Filename");
            System.out.format("%-5s | %-" + maxDescriptionWidth + "s | %-20s\n", "-----", "------------", "--------");

            // System.out.println("---------------------------------------");
            for (PerWalletModelData loopData : controller.getModel().getPerWalletModelDataList()) {
//                boolean isActive = false;
//                if (loopData.getWalletFilename().equals(controller.getModel().getActiveWalletFilename())) {
//                    isActive = true;
//                    System.out.println("PICKED");
//                }
                String itemText;
                if (loopData.getWalletFilename().equals(controller.getModel().getActiveWalletFilename())) {
                    itemText = "<" + item + ">";
                } else {
                    itemText = "" + item;
                }
                System.out.format("%-5s | %-" + maxDescriptionWidth + "s | %-20s\n", itemText, loopData.getWalletDescription(),
                        loopData.getWalletFilename());
//                if (isActive) {
//                    System.out.println(" ");
//                }
                item++;
            }
        } else {
            cannotHandle("list");
        }
    }

    private void pick() {
        List<String> nonOptionArguments = options.nonOptionArguments();

        if (nonOptionArguments != null && nonOptionArguments.size() == 1) {
            // one argument exactly
            String pickChoice = nonOptionArguments.get(0);
            PerWalletModelData found = null;

            if (pickChoice.startsWith("$")) {
                // item number pick
                int itemLoop = 1;
                for (PerWalletModelData loopData : controller.getModel().getPerWalletModelDataList()) {
                    if (("$" + itemLoop).equals(pickChoice)) {
                        found = loopData;
                        break;
                    }
                    itemLoop++;
                }
            } else if (pickChoice.startsWith("@")) {
                // search pick
                String searchTerm = pickChoice.substring(1);
                for (PerWalletModelData loopData : controller.getModel().getPerWalletModelDataList()) {
                    if (("" + loopData.getWalletDescription()).indexOf(searchTerm) > -1) {
                        found = loopData;
                        break;
                    }
                }
            } else {
                System.err.println("A 'pick' argument must start with $ (for pick-by-item) or @ (for pick-by-search");
            }

            if (found == null) {
                System.err.println("Could not find wallet matching pick choice of '" + pickChoice + "'");
            } else {
                controller.getModel().setActiveWalletByFilename(found.getWalletFilename());
                System.out.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX + " Picked wallet '"
                        + found.getWalletDescription() + "'");
            }
        } else {
            System.err.println("Missing or wrong number of arguments for 'pick'");
        }
    }

    private void show() {
        if (options.has("wallet")) {
            PerWalletModelData activePerWalletModelData = controller.getModel().getActivePerWalletModelData();
            System.out.println("\nDescription : " + activePerWalletModelData.getWalletDescription());
            System.out.println("Filename    : " + activePerWalletModelData.getWalletFilename());
            System.out.println("Contents    :\n" + activePerWalletModelData.getWallet().toString());
        } else {
            cannotHandle("show");
        }
    }

    private void cannotHandle(String verb) {
        System.out.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX + "A " + verb + " "
                + options.nonOptionArguments().toString() + " was specified but MultiBitShell did not how to do that.");

    }

    private static void reset(Wallet wallet) {
        // Delete the transactions and save. In future, reset the chain head
        // pointer.
        wallet.clearTransactions(0);
        saveWallet(walletFile, wallet);
    }

    private static void syncChain(final Wallet wallet, File chainFileName) {
        try {
            // Will create a fresh chain if one doesn't exist or there is an
            // issue with this one.
            System.out.println("Connecting ...");
            final BoundedOverheadBlockStore store = new BoundedOverheadBlockStore(params, chainFileName);
            final BlockChain chain = new BlockChain(params, wallet, store);

            wallet.addEventListener(new AbstractWalletEventListener() {
                @Override
                public void onChange() {
                    saveWallet(walletFile, wallet);
                }
            });

            int startTransactions = wallet.getTransactions(true, true).size();

            PeerGroup peers = connect(wallet, chain);
            DownloadListener listener = new DownloadListener();
            peers.startBlockChainDownload(listener);
            try {
                listener.await();
            } catch (InterruptedException e) {
                System.err.println("Chain download interrupted, quitting ...");
                System.exit(1);
            }
            peers.stop();
            int endTransactions = wallet.getTransactions(true, true).size();
            if (endTransactions > startTransactions) {
                System.out.println("Synced " + (endTransactions - startTransactions) + " transactions.");
            }
        } catch (BlockStoreException e) {
            System.err.println("Error reading block chain file " + chainFileName + ": " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static PeerGroup connect(Wallet wallet, BlockChain chain) {
        PeerGroup peers = new PeerGroup(params, chain);
        peers.setUserAgent("WalletTool", "1.0");
        peers.addWallet(wallet);
        peers.setFastCatchupTimeSecs(wallet.getEarliestKeyCreationTime());
        if (options.has("peer")) {
            String peer = (String) options.valueOf("peer");
            try {
                peers.addAddress(new PeerAddress(InetAddress.getByName(peer), params.port));
            } catch (UnknownHostException e) {
                System.err.println("Could not understand peer domain name/IP address: " + peer + ": " + e.getMessage());
                System.exit(1);
            }
        } else {
            peers.addPeerDiscovery(new DnsDiscovery(params));
        }
        peers.start();
        return peers;
    }

    private static void saveWallet(File walletFile, Wallet wallet) {
        // Save the new state of the wallet to a temp file then rename, in case
        // anything goes wrong.
        File tmp;
        try {
            // Create tmp in same directory as wallet to ensure we create on the
            // same drive/volume.
            tmp = File.createTempFile("wallet", null, walletFile.getParentFile());
            tmp.deleteOnExit();
            wallet.saveToFile(tmp);
            tmp.renameTo(walletFile);
        } catch (IOException e) {
            System.err.println("Failed to save wallet! Old wallet should be left untouched.");
            e.printStackTrace();
            System.exit(1);
        }
    }

    private static void addKey(Wallet wallet) {
        ECKey key;
        long creationTimeSeconds = 0;
        if (options.has(dateFlag)) {
            creationTimeSeconds = dateFlag.value(options).getTime() / 1000;
        }
        if (options.has("privkey")) {
            String data = (String) options.valueOf("privkey");
            key = new ECKey(new BigInteger(1, Hex.decode(data)));
            if (options.has("pubkey")) {
                // Give the user a hint.
                System.out.println("You don't have to specify --pubkey when a private key is supplied.");
            }
            key.setCreationTimeSeconds(creationTimeSeconds);
        } else if (options.has("pubkey")) {
            byte[] pubkey = Hex.decode((String) options.valueOf("pubkey"));
            key = new ECKey(null, pubkey);
            key.setCreationTimeSeconds(creationTimeSeconds);
        } else {
            // Freshly generated key.
            key = new ECKey();
        }
        if (wallet.findKeyFromPubKey(key.getPubKey()) != null) {
            System.err.println("That key already exists in this wallet.");
            return;
        }
        wallet.addKey(key);
        System.out.println("addr:" + key.toAddress(params) + " " + key);
    }

    private static void deleteKey(Wallet wallet) {
        String pubkey = (String) options.valueOf("pubkey");
        String addr = (String) options.valueOf("addr");
        if (pubkey == null && addr == null) {
            System.err.println("One of --pubkey or --addr must be specified.");
            return;
        }
        ECKey key = null;
        if (pubkey != null) {
            key = wallet.findKeyFromPubKey(Hex.decode(pubkey));
        } else if (addr != null) {
            try {
                Address address = new Address(wallet.getParams(), addr);
                key = wallet.findKeyFromPubHash(address.getHash160());
            } catch (AddressFormatException e) {
                System.err.println(addr + " does not parse as a Bitcoin address of the right network parameters.");
                return;
            }
        }
        if (key == null) {
            System.err.println("Wallet does not seem to contain that key.");
            return;
        }
        wallet.keychain.remove(key);
    }
}
