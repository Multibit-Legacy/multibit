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

import java.io.IOException;
import java.io.PrintStream;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogManager;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;
import joptsimple.util.DateConverter;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.action.CreateWalletSubmitAction;
import org.multibit.viewsystem.swing.action.DeleteWalletSubmitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.utils.BriefLogFormatter;

/**
 * A command line tool for manipulating wallets and working with Bitcoin.
 * <p>
 */
public class MultiBitShell {
    private static final Logger log = LoggerFactory.getLogger(MultiBitShell.class);

    public static final String ACTION_PREFIX = "--action=";

    private MultiBitController controller;
    
    private PrintStream printStream;
    
    private static MessageFormatter formatter = new MessageFormatter();

    private static final String HELP_TEXT = formatter.formatMessage(Message.START_OF_HELP)
            + "\n\nMultiBitShell: print and manipulate the MultiBit object model\n\n"
            + "Usage (Implemented):\n"
            + "  help                                            Show this help text.\n"
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
            + "          -transaction                            Show the details of the last picked transaction.\n"
            + formatter.formatMessage(Message.END_OF_HELP);

    public MultiBitShell(MultiBitController controller, PrintStream printStream) {
        this.controller = controller;
        this.printStream = printStream;
        formatter.setPrintStream(printStream);
    }

    private static OptionSpec<String> filename;
    private static OptionSpec<ActionEnum> actionFlag;
    private static OptionSpec<Date> dateFlag;
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
        formatter.printMessage(Message.MULTIBIT_SHELL_HAS_STOPPED);
        System.exit(0);
    }

    private void create() {
        if (options.has("wallet")) {
            CreateWalletSubmitAction createNewWalletAction = new CreateWalletSubmitAction(controller, null, null);
            createNewWalletAction.createNewWallet(filename.value(options));
            formatter.printMessage(Message.WALLET_CREATED, new Object[] { filename.value(options) });
        } else {
            cannotHandle("create");
        }
    }

    private void delete() throws IOException {
        if (options.has("wallet")) {
            DeleteWalletSubmitAction deleteWalletSubmitAction = new DeleteWalletSubmitAction(controller, null, null, null);
            deleteWalletSubmitAction.deleteWallet(filename.value(options));
            formatter.printMessage(Message.WALLET_DELETED, new Object[] { filename.value(options) });
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
            formatter.printMessage(Message.START_OF_LIST_OF_WALLETS);
            printStream.format("%-5s | %-" + maxDescriptionWidth + "s | %-20s\n", "$Item", "@Description", "Filename");
            printStream.format("%-5s | %-" + maxDescriptionWidth + "s | %-20s\n", "-----", "------------", "--------");

            for (PerWalletModelData loopData : controller.getModel().getPerWalletModelDataList()) {
                String itemText;
                if (loopData.getWalletFilename().equals(controller.getModel().getActiveWalletFilename())) {
                    itemText = "<" + item + ">";
                } else {
                    itemText = "" + item;
                }
                printStream.format("%-5s | %-" + maxDescriptionWidth + "s | %-20s\n", itemText, loopData.getWalletDescription(),
                        loopData.getWalletFilename());
                item++;
            }
            formatter.printMessage(Message.END_OF_LIST_OF_WALLETS);

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
                formatter.printError(ErrorMessage.PICK_START_CHARACTER_WRONG);
            }

            if (found == null) {
                formatter.printError(ErrorMessage.COULD_NOT_FIND_MATCHING_WALLET, new Object[]{pickChoice});
            } else {
                controller.getModel().setActiveWalletByFilename(found.getWalletFilename());
                formatter.printMessage(Message.PICKED_WALLET, new Object[] { found.getWalletDescription() });
            }
        } else {
            formatter.printError(ErrorMessage.WRONG_NUMBER_OF_ARGUMENTS_FOR_PICK);
        }
    }

    private void show() {
        if (options.has("wallet")) {
            PerWalletModelData activePerWalletModelData = controller.getModel().getActivePerWalletModelData();
            printStream.println("\nDescription : " + activePerWalletModelData.getWalletDescription());
            printStream.println("Filename    : " + activePerWalletModelData.getWalletFilename());
            printStream.println("Contents    :\n" + activePerWalletModelData.getWallet().toString());
        } else {
            cannotHandle("show");
        }
    }

    private void cannotHandle(String verb) {
        formatter.printMessage(Message.CANNOT_HANDLE,
                new Object[] { verb + " " + options.nonOptionArguments().toString() });
    }
}
