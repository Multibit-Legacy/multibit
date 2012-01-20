package org.multibit.action;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.DumpedPrivateKey;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Export Private Keys submit action
 * 
 * @author jim
 * 
 */
public class ExportPrivateKeysSubmitAction implements Action {
    private MultiBitController controller;

    public ExportPrivateKeysSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the required output file
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item privateKeysOutputFilenameItem = data.getItem(MultiBitModel.PRIVATE_KEY_FILENAME);
                if (privateKeysOutputFilenameItem != null) {
                    String outputFilename = (String) privateKeysOutputFilenameItem.getNewValue();
                    // TODO check to see if file already exists - do not
                    // overwrite

                    try {
                        // Create file
                        FileWriter fstream = new FileWriter(outputFilename);
                        BufferedWriter out = new BufferedWriter(fstream);
                        
                        outputHeaderComment(out);
                        
                        // get the wallet's private keys and output them
                        Wallet activeWallet = controller.getModel().getActivePerWalletModelData().getWallet();
                        if (activeWallet != null) {
                            outputKeys(out, activeWallet.keychain);
                        }
                        
                        // Close the output stream
                        out.close();
                    } catch (Exception e) {// Catch exception if any
                        System.err.println("Error: " + e.getMessage());
                    }

                    // output comment text to file - this is always in English
                    // as
                    // the file
                    // is ASCII

                    // output private keys

                    // send back a summary text of whether the write was
                    // successful
                    // or not
                }
            }
        }
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
    }
    
    private void outputHeaderComment(BufferedWriter out) throws IOException{
        out.write("# MultiBit private key file");
        out.newLine();
        out.write("# Format:");
        out.newLine();
        out.write("#   <Base58 encoded private key>[<any number of whitespace characters>[<key createdAt in UTC format>]]");
        out.newLine();
        out.write("#   The Base58 encoded private keys are the same format as produced by the Satoshi client/ sipa dumpprivkey utility");
        out.newLine();
        out.write("#   keyCreatedAt is in UTC format as specified by ISO 8601 e.g: 2011-12-31T16:42:00Z . The century, 'T' and 'Z' are mandatory");
        out.newLine();
    }
    
    private void outputKeys(BufferedWriter out, ArrayList<ECKey> keychain) throws IOException{
        if (keychain != null) {
            for (ECKey ecKey : keychain) {
                DumpedPrivateKey dumpedPrivateKey = ecKey.getPrivateKeyEncoded(controller.getMultiBitService().getNetworkParameters());
                String keyOutput = dumpedPrivateKey.toString(); // need to add datetime of first use in UTC format
                out.write(keyOutput);
                out.newLine();
            }      
        }
        out.write("# End of keys");
        out.newLine();
    }
}
