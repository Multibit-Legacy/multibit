package org.multibit.viewsystem.commandline.view;

import java.io.InputStream;
import java.io.PrintStream;
import java.math.BigInteger;
import java.text.SimpleDateFormat;

import org.multibit.Localiser;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletTableData;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;

/**
 * the help about command line view 
 * 
 * @author jim
 * 
 */
public class HomePageView extends AbstractView{
    private MultiBitModel model;
    
    public static final String SEPARATOR = "    ";

    public HomePageView(MultiBitModel model, Localiser localiser, String viewDescription, InputStream inputStream,
            PrintStream printStream) {
        super(localiser, viewDescription, inputStream, printStream);
        this.model = model;
    }

    /**
     * display the home page to the user
     */
    @Override
    public void displayView() {
        printStream.println(CommandLineViewSystem.TEXT_VIEW_OUTPUT_PREFIX
                + CommandLineViewSystem.DISPLAY_VIEW_PREFIX + description);
        
        // output filename
        String walletFilename = model.getWalletFilename();
        String filenameMessage = localiser.getString("homePageView.filenameText",
                new Object[] { walletFilename });
        printStream.println(filenameMessage);

        // output current balance
        BigInteger balance = model.getBalance();
        String balanceMessage = localiser.getString("homePageView.balanceText",
                new Object[] { balance });   // TODO convert from nanocoins
        printStream.println(balanceMessage);
        
        // output transaction headers
        String printfFormat = "%10s %-19s %-90s %10s %10s %n";
        
        if (WalletTableData.COLUMN_HEADER_KEYS.length >= 5) {
            printStream.format(printfFormat, 
                    localiser.getString(WalletTableData.COLUMN_HEADER_KEYS[0]),
                    localiser.getString(WalletTableData.COLUMN_HEADER_KEYS[1]),
                    localiser.getString(WalletTableData.COLUMN_HEADER_KEYS[2]),
                    localiser.getString(WalletTableData.COLUMN_HEADER_KEYS[3]),
                    localiser.getString(WalletTableData.COLUMN_HEADER_KEYS[4]));
        }
        
        // output the transaction data
         SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy hh:mm");

         for (WalletTableData walletData : model.getWalletData()) {
             String creditText = "";
             if (walletData.getCredit() != null) {
                 creditText = walletData.getCredit().toString();
             }
             
             String debitText = "";
             if (walletData.getDebit() != null) {
                 debitText = walletData.getDebit().toString();
             }
             printStream.format(printfFormat, walletData.getHeight(),
                     dateFormatter.format(walletData.getDate()), walletData.getDescription(),
                     debitText, creditText);
         }
         
         printStream.print("\n");

        // show menu of actions and process response
        displayActionsAndProcessResponse();
    }
}
