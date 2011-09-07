package org.multibit.action;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Properties;

import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBook;
import org.multibit.model.DataProvider;
import org.multibit.network.FileHandler;



/**
 * exit the application
 * @author jim
 *
 */
public class ExitAction implements Action{   
    private MultiBitController controller;
    
    public ExitAction(MultiBitController controller) { 
        this.controller = controller;
    }
    
    public void execute(DataProvider dataProvider) {
        // save the wallet file
        controller.getModel().saveWallet();

        // write the user properties
        FileHandler fileHandler = new FileHandler(controller);
        fileHandler.writeUserPreferences();
        
        // write the address book
        AddressBook addressBook = controller.getModel().getAddressBook();
        addressBook.writeToFile();
        
        // TODO write the wallet file
        
        // shut down the PeerGroup
        controller.getMultiBitService().getPeerGroup().stop();
        
        System.exit(0);     
    }
    
    public String getDisplayText() {
        // TODO localise
        return "exit";
    }
}


