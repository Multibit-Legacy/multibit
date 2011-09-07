package org.multibit.network;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.channels.FileChannel;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import org.multibit.controller.MultiBitController;
import org.multibit.model.WalletInfo;

import com.google.bitcoin.core.Wallet;

/**
 * a class consolidating all the File IO in MultiBit
 * 
 * @author jim
 * 
 */
public class FileHandler {

    public static final String USER_PROPERTIES_FILE_NAME = "multibit.properties";
    public static final String USER_PROPERTIES_HEADER_TEXT = "multibit";

    private static final String SEPARATOR = "-";
    private static final String BACKUP_SUFFIX_FORMAT = "yyyyMMddHHmmss";
    
    private MultiBitController controller;

    public FileHandler(MultiBitController controller) {
        this.controller = controller;
    }

    public Wallet loadWalletFromFile(File walletFile) {
        Wallet wallet = null;
        try {
            wallet = Wallet.loadFromFile(walletFile);
            // set the new wallet into the model
            controller.getModel().setWallet(wallet);
            controller.getModel().setWalletFilename(walletFile.getAbsolutePath());
            
            WalletInfo walletInfo = new WalletInfo(walletFile.getAbsolutePath());
            controller.getModel().setWalletInfo(walletInfo);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        return wallet;
    }
    
    public void saveWalletToFile(Wallet wallet, File walletFile) {
        try {
            // save the companion wallet info
            WalletInfo walletInfo = controller.getModel().getWalletInfo();
            if (walletInfo != null) {
                walletInfo.writeToFile();
            }
            // set the new wallet and wallet filename on the model
            controller.getModel().setWalletFilename(walletFile.getAbsolutePath());
            controller.getModel().setWallet(wallet);

            wallet.saveToFile(walletFile);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    public void writeUserPreferences() {
        // write the user preference properties
        Properties userPreferences = controller.getModel().getAllUserPreferences();
        OutputStream outputStream;
        try {
            outputStream = new FileOutputStream(USER_PROPERTIES_FILE_NAME);
            BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(outputStream);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(bufferedOutputStream, "UTF8");
            userPreferences.store(outputStreamWriter, USER_PROPERTIES_HEADER_TEXT);
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public static Properties loadUserPreferences() {
        Properties userPreferences = new Properties();
        try {
            InputStream inputStream = new FileInputStream(USER_PROPERTIES_FILE_NAME);
            InputStreamReader inputStreamReader = new InputStreamReader(inputStream, "UTF8");
            userPreferences.load(inputStreamReader);
        } catch (FileNotFoundException e) {
            // ok - may not have been created yet
        } catch (IOException e) {
            // ok may not be written yet
        }

        return userPreferences;
    }
     
    public String createBackupFile(File file) throws IOException {
        String filename = file.getAbsolutePath();
        DateFormat dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        String backupFilename = filename + SEPARATOR + dateFormat.format(new Date());
        
        File backupFile = new File(backupFilename);
        copyFile(file, backupFile);

        return backupFilename;
    }

    public String createInfoFilenameFromWalletFilename(File file) throws IOException {
        String filename = file.getAbsolutePath();
        DateFormat dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        String backupFilename = filename + SEPARATOR + dateFormat.format(new Date());
        
        File backupFile = new File(backupFilename);
        copyFile(file, backupFile);

        return backupFilename;
    }

    public void copyFile(File sourceFile, File destinationFile) throws IOException {
        if (!destinationFile.exists()) {
            destinationFile.createNewFile();
        }
        FileInputStream fileInputStream = null;
        FileOutputStream fileOutpurStream = null;
        FileChannel source = null;
        FileChannel destination = null;
        try {
            fileInputStream = new FileInputStream(sourceFile);
            source = fileInputStream.getChannel();
            fileOutpurStream = new FileOutputStream(destinationFile);
            destination = fileOutpurStream.getChannel();
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
            } else if (fileOutpurStream != null) {
                fileOutpurStream.close();
            }
        }
    }
}
