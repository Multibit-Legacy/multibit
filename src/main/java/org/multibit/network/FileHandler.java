package org.multibit.network;

import com.google.bitcoin.core.Wallet;
import org.multibit.controller.MultiBitController;
import org.multibit.model.WalletInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.channels.FileChannel;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

/**
 * a class consolidating all the File IO in MultiBit
 * 
 * @author jim
 * 
 */
public class FileHandler {

    private Logger log = LoggerFactory.getLogger(FileHandler.class);

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
            // add the new wallet into the model
            controller.getModel().addWallet(wallet, walletFile.getAbsolutePath());
 
            WalletInfo walletInfo = new WalletInfo(walletFile.getAbsolutePath());
            controller.getModel().setWalletInfo(walletInfo);
        } catch (IOException e) {
            log.error("Failed to load wallet", e);
        }

        return wallet;
    }

    public void saveWalletToFile(Wallet wallet, File walletFile) {
        try {
            // save the companion wallet info
            WalletInfo walletInfo = controller.getModel().getWalletInfo();
            if (walletInfo != null) {
                walletInfo.writeToFile();
            } else {
                WalletInfo newWalletInfo = new WalletInfo(walletFile.getAbsolutePath());
                controller.getModel().setWalletInfo(newWalletInfo);
                newWalletInfo.writeToFile();
            }

            // add the new wallet and wallet filename on the model
            controller.getModel().addWallet(wallet, walletFile.getAbsolutePath());

            if (wallet != null) {
                wallet.saveToFile(walletFile);
            }
        } catch (IOException e) {
            log.error(e.getMessage(), e);
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
            log.error(e.getMessage(), e);
        } catch (IOException e) {
            log.error(e.getMessage(), e);
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
