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
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Wallet;

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

    public PerWalletModelData loadFromFile(File walletFile) throws IOException {
        if (walletFile == null) {
            return null;
        }

        String walletFilename = walletFile.getAbsolutePath();
        Wallet wallet = Wallet.loadFromFile(walletFile);

        // add the new wallet into the model
        PerWalletModelData perWalletModelData = controller.getModel().addWallet(wallet, walletFilename);

        WalletInfo walletInfo = new WalletInfo(walletFilename);
        perWalletModelData.setWalletInfo(walletInfo);

        rememberFileSizesAndLastModified(walletFile, walletInfo);

        return perWalletModelData;
    }

    /**
     * save the perWalletModelData to file
     * 
     * @param perWalletModelData
     * @return saveAbortedBecauseFilesHaveChanged True if one of the files has
     *         changed by another process - save aborted. False if no files were
     *         changed by another process.
     */
    public boolean savePerWalletModelData(PerWalletModelData perWalletModelData) {
        boolean saveAbortedBecauseFilesHaveChanged = false;
        try {
            if (perWalletModelData == null) {
                // nothing to do
                return false;
            }

            // check dates and sizes of files
            saveAbortedBecauseFilesHaveChanged = haveFilesChanged(perWalletModelData);

            if (!saveAbortedBecauseFilesHaveChanged) {
                File walletFile = new File(perWalletModelData.getWalletFilename());
                WalletInfo walletInfo = perWalletModelData.getWalletInfo();

                // save the companion wallet info
                if (walletInfo != null) {
                    walletInfo.writeToFile();
                } else {
                    walletInfo = new WalletInfo(perWalletModelData.getWalletFilename());
                    perWalletModelData.setWalletInfo(walletInfo);
                    walletInfo.writeToFile();
                }

                if (perWalletModelData.getWallet() != null) {
                    perWalletModelData.getWallet().saveToFile(walletFile);
                }

                rememberFileSizesAndLastModified(walletFile, walletInfo);
            }
        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }
        return saveAbortedBecauseFilesHaveChanged;
    }

    public boolean haveFilesChanged(PerWalletModelData perWalletModelData) {
        if (perWalletModelData == null || perWalletModelData.getWalletFilename() == null) {
            return false;
        }
        
        boolean haveFilesChanged = false;

        String walletInfoFilename = WalletInfo.createWalletInfoFilename(perWalletModelData.getWalletFilename());
        File walletInfoFile = new File(walletInfoFilename);
        File walletFile = new File(perWalletModelData.getWalletFilename());

        String walletFileSize = "" + walletFile.length();
        String walletFileLastModified = "" + walletFile.lastModified();
        String walletInfoFileSize = "" + walletInfoFile.length();
        String walletInfoFileLastModified = "" + walletInfoFile.lastModified();

        WalletInfo walletInfo = perWalletModelData.getWalletInfo();
        if (walletInfo != null) {
            if (!walletFileSize.equals(walletInfo.getProperty(MultiBitModel.WALLET_FILE_SIZE))) {
                haveFilesChanged = true;
            }

            if (!walletFileLastModified.equals(walletInfo.getProperty(MultiBitModel.WALLET_FILE_LAST_MODIFIED))) {
                haveFilesChanged = true;
            }

            if (!walletInfoFileSize.equals(walletInfo.getProperty(MultiBitModel.WALLET_INFO_FILE_SIZE))) {
                haveFilesChanged = true;
            }

            if (!walletInfoFileLastModified.equals(walletInfo.getProperty(MultiBitModel.WALLET_INFO_FILE_LAST_MODIFIED))) {
                haveFilesChanged = true;
            }
        }
        log.info("Result of check of whether files have changed for wallet filename " + perWalletModelData.getWalletFilename()
                + " was " + haveFilesChanged + ".");

        return haveFilesChanged;
    }

    /**
     * keep a record of the wallet and wallet info files sizes and date last
     * modified
     * 
     * @param walletFilename
     */
    private void rememberFileSizesAndLastModified(File walletFile, WalletInfo walletInfo) {
        // get the files' last modified data and sizes and store them in the
        // wallet properties
        long walletFileSize = walletFile.length();
        long walletFileLastModified = walletFile.lastModified();

        String walletFilename = walletFile.getAbsolutePath();
        String walletInfoFilename = WalletInfo.createWalletInfoFilename(walletFilename);
        File walletInfoFile = new File(walletInfoFilename);
        long walletInfoFileSize = walletInfoFile.length();
        long walletInfoFileLastModified = walletInfoFile.lastModified();

        walletInfo.put(MultiBitModel.WALLET_FILE_SIZE, "" + walletFileSize);
        walletInfo.put(MultiBitModel.WALLET_FILE_LAST_MODIFIED, "" + walletFileLastModified);
        walletInfo.put(MultiBitModel.WALLET_INFO_FILE_SIZE, "" + walletInfoFileSize);
        walletInfo.put(MultiBitModel.WALLET_INFO_FILE_LAST_MODIFIED, "" + walletInfoFileLastModified);
        log.info("Wallet filename " + walletFilename + " , " + MultiBitModel.WALLET_FILE_SIZE + " " + walletFileSize + " ,"
                + MultiBitModel.WALLET_FILE_LAST_MODIFIED + " " + walletFileLastModified + " ,"
                + MultiBitModel.WALLET_INFO_FILE_SIZE + " " + walletInfoFileSize + " ,"
                + MultiBitModel.WALLET_INFO_FILE_LAST_MODIFIED + " " + walletInfoFileLastModified);
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
