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
package org.multibit.file;

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
import java.util.List;
import java.util.Properties;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.network.MultiBitService;
import org.multibit.utils.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Wallet;

/**
 * a class consolidating the File IO in MultiBit for wallets and wallet infos
 * 
 * it also has responsibility for determining the directory used for storing the
 * user specific application data
 * 
 * @author jim
 * 
 */
public class FileHandler {

    private static Logger log = LoggerFactory.getLogger(FileHandler.class);

    public static final String USER_PROPERTIES_FILE_NAME = "multibit.properties";
    public static final String USER_PROPERTIES_HEADER_TEXT = "multibit";

    private static final String SEPARATOR = "-";
    private static final String BACKUP_SUFFIX_FORMAT = "yyyyMMddHHmmss";

    private MultiBitController controller;

    private Date dateForBackupName = null;

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

        synchronized (walletInfo) {
            rememberFileSizesAndLastModified(walletFile, walletInfo);
            perWalletModelData.setDirty(false);
        }

        return perWalletModelData;
    }

    /**
     * save the perWalletModelData to file
     * 
     * @param perWalletModelData
     *            TODO give notification of whether data was written to a backup
     *            file
     * @param forceWrite
     *            force the write of the perWalletModelData
     */
    public void savePerWalletModelData(PerWalletModelData perWalletModelData, boolean forceWrite) {
        // WARNING: This wallet.toString() puts private keys in the log !

        // log.info("Wallet details for wallet file = " +
        // perWalletModelData.getWalletFilename() + "\n"
        // + perWalletModelData.getWallet().toString());
 
        if (perWalletModelData == null || perWalletModelData.getWalletFilename() == null) {
            // nothing to do
            return;
        }
        
        File walletFile = new File(perWalletModelData.getWalletFilename());
        
        WalletInfo walletInfo = perWalletModelData.getWalletInfo();
        
        synchronized (walletInfo) {
            try {
                // save the perWalletModelData if it is dirty or if forceWrite is true
                if (perWalletModelData.isDirty() || forceWrite) {
                    // check dates and sizes of files
                    boolean filesHaveChanged = haveFilesChanged(perWalletModelData);

                    if (!filesHaveChanged || forceWrite) {
                        // normal write of data

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

                        // the perWalletModelData is no longer dirty
                        perWalletModelData.setDirty(false);
                    } else {
                        // write to backup files
                        // work out / reuse the backup file names
                        String walletInfoBackupFilename = null;
                        String walletBackupFilename = null;

                        if (perWalletModelData.getWalletBackupFilename() != null) {
                            walletBackupFilename = perWalletModelData.getWalletBackupFilename();
                        }
                        if (perWalletModelData.getWalletInfoBackupFilename() != null) {
                            walletInfoBackupFilename = perWalletModelData.getWalletInfoBackupFilename();
                        }

                        if (walletBackupFilename == null) {
                            walletBackupFilename = createBackupFilename(walletFile, false);
                            perWalletModelData.setWalletBackupFilename(walletBackupFilename);

                            walletInfoBackupFilename = createBackupFilename(
                                    new File(WalletInfo.createWalletInfoFilename(perWalletModelData.getWalletFilename())), true);
                            perWalletModelData.setWalletInfoBackupFilename(walletInfoBackupFilename);
                        }

                        // save the companion wallet info
                        if (walletInfo != null) {
                            walletInfo.writeToFile(walletInfoBackupFilename);
                        }
                        // save the wallet file
                        if (perWalletModelData.getWallet() != null) {
                            perWalletModelData.getWallet().saveToFile(new File(walletBackupFilename));
                        }

                        // the perWalletModelData is no longer dirty
                        perWalletModelData.setDirty(false);
                    }

                }
            } catch (IOException e) {
                log.error(e.getMessage(), e);
            }
        }
        return;
    }

    /**
     * delete the wallet and the wallet info file
     * 
     * @param perWalletModelData
     */
    public void deleteWalletAndWalletInfo(PerWalletModelData perWalletModelData) {
        if (perWalletModelData == null) {
            // nothing to do
            return;
        }
        
        File walletFile = new File(perWalletModelData.getWalletFilename());
        WalletInfo walletInfo = perWalletModelData.getWalletInfo();
        String walletInfoFilenameAsString = WalletInfo.createWalletInfoFilename(perWalletModelData.getWalletFilename());
        File walletInfoFile = new File(walletInfoFilenameAsString);
               
        synchronized (walletInfo) {
            // see if either of the files are readonly - abort
            if (!walletFile.canWrite() || !walletInfoFile.canWrite()) {
                throw new DeleteWalletException(controller.getLocaliser().getString("deleteWalletException.walletWasReadonly"));
            }
            
            // delete the wallet info file first, then the wallet
            boolean success = walletInfoFile.delete();
            if (success) {
                success = walletFile.delete();
                if (success) {
                    // wallet file deleted ok
                    walletInfo.setDeleted(true);
                } else {
                    throw new DeleteWalletException(controller.getLocaliser().getString("deleteWalletException.genericCouldNotDelete",
                            new String[]{perWalletModelData.getWalletFilename()}));                        
                }
            } else {
                throw new DeleteWalletException(controller.getLocaliser().getString("deleteWalletException.couldNotDeleteWalletInfo",
                        new String[] {perWalletModelData.getWalletFilename()}));    
            }
        }
        
        // if the wallet was deleted, delete the model data
        if (walletInfo.isDeleted()) {
                controller.getModel().remove(perWalletModelData);
        }
        return;
    }

    public boolean haveFilesChanged(PerWalletModelData perWalletModelData) {
        if (perWalletModelData == null || perWalletModelData.getWalletFilename() == null) {
            return false;
        }

        boolean haveFilesChanged = false;

        String walletInfoFilename = WalletInfo.createWalletInfoFilename(perWalletModelData.getWalletFilename());
        File walletInfoFile = new File(walletInfoFilename);
        File walletFile = new File(perWalletModelData.getWalletFilename());

        WalletInfo walletInfo = perWalletModelData.getWalletInfo();

        if (walletInfo != null) {
            synchronized (walletInfo) {
                String walletFileSize = "" + walletFile.length();
                String walletFileLastModified = "" + walletFile.lastModified();
                String walletInfoFileSize = "" + walletInfoFile.length();
                String walletInfoFileLastModified = "" + walletInfoFile.lastModified();
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
                if (haveFilesChanged) {
                    log.debug("Result of check of whether files have changed for wallet filename "
                            + perWalletModelData.getWalletFilename() + " was " + haveFilesChanged + ".");
                }

                // create backup filenames early if the files have changed
                // (it is then available in the tooltip)
                if (haveFilesChanged && perWalletModelData.getWalletBackupFilename() == null) {
                    try {
                        perWalletModelData.setWalletBackupFilename(createBackupFilename(walletFile, false));

                        perWalletModelData.setWalletInfoBackupFilename(createBackupFilename(
                                new File(WalletInfo.createWalletInfoFilename(perWalletModelData.getWalletFilename())), true));
                    } catch (IOException e) {
                        log.error(e.getMessage(), e);
                    }
                }
            }
        }

        return haveFilesChanged;
    }

    /**
     * keep a record of the wallet and wallet info files sizes and date last
     * modified
     * 
     * @param walletFile
     *            The wallet file
     * @param walletInfo
     *            The wallet info
     */
    private void rememberFileSizesAndLastModified(File walletFile, WalletInfo walletInfo) {
        // get the files' last modified data and sizes and store them in the
        // wallet properties
        if (walletFile == null || walletInfo == null) {
            return;
        }

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

        // TODO Fix this - create a toString()
        log.debug("rememberFileSizesAndLastModified: Wallet filename " + walletFilename + " , " + MultiBitModel.WALLET_FILE_SIZE + " " + walletFileSize + " ,"
                + MultiBitModel.WALLET_FILE_LAST_MODIFIED + " " + walletFileLastModified + " ,"
                + MultiBitModel.WALLET_INFO_FILE_SIZE + " " + walletInfoFileSize + " ,"
                + MultiBitModel.WALLET_INFO_FILE_LAST_MODIFIED + " " + walletInfoFileLastModified);
    }

    public static void writeUserPreferences(MultiBitController controller) {
        // save all the wallets' filenames in the user preferences
        if (controller.getModel().getPerWalletModelDataList() != null) {
            List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            int numberOfWallets = perWalletModelDataList.size();
            log.debug("When writing wallets, there were " + numberOfWallets);
            controller.getModel().setUserPreference(MultiBitModel.NUMBER_OF_WALLETS, numberOfWallets + "");
            controller.getModel().setUserPreference(MultiBitModel.ACTIVE_WALLET_FILENAME,
                    controller.getModel().getActiveWalletFilename());
            if (numberOfWallets > 0) {
                for (int i = 1; i <= numberOfWallets; i++) {
                    PerWalletModelData perWalletModelData = perWalletModelDataList.get(i - 1);
                    if (perWalletModelData.getWalletFilename() != null) {
                        controller.getModel().setUserPreference(MultiBitModel.WALLET_FILENAME_PREFIX + i,
                                perWalletModelData.getWalletFilename());
                    }
                }
            }
        }

        // write the user preference properties
        Properties userPreferences = controller.getModel().getAllUserPreferences();
        OutputStream outputStream;
        try {
            String userPropertiesFilename;
            if ("".equals(controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory())) {
                userPropertiesFilename = USER_PROPERTIES_FILE_NAME;
            } else {
                userPropertiesFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory()
                        + File.separator + USER_PROPERTIES_FILE_NAME;
            }

            outputStream = new FileOutputStream(userPropertiesFilename);
            BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(outputStream);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(bufferedOutputStream, "UTF8");
            userPreferences.store(outputStreamWriter, USER_PROPERTIES_HEADER_TEXT);
        } catch (FileNotFoundException e) {
            log.error(e.getMessage(), e);
        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }
    }

    public void writeUserPreferences() {
        writeUserPreferences(controller);
    }

    public static Properties loadUserPreferences(ApplicationDataDirectoryLocator applicationDataDirectoryLocator) {
        Properties userPreferences = new Properties();
        try {
            String userPropertiesFilename;
            if ("".equals(applicationDataDirectoryLocator.getApplicationDataDirectory())) {
                userPropertiesFilename = USER_PROPERTIES_FILE_NAME;
            } else {
                userPropertiesFilename = applicationDataDirectoryLocator.getApplicationDataDirectory() + File.separator
                        + USER_PROPERTIES_FILE_NAME;
            }
            InputStream inputStream = new FileInputStream(userPropertiesFilename);
            InputStreamReader inputStreamReader = new InputStreamReader(inputStream, "UTF8");
            userPreferences.load(inputStreamReader);
        } catch (FileNotFoundException e) {
            // ok - may not have been created yet
        } catch (IOException e) {
            // ok may not be written yet
        }

        return userPreferences;
    }

    /**
     * create a backup filename the format is: original file: filename.suffix
     * backup file: filename-yyyymmddhhmmss.suffix
     * 
     * @param file
     * @param reusePreviousBackupDate
     *            Reuse the previously created backup date so that wallet and
     *            wallet info names match
     * @return
     * @throws IOException
     */
    private String createBackupFilename(File file, boolean reusePreviousBackupDate) throws IOException {
        String filename = file.getAbsolutePath();

        // find suffix
        int suffixSeparator = filename.lastIndexOf(".");
        String stem = filename.substring(0, suffixSeparator);
        String suffix = filename.substring(suffixSeparator); // includes
                                                             // separating dot

        if (dateForBackupName == null || !reusePreviousBackupDate) {
            dateForBackupName = new Date();
        }
        DateFormat dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        String backupFilename = stem + SEPARATOR + dateFormat.format(dateForBackupName) + suffix;

        return backupFilename;
    }

    /**
     * to support multiple users on the same machine, the block chain is
     * installed into the program installation directory and is then copied to
     * the user's application data directory when MultiBit is first used
     * 
     * thus each user has their own copy of the blockchain
     */
    public void copyBlockChainFromInstallationDirectory(MultiBitService multiBitService, String destinationBlockChainFilename) {
        if (destinationBlockChainFilename == null) {
            return;
        }

        // see if the block chain in the user's application data directory
        // exists
        // it is never overwritten
        File destinationBlockchain = new File(destinationBlockChainFilename);
        if (!destinationBlockchain.exists()) {
            // work out the source blockchain (put into the program installation
            // directory by the installer)
            File directory = new File(".");
            try {
                String currentWorkingDirectory = directory.getCanonicalPath();

                String filePrefix = multiBitService.getFilePrefix();
                String blockchainFilename = filePrefix + MultiBitService.BLOCKCHAIN_SUFFIX;
                String sourceBlockchainFilename = currentWorkingDirectory + File.separator + blockchainFilename;
                File sourceBlockchain = new File(sourceBlockchainFilename);
                if (sourceBlockchain.exists()) {
                    // it should exist since installer puts them in
                    log.info("Copying blockchain from '" + sourceBlockchainFilename + "' to '" + destinationBlockChainFilename
                            + "'");
                    long startTime = (DateUtils.nowUtc()).getMillis();
                    copyFile(sourceBlockchain, destinationBlockchain);
                    long stopTime = (DateUtils.nowUtc()).getMillis();
                    log.info("Time taken to copy blockchain was " + (stopTime - startTime) + " ms.");

                    // check all the data was copied
                    long sourceLength = sourceBlockchain.length();
                    long destinationLength = destinationBlockchain.length();
                    if (sourceLength != destinationLength) {
                        log.error("Blockchain was not copied to user's application data directory correctly.\nThe source blockchain '"
                                + sourceBlockchainFilename
                                + "' is of length "
                                + sourceLength
                                + "\nbut the destination blockchain '"
                                + destinationBlockChainFilename
                                + "' is of length "
                                + destinationLength);
                    }
                }

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public void copyFile(File sourceFile, File destinationFile) throws IOException {
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
}
