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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.network.MultiBitService;
import org.multibit.store.MultiBitWalletProtobufSerializer;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.multibit.utils.DateUtils;
import org.multibit.viewsystem.View;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.KeyCrypterException;
import org.multibit.model.core.CoreModel;


/**
 * Class consolidating the File IO in MultiBit for wallets and wallet infos.
 * 
 * @author jim
 * 
 */
public class FileHandler {
    private static Logger log = LoggerFactory.getLogger(FileHandler.class);

    public static final String USER_PROPERTIES_FILE_NAME = "multibit.properties";
    public static final String USER_PROPERTIES_HEADER_TEXT = "multibit";

    private static final String SEPARATOR = "-";
    public static final String BACKUP_SUFFIX_FORMAT = "yyyyMMddHHmmss";

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private Date dateForBackupName = null;
    
    private DateFormat dateFormat;
    private MultiBitWalletProtobufSerializer walletProtobufSerializer;
    
    // Nonsense bytes to fill up deleted files - these have no meaning.
    private static byte[] NONSENSE_BYTES = new byte[]{(byte)0xF0, (byte)0xA6, (byte)0x55, (byte)0xAA, (byte)0x33, (byte)0x77, (byte)0x33, (byte)0x37,
                                                                (byte)0x12, (byte)0x34, (byte)0x56, (byte)0x78, (byte)0xC2, (byte)0xB3, (byte)0xA4, (byte)0x9A,
                                                                (byte)0x30, (byte)0x7F, (byte)0xE5, (byte)0x5A, (byte)0x23, (byte)0x47, (byte)0x13, (byte)0x17,
                                                                (byte)0x15, (byte)0x32, (byte)0x5C, (byte)0x77, (byte)0xC9, (byte)0x73, (byte)0x04, (byte)0x2D,
                                                                (byte)0x40, (byte)0x0F, (byte)0xA5, (byte)0xA6, (byte)0x43, (byte)0x77, (byte)0x33, (byte)0x3B,
                                                                (byte)0x62, (byte)0x34, (byte)0xB6, (byte)0x72, (byte)0x32, (byte)0xB3, (byte)0xA4, (byte)0x4B,
                                                                (byte)0x80, (byte)0x7F, (byte)0xC5, (byte)0x43, (byte)0x23, (byte)0x47, (byte)0x13, (byte)0xB7,
                                                                (byte)0xA5, (byte)0x32, (byte)0xDC, (byte)0x79, (byte)0x19, (byte)0xB1, (byte)0x03, (byte)0x9D};
    
    private static int BULKING_UP_FACTOR = 16;
    private static byte[] SECURE_DELETE_FILL_BYTES = new byte[NONSENSE_BYTES.length * BULKING_UP_FACTOR];
    
    static {
        // Make some SECURE_DELETE_FILL_BYTES bytes = x BULKING_UP_FACTOR the NONSENSE just to save write time.
        for (int i = 0; i < BULKING_UP_FACTOR; i++) {
            System.arraycopy(NONSENSE_BYTES, 0, SECURE_DELETE_FILL_BYTES, NONSENSE_BYTES.length * i, NONSENSE_BYTES.length);
        }
    }
    
    public FileHandler(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        walletProtobufSerializer = new MultiBitWalletProtobufSerializer();
    }

    public WalletData loadFromFile(File walletFile) throws WalletLoadException, WalletVersionException {
        if (walletFile == null) {
            return null;
        }

        String walletFilenameToUse = walletFile.getAbsolutePath();

        try {
            // See if the wallet is serialized or protobuf.
            WalletInfoData walletInfo;
            if (isWalletSerialised(walletFile)) {
                walletInfo = new WalletInfoData(walletFilenameToUse, MultiBitWalletVersion.SERIALIZED);
            } else {
                walletInfo = new WalletInfoData(walletFilenameToUse, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
            }
            
            // If the wallet file is missing or empty but the backup file exists load that instead.
            // This indicates that the write was interrupted (e.g. power loss).
            boolean saveImmediately = false;
            if (!walletFile.exists() || walletFile.length() == 0) {
                String walletBackupFilename =  walletInfo.getProperty(BitcoinModel.WALLET_BACKUP_FILE);
                if (walletBackupFilename != null && !"".equals(walletBackupFilename)) {
                    File walletBackupFile = new File(walletBackupFilename);
                    if (walletBackupFile.exists()) {
                        // Use the walletBackup.
                        log.debug("The wallet file '" + walletFile.getAbsolutePath() + "' is empty so using the wallet backup file of '" + walletBackupFile.getAbsolutePath() + "'.");
                        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("fileHandler.walletIsMissing", new Object[] {walletFile.getAbsolutePath(), walletBackupFile.getAbsolutePath()})));
                        walletFile = walletBackupFile;
                        
                        saveImmediately = true;
                        
                        // Wipe the wallet backup property so that the backup file will not be overwritten
                        walletInfo.put(BitcoinModel.WALLET_BACKUP_FILE, "");                 
                    }
                } 
            }

            FileInputStream fileInputStream = new FileInputStream(walletFile);
            InputStream stream = null;
            Wallet wallet = null;
            try {
                stream = new BufferedInputStream(fileInputStream);
                
                // serialised.1 wallets are stored as serialised.
                // protobuf.2   (unencrypted) wallets stored as Wallet messages
                // protobuf.3   (encrypted) wallets are stored as Wallet messages with a mandatory extension
                wallet = Wallet.loadFromFileStream(stream);
                
                // If wallet description is only in the wallet, copy it to the wallet info
                // (perhaps the user deleted/ did not copy the info file.
                if (walletInfo != null) {
                    String walletDescriptionInInfo = walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
                    if ((walletDescriptionInInfo == null || walletDescriptionInInfo.length() == 0) && wallet.getDescription() != null ) {
                        walletInfo.put(WalletInfoData.DESCRIPTION_PROPERTY, wallet.getDescription());
                    }
                }
            } finally {
                if (stream != null) {
                    stream.close();
                }
                fileInputStream.close();
            }
  
            // Add the new wallet into the model.
            wallet.setNetworkParameters(this.bitcoinController.getModel().getNetworkParameters());
            WalletData perWalletModelData = this.bitcoinController.getModel().addWallet(this.bitcoinController, wallet, walletFilenameToUse);

            perWalletModelData.setWalletInfo(walletInfo);

            if (saveImmediately) {
                savePerWalletModelData(perWalletModelData, true);
            }
            
            synchronized (walletInfo) {
                rememberFileSizesAndLastModified(new File(walletFilenameToUse), walletInfo);
                perWalletModelData.setDirty(false);
            }

            return perWalletModelData;
        } catch (WalletVersionException wve) {
            // We want this to propagate out.
            throw wve;
        } catch (Exception e) {
            e.printStackTrace();
            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
            throw new WalletLoadException(e.getClass().getCanonicalName() + " " + e.getMessage(), e);
        }
    }
 
    boolean isWalletSerialised(File walletFile) {
        boolean isWalletSerialised = false;
        InputStream stream = null;
        try {
            // Determine what kind of wallet stream this is: Java Serialization
            // or protobuf format.
            stream = new BufferedInputStream(new FileInputStream(walletFile));
            isWalletSerialised = stream.read() == 0xac && stream.read() == 0xed;
        } catch (FileNotFoundException e) {
            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
        } catch (IOException e) {
            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
                }
            }
        }
        return isWalletSerialised;
    }

    /**
     * Save the perWalletModelData to file.
     * 
     * @param perWalletModelData
     * @param forceWrite
     *            force the write of the perWalletModelData     
     */
    public void savePerWalletModelData(WalletData perWalletModelData, boolean forceWrite) {
        if (perWalletModelData == null || perWalletModelData.getWalletFilename() == null) {
            return;
        }

        // Log the stack trace of the call.
//        Exception exception = new IllegalStateException();
//        log.debug("Saving wallet '" + perWalletModelData.getWalletFilename() + "' from stack:");
//        if (exception != null && exception.getStackTrace() != null) {
//            for (int i = 0; i < exception.getStackTrace().length; i++) {
//                log.debug("    " + exception.getStackTrace()[i].getFileName() + " - " + exception.getStackTrace()[i].getMethodName() + ";" + exception.getStackTrace()[i].getLineNumber());    
//            }
//        }
        File walletFile = new File(perWalletModelData.getWalletFilename());

        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();

        if (walletInfo != null) {
            synchronized (walletInfo) {
                // Save the perWalletModelData if it is dirty or if forceWrite is true.
                if (perWalletModelData.isDirty() || forceWrite) {
                    // Check dates and sizes of files.
                    boolean filesHaveChanged = haveFilesChanged(perWalletModelData);

                    if (!filesHaveChanged || forceWrite) {
                        // Normal write of data.
                        String walletInfoFilename = WalletInfoData.createWalletInfoFilename(perWalletModelData.getWalletFilename());
                        saveWalletAndWalletInfo(perWalletModelData, perWalletModelData.getWalletFilename(), walletInfoFilename);

                        rememberFileSizesAndLastModified(walletFile, walletInfo);

                        // The perWalletModelData is no longer dirty.
                        perWalletModelData.setDirty(false);
                    } else {
                        // Write to backup files.
                        backupPerWalletModelData(perWalletModelData, null);
                    }
                }
            }
        }
        return;
    }

    /**
     * Backup the perWalletModelData. The lastFailedMigrateVersion parameter can
     * be used so that wallet migration is delayed for the backup should it ever
     * need to be opened in MultiBit (to stop the wallet migrate trying to
     * re-migrate it).
     * 
     * @param perWalletModelData
     * @param lastFailedMigrateVersion
     */
    public void backupPerWalletModelData(WalletData perWalletModelData, String lastFailedMigrateVersion) {
        // Write to backup files.
        // Work out / reuse the backup file names.
        String walletInfoBackupFilename = null;
        String walletBackupFilename = null;

        try {
            if (perWalletModelData.getWalletBackupFilename() != null) {
                walletBackupFilename = perWalletModelData.getWalletBackupFilename();
            }
            if (perWalletModelData.getWalletInfoBackupFilename() != null) {
                walletInfoBackupFilename = perWalletModelData.getWalletInfoBackupFilename();
            }

            if (walletBackupFilename == null) {
                walletBackupFilename = createBackupFilename(new File(perWalletModelData.getWalletFilename()), true, false, null);
                perWalletModelData.setWalletBackupFilename(walletBackupFilename);

                walletInfoBackupFilename = createBackupFilename(
                        new File(WalletInfoData.createWalletInfoFilename(perWalletModelData.getWalletFilename())), false, true, null);
                perWalletModelData.setWalletInfoBackupFilename(walletInfoBackupFilename);
            }

            String previousLastFailedMigrateVersion = perWalletModelData.getWalletInfo().getProperty(
                    BitcoinModel.LAST_FAILED_MIGRATE_VERSION);
            if (lastFailedMigrateVersion != null) {
                perWalletModelData.getWalletInfo().put(BitcoinModel.LAST_FAILED_MIGRATE_VERSION, lastFailedMigrateVersion);
            }
            saveWalletAndWalletInfo(perWalletModelData, walletBackupFilename, walletInfoBackupFilename);

            // Put the previous lastFailedMigrateVersion back.
            if (lastFailedMigrateVersion != null) {
                if (previousLastFailedMigrateVersion == null) {
                    perWalletModelData.getWalletInfo().remove(lastFailedMigrateVersion);
                } else {
                    perWalletModelData.getWalletInfo().put(BitcoinModel.LAST_FAILED_MIGRATE_VERSION,
                            previousLastFailedMigrateVersion);
                }
            }

            // The perWalletModelData is no longer dirty.
            perWalletModelData.setDirty(false);

            log.info("Written backup wallet files to '" + walletBackupFilename + "'");
        } catch (IOException ioe) {
            log.error(ioe.getClass().getCanonicalName() + " " + ioe.getMessage());
            throw new WalletSaveException("Cannot backup wallet '" + perWalletModelData.getWalletFilename(), ioe);
        }
    }

   /**
    * To protect the wallet data, the write is in steps:
    * 1) Create a backup file called <wallet file name>-<yyyymmddhhmmss>.wallet and copy the original wallet to that
    * 2) Write the new wallet to the walletFilename
    * 3) Delete the old backup file
    * 4) Make the backup file in step 1) the new backup file
    * 
    **/
    private void saveWalletAndWalletInfo(WalletData perWalletModelData, String walletFilename, String walletInfoFilename) {
        File walletFile = new File(walletFilename);
        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();
        
        FileOutputStream fileOutputStream = null;
        
        // Save the wallet file
        try {
            if (perWalletModelData.getWallet() != null) {
                // Wallet description is currently stored in the wallet info file but is now available on the wallet itself.
                // Store the description from the wallet info in the wallet - in the future the wallet value will be primary
                // and wallet infos can be deprecated.
                // TODO - migrate completely to use wallet description and then deprecate value in info file
                if (walletInfo != null) {
                    String walletDescriptionInInfoFile = walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
                    if (walletDescriptionInInfoFile != null) {
                        perWalletModelData.getWallet().setDescription(walletDescriptionInInfoFile);
                    }
                }
                String oldBackupFilename = perWalletModelData.getWalletInfo().getProperty(BitcoinModel.WALLET_BACKUP_FILE);
                File oldBackupFile = null;
                String newBackupFilename = null;
                if (null != oldBackupFilename && !"".equals(oldBackupFilename) ) {
                    oldBackupFile = new File(oldBackupFilename);
                }
                if (MultiBitWalletVersion.PROTOBUF == walletInfo.getWalletVersion() ||
                        MultiBitWalletVersion.PROTOBUF_ENCRYPTED == walletInfo.getWalletVersion()) {
                    newBackupFilename = copyExistingWalletToBackupAndDeleteOriginal(walletFile);
                }

                log.debug("Saving wallet file '" + walletFile.getAbsolutePath() + "' ...");
                if (MultiBitWalletVersion.SERIALIZED == walletInfo.getWalletVersion()) {
                    saveToFileAsSerialised(perWalletModelData.getWallet(), walletFile);
                } else {
                    // See if there are any encrypted private keys - if there
                    // are the wallet will be saved
                    // as encrypted and the version set to PROTOBUF_ENCRYPTED.
                    boolean walletIsActuallyEncrypted = false;
                    Wallet wallet = perWalletModelData.getWallet();
                    // Check all the keys individually.
                    for (ECKey key : wallet.getKeychain()) {
                        if (key.isEncrypted()) {
                            walletIsActuallyEncrypted = true;
                            break;
                        }
                    }

                    if (walletIsActuallyEncrypted) {
                        walletInfo.setWalletVersion(MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
                    }

                    if (MultiBitWalletVersion.PROTOBUF == walletInfo.getWalletVersion()) {
                        // Save as a Wallet message.
                        perWalletModelData.getWallet().saveToFile(walletFile);
                    } else if (MultiBitWalletVersion.PROTOBUF_ENCRYPTED == walletInfo.getWalletVersion()) {
                        fileOutputStream = new FileOutputStream(walletFile);

                        // Save as a Wallet message with a mandatory extension
                        // to prevent loading by older versions of multibit.
                        walletProtobufSerializer.writeWallet(perWalletModelData.getWallet(), fileOutputStream);
                    } else {
                        throw new WalletVersionException("Cannot save wallet '" + perWalletModelData.getWalletFilename()
                                + "'. Its wallet version is '" + walletInfo.getWalletVersion().toString()
                                + "' but this version of MultiBit does not understand that format.");
                    }
                }
                log.debug("... done saving wallet file.");

                
                if (MultiBitWalletVersion.PROTOBUF == walletInfo.getWalletVersion() ||
                        MultiBitWalletVersion.PROTOBUF_ENCRYPTED == walletInfo.getWalletVersion()) {
                    perWalletModelData.getWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, newBackupFilename);
                    
                    // Delete the oldBackupFile unless the user has manually opened it.
                    boolean userHasOpenedBackupFile = false;
                    List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();
                    if (perWalletModelDataList != null) {
                        for (WalletData perWalletModelDataLoop : perWalletModelDataList) {
                            if ((oldBackupFilename != null && oldBackupFilename.equals(perWalletModelDataLoop.getWalletFilename())) ||
                                (newBackupFilename != null && newBackupFilename.equals(perWalletModelDataLoop.getWalletFilename()))) {
                                userHasOpenedBackupFile = true;
                                break;
                            }
                        }
                    }
                    if (!userHasOpenedBackupFile) {
                        secureDelete(oldBackupFile);
                    }
                }
            }
        } catch (IOException ioe) {
            throw new WalletSaveException("Cannot save wallet '" + perWalletModelData.getWalletFilename(), ioe);
        } finally {
            if (fileOutputStream != null) {
                try {
                    fileOutputStream.flush();
                    fileOutputStream.close();
                } catch (IOException e) {
                    throw new WalletSaveException("Cannot save wallet '" + perWalletModelData.getWalletFilename(), e);
                }
            }
        }
        
        // Write wallet info.
        walletInfo.writeToFile(walletInfoFilename, walletInfo.getWalletVersion());
    }
    
    /**
     * Backup the private keys of the active wallet to a file with name <wallet name>-yyyymmddhhmmss.key
     * @param passwordToUse
     * @return File to which keys were backed up, or null if they were not.
     * @throws EncrypterDecrypterException 
     */
    public File backupPrivateKeys(CharSequence passwordToUse) throws IOException, KeyCrypterException {
        File privateKeysBackupFile = null;

        // Only encrypted files are backed up, and they must have a non blank password.
        if (passwordToUse != null && passwordToUse.length() > 0) {
            if (controller.getModel() != null
                    && this.bitcoinController.getModel().getActiveWalletWalletInfo() != null
                    && this.bitcoinController.getModel().getActiveWalletWalletInfo().getWalletVersion() == MultiBitWalletVersion.PROTOBUF_ENCRYPTED) {
                // Save a backup copy of the private keys, encrypted with the newPasswordToUse.
                PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(this.bitcoinController.getModel().getNetworkParameters());
                String privateKeysBackupFilename = createBackupFilename(new File(this.bitcoinController.getModel().getActiveWalletFilename()),
                        false, false, BitcoinModel.PRIVATE_KEY_FILE_EXTENSION);
                privateKeysBackupFile = new File(privateKeysBackupFilename);
                BlockChain blockChain = null;
                if (this.bitcoinController.getMultiBitService() != null) {
                    blockChain = this.bitcoinController.getMultiBitService().getChain();
                }
                
                privateKeysHandler.exportPrivateKeys(privateKeysBackupFile, this.bitcoinController.getModel().getActiveWallet(), blockChain, true, passwordToUse, passwordToUse);
            } else {
                log.debug("Wallet '" + this.bitcoinController.getModel().getActiveWalletFilename()
                        + "' private keys not backed up as not PROTOBUF_ENCRYPTED");
            }
        } else {
            log.debug("Wallet '" + this.bitcoinController.getModel().getActiveWalletFilename()
                    + "' private keys not backed up password was blank or of zero length");
        }
        return privateKeysBackupFile;
    }
    
    private String copyExistingWalletToBackupAndDeleteOriginal(File walletFile) throws IOException {
        // Create a backup file called <wallet filename>.<random number>.bak
        String newWalletBackupFilename = createBackupFilename(walletFile, false, false, null);
        File newWalletBackupFile = new File(newWalletBackupFilename);
        if (walletFile != null && walletFile.exists()) {
            FileHandler.copyFile(walletFile, newWalletBackupFile);
            if (walletFile.length() != newWalletBackupFile.length()) {
                throw new IOException("Failed to copy the existing wallet from '" + walletFile.getAbsolutePath() + "' to '"
                        + newWalletBackupFilename + "'");
            }

            if (!walletFile.getAbsolutePath().equals(newWalletBackupFile.getAbsolutePath())) {
                secureDelete(walletFile);
            }
        }
        
        return newWalletBackupFilename;
    }

    /**
     * Secure delete the wallet and the wallet info file.
     * 
     * @param perWalletModelData
     */
    public void deleteWalletAndWalletInfo(WalletData perWalletModelData) {
        if (perWalletModelData == null) {
            return;
        }

        File walletFile = new File(perWalletModelData.getWalletFilename());
        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();
        String walletInfoFilenameAsString = WalletInfoData.createWalletInfoFilename(perWalletModelData.getWalletFilename());
        File walletInfoFile = new File(walletInfoFilenameAsString);

        synchronized (walletInfo) {
            // See if either of the files are readonly - abort.
            if (!walletFile.canWrite() || !walletInfoFile.canWrite()) {
                throw new DeleteWalletException(controller.getLocaliser().getString("deleteWalletException.walletWasReadonly"));
            }

            // Delete the wallet info file first, then the wallet.
            try {
                FileHandler.secureDelete(walletInfoFile);
                FileHandler.secureDelete(walletFile);
                walletInfo.setDeleted(true);
            } catch (IOException ioe) {
                log.error(ioe.getClass().getCanonicalName() + " " + ioe.getMessage());
                throw new DeleteWalletException(controller.getLocaliser().getString("deleteWalletException.genericCouldNotDelete",
                        new String[] { perWalletModelData.getWalletFilename() }));
            }
        }

        // If the wallet was deleted, delete the model data.
        if (walletInfo.isDeleted()) {
            this.bitcoinController.getModel().remove(perWalletModelData);
        }
        return;
    }

    public boolean haveFilesChanged(WalletData perWalletModelData) {
        if (perWalletModelData == null || perWalletModelData.getWalletFilename() == null) {
            return false;
        }

        boolean haveFilesChanged = false;

        String walletInfoFilename = WalletInfoData.createWalletInfoFilename(perWalletModelData.getWalletFilename());
        File walletInfoFile = new File(walletInfoFilename);
        File walletFile = new File(perWalletModelData.getWalletFilename());

        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();

        if (walletInfo != null) {
            synchronized (walletInfo) {
                String walletFileSize = "" + walletFile.length();
                String walletFileLastModified = "" + walletFile.lastModified();
                String walletInfoFileSize = "" + walletInfoFile.length();
                String walletInfoFileLastModified = "" + walletInfoFile.lastModified();

                // log.debug("Previous wallet file size was '" + walletInfo.getProperty(BitcoinModel.WALLET_FILE_SIZE)
                //         + "'. It is now '" + walletFileSize + "'");
                if (!walletFileSize.equals(walletInfo.getProperty(BitcoinModel.WALLET_FILE_SIZE))) {
                    haveFilesChanged = true;
                }

                // log.debug("Previous wallet file modification date was '"
                //        + walletInfo.getProperty(BitcoinModel.WALLET_FILE_LAST_MODIFIED) + "'. It is now '"
                //         + walletFileLastModified + "'");
                if (!walletFileLastModified.equals(walletInfo.getProperty(BitcoinModel.WALLET_FILE_LAST_MODIFIED))) {
                    haveFilesChanged = true;
                }

                // log.debug("Previous wallet info file size was '" + walletInfo.getProperty(BitcoinModel.WALLET_INFO_FILE_SIZE)
                //         + "'. It is now '" + walletInfoFileSize + "'");
                if (!walletInfoFileSize.equals(walletInfo.getProperty(BitcoinModel.WALLET_INFO_FILE_SIZE))) {
                    haveFilesChanged = true;
                }

                // log.debug("Previous wallet info file modification date was '"
                //         + walletInfo.getProperty(BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED) + "'. It is now '"
                //         + walletInfoFileLastModified + "'");
//                if (!walletInfoFileLastModified.equals(walletInfo.getProperty(BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED))) {
//                    haveFilesChanged = true;
//                 }

                if (haveFilesChanged) {
                    log.debug("Result of check of whether files have changed for wallet filename "
                            + perWalletModelData.getWalletFilename() + " was " + haveFilesChanged + ".");
                    log.debug(BitcoinModel.WALLET_FILE_SIZE + " " + walletFileSize + " ,"
                            + BitcoinModel.WALLET_FILE_LAST_MODIFIED + " " + walletFileLastModified + " ,"
                            + BitcoinModel.WALLET_INFO_FILE_SIZE + " " + walletInfoFileSize + " ,"
                            + BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED + " " + walletInfoFileLastModified);
                }

                // Create backup filenames early if the files have changed.
                // (It is then available in the tooltip).
                if (haveFilesChanged && perWalletModelData.getWalletBackupFilename() == null) {
                    try {
                        perWalletModelData.setWalletBackupFilename(createBackupFilename(walletFile, true, false, null));

                        perWalletModelData.setWalletInfoBackupFilename(createBackupFilename(
                                new File(WalletInfoData.createWalletInfoFilename(perWalletModelData.getWalletFilename())), false, true,
                                null));
                    } catch (IOException e) {
                        log.error(e.getMessage(), e);
                    }
                }
            }
        }

        return haveFilesChanged;
    }

    /**
     * Keep a record of the wallet and wallet info files sizes and date last
     * modified.
     * 
     * @param walletFile
     *            The wallet file
     * @param walletInfo
     *            The wallet info
     */
    private void rememberFileSizesAndLastModified(File walletFile, WalletInfoData walletInfo) {
        // Get the files' last modified data and sizes and store them in the wallet properties.
        if (walletFile == null || walletInfo == null) {
            return;
        }

        long walletFileSize = walletFile.length();
        long walletFileLastModified = walletFile.lastModified();

        String walletFilename = walletFile.getAbsolutePath();
        String walletInfoFilename = WalletInfoData.createWalletInfoFilename(walletFilename);
        File walletInfoFile = new File(walletInfoFilename);
        long walletInfoFileSize = walletInfoFile.length();
        long walletInfoFileLastModified = walletInfoFile.lastModified();

        walletInfo.put(BitcoinModel.WALLET_FILE_SIZE, "" + walletFileSize);
        walletInfo.put(BitcoinModel.WALLET_FILE_LAST_MODIFIED, "" + walletFileLastModified);
        walletInfo.put(BitcoinModel.WALLET_INFO_FILE_SIZE, "" + walletInfoFileSize);
        walletInfo.put(BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED, "" + walletInfoFileLastModified);

        log.debug("rememberFileSizesAndLastModified: Wallet filename " + walletFilename + " , " + BitcoinModel.WALLET_FILE_SIZE
                + " " + walletFileSize + " ," + BitcoinModel.WALLET_FILE_LAST_MODIFIED + " " + walletFileLastModified + " ,"
                + BitcoinModel.WALLET_INFO_FILE_SIZE + " " + walletInfoFileSize + " ,"
                + BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED + " " + walletInfoFileLastModified);
    }

    public static void writeUserPreferences(BitcoinController bitcoinController) {
        final Controller controller = bitcoinController;
        // Save all the wallets' filenames in the user preferences.
        if (bitcoinController.getModel().getPerWalletModelDataList() != null) {
            List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();
      
            List<String> orderList = new ArrayList<String>();
            List<String> earlyList = new ArrayList<String>();
            List<String> protobuf3List = new ArrayList<String>();
            
            for (WalletData perWalletModelData : perWalletModelDataList) {
                // Check if this is the initial empty WalletData
                if ("".equals(perWalletModelData.getWalletFilename()) || perWalletModelData.getWalletFilename() == null || perWalletModelData.getWalletInfo() == null) {
                    continue;
                }
                
                // Do not save deleted wallets
                if (perWalletModelData.getWalletInfo().isDeleted()) {
                    log.debug("Not writing out info about wallet '" + perWalletModelData.getWalletFilename() + "' as it has been deleted");
                    continue;
                }
                
                if (!orderList.contains(perWalletModelData.getWalletFilename())) {
                    orderList.add(perWalletModelData.getWalletFilename());
                }
                
                if (perWalletModelData.getWalletInfo().getWalletVersion() == MultiBitWalletVersion.PROTOBUF_ENCRYPTED) {
                    if (!protobuf3List.contains(perWalletModelData.getWalletFilename())) {
                        protobuf3List.add(perWalletModelData.getWalletFilename());
                    }
                 } else if (perWalletModelData.getWalletInfo().getWalletVersion() == null
                         || perWalletModelData.getWalletInfo().getWalletVersion() == MultiBitWalletVersion.SERIALIZED 
                         || perWalletModelData.getWalletInfo().getWalletVersion() == MultiBitWalletVersion.PROTOBUF) {
                    if (!earlyList.contains(perWalletModelData.getWalletFilename())) {
                        earlyList.add(perWalletModelData.getWalletFilename());
                    }
                }
            }
            
            int orderCount = 1;
            for (String walletFilename : orderList) {
                controller.getModel().setUserPreference(BitcoinModel.WALLET_ORDER_PREFIX + orderCount, walletFilename);
                orderCount++;  
            }
            controller.getModel().setUserPreference(BitcoinModel.WALLET_ORDER_TOTAL, "" + orderList.size());
        
            int earlyCount = 1;
            for (String walletFilename : earlyList) {
                controller.getModel().setUserPreference(BitcoinModel.EARLY_WALLET_FILENAME_PREFIX + earlyCount, walletFilename);
                earlyCount++;
            }
            controller.getModel().setUserPreference(BitcoinModel.NUMBER_OF_EARLY_WALLETS, "" + earlyList.size());
            
            int protobuf3Count = 1;
            for (String walletFilename : protobuf3List) {
                controller.getModel().setUserPreference(BitcoinModel.PROTOBUF3_WALLET_FILENAME_PREFIX + protobuf3Count, walletFilename);
                protobuf3Count++;
            }
            controller.getModel().setUserPreference(BitcoinModel.NUMBER_OF_PROTOBUF3_WALLETS, "" + protobuf3List.size());
            controller.getModel().setUserPreference(BitcoinModel.ACTIVE_WALLET_FILENAME, bitcoinController.getModel().getActiveWalletFilename());
        }

        Properties userPreferences = controller.getModel().getAllUserPreferences();
     
        // If the view is marked as also requiring a backwards compatible numeric field write that.
        @SuppressWarnings("deprecation")
        int currentViewNumericFormat = View.toOldViewNumeric(controller.getCurrentView());
        if (currentViewNumericFormat != 0) {
            userPreferences.put(CoreModel.SELECTED_VIEW, "" + currentViewNumericFormat);
        } else {
            // Make sure the old numeric value for a view is not in the user properties.
            userPreferences.remove(CoreModel.SELECTED_VIEW);
        }
        // Write the user preference properties.
        OutputStream outputStream = null;
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
        } finally {
            if (outputStream != null) {
                try {
                    outputStream.flush();
                    outputStream.close();
                } catch (IOException e) {
                    log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
                } finally {
                    outputStream = null;
                }
            }
        }
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
            // Ok - may not have been created yet.
        } catch (IOException e) {
            // Ok may not be written yet.
        }

        return userPreferences;
    }

    /**
     * Uses Java serialization to save the wallet to the given file.
     */
    private synchronized void saveToFileAsSerialised(Wallet wallet, File f) throws IOException {
        log.debug("Saving wallet to file " + f.getAbsolutePath() + " in version 1 (serialised) format.");
        FileOutputStream stream = null;
        try {
            stream = new FileOutputStream(f);
            saveToFileStreamAsSerialised(wallet, stream);
        } finally {
            if (stream != null) {
                stream.close();
                stream = null;
            }
        }
    }

    /**
     * Uses Java serialization to save the wallet to the given file stream.
     */
    private synchronized void saveToFileStreamAsSerialised(Wallet wallet, OutputStream f) throws IOException {
        ObjectOutputStream oos = new ObjectOutputStream(f);
        oos.writeObject(wallet);
        oos.flush();
        oos.close();
    }

    /**
     * Create a backup filename the format is: original file: filename.suffix.
     * backup file: filename-yyyymmddhhmmss.suffix
     * 
     * @param file
     * @param saveBackupDate - save the backup date for use later
     * @param reusePreviousBackupDate
     *            Reuse the previously created backup date so that wallet and
     *            wallet info names match
     * @param suffixToUse the suffix text to use
     * @return
     * @throws IOException
     */
    public String createBackupFilename(File file, boolean saveBackupDate, boolean reusePreviousBackupDate, String suffixToUse) throws IOException {
        String filename = file.getAbsolutePath();

        // Find suffix.
        int suffixSeparator = filename.lastIndexOf(".");
        String stem = filename.substring(0, suffixSeparator);
        String suffix;
        if (suffixToUse != null) {
            suffix = "." + suffixToUse;
        } else {
            suffix = filename.substring(suffixSeparator); // Includes separating dot.
        }                                                
        Date backupDateToUse = new Date();
        
        if (saveBackupDate) {
            dateForBackupName = backupDateToUse;
        }
        
        if (reusePreviousBackupDate) {
            backupDateToUse = dateForBackupName;
        }
        String backupFilename = stem + SEPARATOR + dateFormat.format(backupDateToUse) + suffix;

        return backupFilename;
    }

    /**
     * To support multiple users on the same machine, the checkpoints file is
     * installed into the program installation directory and is then copied to
     * the user's application data directory when MultiBit is first used.
     * 
     * Thus each user has their own copy of the blockchain.
     */
    public void copyCheckpointsFromInstallationDirectory(String destinationCheckpointsFilename)
            throws IOException {
        if (destinationCheckpointsFilename == null) {
            return;
        }

        // See if the block chain in the user's application data directory exists.
        File destinationCheckpoints = new File(destinationCheckpointsFilename);

        if (!destinationCheckpoints.exists()) {
            // Work out the source checkpoints (put into the program installation directory by the installer).
            File directory = new File(".");
            String currentWorkingDirectory = directory.getCanonicalPath();

            String filePrefix = MultiBitService.getFilePrefix();
            String checkpointsFilename = filePrefix + MultiBitService.CHECKPOINTS_SUFFIX;
            String sourceCheckpointsFilename = currentWorkingDirectory + File.separator + checkpointsFilename;
            File sourceBlockcheckpoints = new File(sourceCheckpointsFilename);
            if (sourceBlockcheckpoints.exists() && !destinationCheckpointsFilename.equals(sourceCheckpointsFilename)) {
                // It should exist since installer puts them in.
                log.info("Copying checkpoints from '" + sourceCheckpointsFilename + "' to '" + destinationCheckpointsFilename + "'");
                copyFile(sourceBlockcheckpoints, destinationCheckpoints);

                // Check all the data was copied.
                long sourceLength = sourceBlockcheckpoints.length();
                long destinationLength = destinationCheckpoints.length();
                if (sourceLength != destinationLength) {
                    String errorText = "Checkpoints were not copied to user's application data directory correctly.\nThe source checkpoints '"
                            + sourceCheckpointsFilename
                            + "' is of length "
                            + sourceLength
                            + "\nbut the destination checkpoints '"
                            + destinationCheckpointsFilename
                            + "' is of length "
                            + destinationLength;
                    log.error(errorText);
                    throw new FileHandlerException(errorText);
                }
            }
        }
    }

    /**
     * To support multiple users on the same machine, the block chain is
     * installed into the program installation directory and is then copied to
     * the user's application data directory when MultiBit is first used.
     * 
     * Thus each user has their own copy of the blockchain.
     */
    public void copyBlockChainFromInstallationDirectory(String destinationBlockChainFilename, boolean alwaysOverWrite)
            throws IOException {
        if (destinationBlockChainFilename == null) {
            return;
        }

        // See if the block chain in the user's application data directory
        // exists.
        File destinationBlockchain = new File(destinationBlockChainFilename);

        if (!destinationBlockchain.exists() || alwaysOverWrite) {
            // Work out the source blockchain (put into the program installation
            // directory by the installer).
            File directory = new File(".");
            String currentWorkingDirectory = directory.getCanonicalPath();

            String filePrefix = MultiBitService.getFilePrefix();
            String blockchainFilename = filePrefix + MultiBitService.BLOCKCHAIN_SUFFIX;
            String sourceBlockchainFilename = currentWorkingDirectory + File.separator + blockchainFilename;
            File sourceBlockchain = new File(sourceBlockchainFilename);
            if (sourceBlockchain.exists() && !destinationBlockChainFilename.equals(sourceBlockchainFilename)) {
                // It should exist since installer puts them in.
                log.info("Copying blockchain from '" + sourceBlockchainFilename + "' to '" + destinationBlockChainFilename + "'");
                
                if (alwaysOverWrite) {
                    // Delete the existing destinationBlockchain if it exists.
                    if (destinationBlockchain.exists()) {
                        destinationBlockchain.delete();
                    }
                }
                long startTime = (DateUtils.nowUtc()).getMillis();
                copyFile(sourceBlockchain, destinationBlockchain);
                long stopTime = (DateUtils.nowUtc()).getMillis();
                log.info("Time taken to copy blockchain was " + (stopTime - startTime) + " ms.");

                // Check all the data was copied.
                long sourceLength = sourceBlockchain.length();
                long destinationLength = destinationBlockchain.length();
                if (sourceLength != destinationLength) {
                    String errorText = "Blockchain was not copied to user's application data directory correctly.\nThe source blockchain '"
                            + sourceBlockchainFilename
                            + "' is of length "
                            + sourceLength
                            + "\nbut the destination blockchain '"
                            + destinationBlockChainFilename
                            + "' is of length "
                            + destinationLength;
                    log.error(errorText);
                    throw new FileHandlerException(errorText);
                }
            }
        }
    }

    public static void copyFile(File sourceFile, File destinationFile) throws IOException {
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
                source = null;
            } else if (fileInputStream != null) {
                fileInputStream.close();
                fileInputStream = null;
            }
            if (destination != null) {
                destination.close();
                destination = null;
            } else if (fileOutputStream != null) {
                fileOutputStream.flush();
                fileOutputStream.close();
            }
        }
    }

    public static File createTempDirectory(String filePrefix) throws IOException {
        final File temp;

        temp = File.createTempFile(filePrefix, Long.toString(System.currentTimeMillis()));

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
     * Delete a file with an overwrite of all of the data.
     * 
     * Set bit patterns are used rather than random numbers to avoid
     * a futex_wait_queue_me error on Linux systems (related to /dev/random usage)
     * 
     * @param file
     * @throws IOException
     */
    public static void secureDelete(File file) throws IOException {
        log.debug("Start of secureDelete");
        
        RandomAccessFile raf = null;
        if (file != null && file.exists()) {
            try {
                // Prep for file delete as this can be fiddly on windows.
                // Make sure it is writable and any references to it are garbage collected and finalized.
                file.setWritable(true);
                System.gc();
                
                long length = file.length();
                raf = new RandomAccessFile(file, "rws");
                raf.seek(0);
                raf.getFilePointer();
                int pos = 0;
                while (pos < length) {
                    raf.write(SECURE_DELETE_FILL_BYTES);
                    pos += SECURE_DELETE_FILL_BYTES.length;
                }
            } finally {
                if (raf != null) {
                    raf.close();
                    raf = null;
                }
            }
            boolean deleteSuccess = file.delete();
            log.debug("Result of delete of file '" + file.getAbsolutePath() + "' was " + deleteSuccess);
        }
        log.debug("End of secureDelete");
    }
}
