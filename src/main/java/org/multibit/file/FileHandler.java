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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.security.SecureRandom;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
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
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.util.Arrays;

import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.crypto.EncryptedPrivateKey;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterException;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.protobuf.ByteString;

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

    public static final String TOP_LEVEL_WALLET_BACKUP_SUFFIX = "-data";
    public static final String PRIVATE_KEY_BACKUP_DIRECTORY_NAME = "key-backup";
    public static final String ROLLING_WALLET_BACKUP_DIRECTORY_NAME = "rolling-backup";
    public static final String ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME = "wallet-backup";
    public static final String UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME = "wallet-unenc-backup";
    
    public static final String INFO_FILE_SUFFIX_STRING = "info";
    public static final String FILE_ENCRYPTED_WALLET_SUFFIX = "cipher";

    transient private static SecureRandom secureRandom = new SecureRandom();
    public static final int EXPECTED_LENGTH_OF_SALT = 8;
    public static final int EXPECTED_LENGTH_OF_IV = 16;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private static final int MAX_FILE_SIZE = 1024 * 1024 * 1024; // Dont read files greater than 1 gigabyte.

    private Date dateForBackupName = null;

    private DateFormat dateFormat;
    private MultiBitWalletProtobufSerializer walletProtobufSerializer;
    
    public static final byte[] ENCRYPTED_FILE_FORMAT_MAGIC_BYTES = new byte[]{(byte) 0x6D, (byte) 0x65, (byte) 0x6E, (byte) 0x64, (byte) 0x6F, (byte) 0x7A, (byte) 0x61}; // mendoza in ASCII
    

    // Nonsense bytes to fill up deleted files - these have no meaning.
    private static byte[] NONSENSE_BYTES = new byte[] { (byte) 0xF0, (byte) 0xA6, (byte) 0x55, (byte) 0xAA, (byte) 0x33,
            (byte) 0x77, (byte) 0x33, (byte) 0x37, (byte) 0x12, (byte) 0x34, (byte) 0x56, (byte) 0x78, (byte) 0xC2, (byte) 0xB3,
            (byte) 0xA4, (byte) 0x9A, (byte) 0x30, (byte) 0x7F, (byte) 0xE5, (byte) 0x5A, (byte) 0x23, (byte) 0x47, (byte) 0x13,
            (byte) 0x17, (byte) 0x15, (byte) 0x32, (byte) 0x5C, (byte) 0x77, (byte) 0xC9, (byte) 0x73, (byte) 0x04, (byte) 0x2D,
            (byte) 0x40, (byte) 0x0F, (byte) 0xA5, (byte) 0xA6, (byte) 0x43, (byte) 0x77, (byte) 0x33, (byte) 0x3B, (byte) 0x62,
            (byte) 0x34, (byte) 0xB6, (byte) 0x72, (byte) 0x32, (byte) 0xB3, (byte) 0xA4, (byte) 0x4B, (byte) 0x80, (byte) 0x7F,
            (byte) 0xC5, (byte) 0x43, (byte) 0x23, (byte) 0x47, (byte) 0x13, (byte) 0xB7, (byte) 0xA5, (byte) 0x32, (byte) 0xDC,
            (byte) 0x79, (byte) 0x19, (byte) 0xB1, (byte) 0x03, (byte) 0x9D };

    private static int BULKING_UP_FACTOR = 16;
    private static byte[] SECURE_DELETE_FILL_BYTES = new byte[NONSENSE_BYTES.length * BULKING_UP_FACTOR];

    static {
        // Make some SECURE_DELETE_FILL_BYTES bytes = x BULKING_UP_FACTOR the
        // NONSENSE just to save write time.
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

    /**
     * Load up a WalletData from a specified wallet file.
     * 
     * @param walletFile
     * @return WalletData - the walletData for the created wallet
     * @throws WalletLoadException
     * @throws WalletVersionException
     */
    public WalletData loadFromFile(File walletFile) throws WalletLoadException, WalletVersionException {
        if (walletFile == null) {
            return null;
        }

        String walletFilenameToUse = walletFile.getAbsolutePath();

        try {
            // See if the wallet is serialized or protobuf.
            WalletInfoData walletInfo;

            if (isWalletSerialised(walletFile)) {
                // Serialised wallets are no longer supported.
                throw new WalletLoadException("Could not load wallet '" + walletFilenameToUse
                        + "'. Serialized wallets are no longer supported.");
            } else {
                walletInfo = new WalletInfoData(walletFilenameToUse, null, MultiBitWalletVersion.PROTOBUF_ENCRYPTED);
            }

            // If the wallet file is missing or empty but the backup file exists
            // load that instead.
            // This indicates that the write was interrupted (e.g. power loss).
            boolean saveImmediately = false;
            if (!walletFile.exists() || walletFile.length() == 0) {
                String walletBackupFilename = walletInfo.getProperty(BitcoinModel.WALLET_BACKUP_FILE);
                if (walletBackupFilename != null && !"".equals(walletBackupFilename)) {
                    File walletBackupFile = new File(walletBackupFilename);
                    if (walletBackupFile.exists()) {
                        // Use the walletBackup.
                        log.debug("The wallet file '" + walletFile.getAbsolutePath()
                                + "' is empty so using the wallet backup file of '" + walletBackupFile.getAbsolutePath() + "'.");
                        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString(
                                "fileHandler.walletIsMissing",
                                new Object[] { walletFile.getAbsolutePath(), walletBackupFile.getAbsolutePath() })));
                        walletFile = walletBackupFile;

                        saveImmediately = true;

                        // Wipe the wallet backup property so that the backup
                        // file will not be overwritten
                        walletInfo.put(BitcoinModel.WALLET_BACKUP_FILE, "");
                    }
                }
            }

            FileInputStream fileInputStream = new FileInputStream(walletFile);
            InputStream stream = null;
            Wallet wallet = null;
            try {
                stream = new BufferedInputStream(fileInputStream);

                // serialised.1 wallets are stored as serialised (no longer
                // supported).
                // protobuf.2 (unencrypted) wallets stored as Wallet messages
                // protobuf.3 (encrypted) wallets are stored as Wallet messages
                // with a mandatory extension
                wallet = Wallet.loadFromFileStream(stream);

                if (walletInfo != null) {
                    // If wallet description is only in the wallet, copy it to
                    // the wallet info
                    // (perhaps the user deleted/ did not copy the info file).
                    String walletDescriptionInInfo = walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
                    if ((walletDescriptionInInfo == null || walletDescriptionInInfo.length() == 0)
                            && wallet.getDescription() != null) {
                        walletInfo.put(WalletInfoData.DESCRIPTION_PROPERTY, wallet.getDescription());
                    }

                    // Check that only receiving addresses that appear in a key
                    // appear in the wallet info.
                    walletInfo.checkAllReceivingAddressesAppearInWallet(wallet);
                }

                // Ensure that the directories for the backups of the private
                // keys, rolling backups and regular backups exist.
                createBackupDirectories(walletFile);
            } finally {
                if (stream != null) {
                    stream.close();
                }
                fileInputStream.close();
            }

            // Add the new wallet into the model.
            wallet.setNetworkParameters(this.bitcoinController.getModel().getNetworkParameters());
            WalletData perWalletModelData = this.bitcoinController.getModel().addWallet(this.bitcoinController, wallet,
                    walletFilenameToUse);

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
        // Exception exception = new IllegalStateException();
        // log.debug("Saving wallet '" + perWalletModelData.getWalletFilename()
        // + "' from stack:");
        // if (exception != null && exception.getStackTrace() != null) {
        // for (int i = 0; i < exception.getStackTrace().length; i++) {
        // log.debug("    " + exception.getStackTrace()[i].getFileName() + " - "
        // + exception.getStackTrace()[i].getMethodName() + ";" +
        // exception.getStackTrace()[i].getLineNumber());
        // }
        // }
        File walletFile = new File(perWalletModelData.getWalletFilename());

        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();

        if (walletInfo != null) {
            synchronized (walletInfo) {
                // Save the perWalletModelData if it is dirty or if forceWrite
                // is true.
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
                        backupPerWalletModelData(perWalletModelData);
                    }
                }
            }
        }
        return;
    }

    /**
     * Backup the perWalletModelData to the <wallet>-data/wallet-backup (encrypted) or wallet-unenc-backup (unencrypted) directories.
     * 
     * @param perWalletModelData
     */
    public void backupPerWalletModelData(WalletData perWalletModelData) {
        // Write to backup files.

        try {
            String backupSuffixText;
            if (perWalletModelData.getWalletInfo().getWalletVersion() == MultiBitWalletVersion.PROTOBUF) {
                backupSuffixText = FileHandler.UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
            } else {
                backupSuffixText = FileHandler.ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
            }
            String walletBackupFilename = createBackupFilename(new File(perWalletModelData.getWalletFilename()), backupSuffixText, true, false, BitcoinModel.WALLET_FILE_EXTENSION);
            perWalletModelData.setWalletBackupFilename(walletBackupFilename);

            String walletInfoBackupFilename = walletBackupFilename.replaceAll(BitcoinModel.WALLET_FILE_EXTENSION + "$", INFO_FILE_SUFFIX_STRING);
            perWalletModelData.setWalletInfoBackupFilename(walletInfoBackupFilename);

            saveWalletAndWalletInfoSimple(perWalletModelData, walletBackupFilename, walletInfoBackupFilename);

            log.info("Written backup wallet files to '" + walletBackupFilename + "', '" + walletInfoBackupFilename + "'");
        } catch (IOException ioe) {
            log.error(ioe.getClass().getCanonicalName() + " " + ioe.getMessage());
            throw new WalletSaveException("Cannot backup wallet '" + perWalletModelData.getWalletFilename(), ioe);
        }
    }

    /**
     * To protect the wallet data, the write is in steps: 1) Create a backup
     * file called <wallet file name>-<yyyymmddhhmmss>.wallet and copy the
     * original wallet to that 2) Write the new wallet to the walletFilename 3)
     * Delete the old backup file 4) Make the backup file in step 1) the new
     * backup file
     * 
     **/
    private void saveWalletAndWalletInfo(WalletData perWalletModelData, String walletFilename, String walletInfoFilename) {
        File walletFile = new File(walletFilename);
        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();

        FileOutputStream fileOutputStream = null;

        // Save the wallet file
        try {
            if (perWalletModelData.getWallet() != null) {
                // Wallet description is currently stored in the wallet info
                // file but is now available on the wallet itself.
                // Store the description from the wallet info in the wallet - in
                // the future the wallet value will be primary
                // and wallet infos can be deprecated.
                // TODO - migrate completely to use wallet description and then
                // deprecate value in info file.
                if (walletInfo != null) {
                    String walletDescriptionInInfoFile = walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
                    if (walletDescriptionInInfoFile != null) {
                        perWalletModelData.getWallet().setDescription(walletDescriptionInInfoFile);
                    }
                }
                String oldBackupFilename = perWalletModelData.getWalletInfo().getProperty(BitcoinModel.WALLET_BACKUP_FILE);
                File oldBackupFile = null;
                String newBackupFilename = null;
                if (null != oldBackupFilename && !"".equals(oldBackupFilename)) {
                    oldBackupFile = new File(oldBackupFilename);
                }
                if (MultiBitWalletVersion.PROTOBUF == walletInfo.getWalletVersion()
                        || MultiBitWalletVersion.PROTOBUF_ENCRYPTED == walletInfo.getWalletVersion()) {
                    newBackupFilename = copyExistingWalletToBackupAndDeleteOriginal(walletFile);
                }

                log.debug("Saving wallet file '" + walletFile.getAbsolutePath() + "' ...");
                if (MultiBitWalletVersion.SERIALIZED == walletInfo.getWalletVersion()) {
                    throw new WalletSaveException("Cannot save wallet '" + walletFile.getAbsolutePath() + "'. Serialized wallets are no longer supported.");
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

                if (MultiBitWalletVersion.PROTOBUF == walletInfo.getWalletVersion()
                        || MultiBitWalletVersion.PROTOBUF_ENCRYPTED == walletInfo.getWalletVersion()) {
                    perWalletModelData.getWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, newBackupFilename);

                    // Delete the oldBackupFile unless the user has manually
                    // opened it.
                    boolean userHasOpenedBackupFile = false;
                    List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();
                    if (perWalletModelDataList != null) {
                        for (WalletData perWalletModelDataLoop : perWalletModelDataList) {
                            if ((oldBackupFilename != null && oldBackupFilename.equals(perWalletModelDataLoop.getWalletFilename()))
                                    || (newBackupFilename != null && newBackupFilename.equals(perWalletModelDataLoop
                                            .getWalletFilename()))) {
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
     * Simply save the wallet and wallet info files.
     * Used for backup writes.
     * 
     * @param perWalletModelData
     * @param walletFilename
     * @param walletInfoFilename
     */
    private void saveWalletAndWalletInfoSimple(WalletData perWalletModelData, String walletFilename, String walletInfoFilename) {
        File walletFile = new File(walletFilename);
        WalletInfoData walletInfo = perWalletModelData.getWalletInfo();

        FileOutputStream fileOutputStream = null;

        // Save the wallet file
        try {
            if (perWalletModelData.getWallet() != null) {
                // Wallet description is currently stored in the wallet info
                // file but is now available on the wallet itself.
                // Store the description from the wallet info in the wallet - in
                // the future the wallet value will be primary
                // and wallet infos can be deprecated.
                // TODO - migrate completely to use wallet description and then
                // deprecate value in info file.
                if (walletInfo != null) {
                    String walletDescriptionInInfoFile = walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
                    if (walletDescriptionInInfoFile != null) {
                        perWalletModelData.getWallet().setDescription(walletDescriptionInInfoFile);
                    }
                }

                log.debug("Saving wallet file '" + walletFile.getAbsolutePath() + "' ...");
                if (MultiBitWalletVersion.SERIALIZED == walletInfo.getWalletVersion()) {
                    throw new WalletSaveException("Cannot save wallet '" + walletFile.getAbsolutePath() + "'. Serialized wallets are no longer supported.");
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
     * Backup the private keys of the active wallet to a file with name <wallet-name>-data/key-backup/<wallet
     * name>-yyyymmddhhmmss.key
     * 
     * @param passwordToUse
     * @return File to which keys were backed up, or null if they were not.
     * @throws KeyCrypterException
     */
    public File backupPrivateKeys(CharSequence passwordToUse) throws IOException, KeyCrypterException {
        File privateKeysBackupFile = null;

        // Only encrypted files are backed up, and they must have a non blank password.
        if (passwordToUse != null && passwordToUse.length() > 0) {
            if (controller.getModel() != null
                    && this.bitcoinController.getModel().getActiveWalletWalletInfo() != null
                    && this.bitcoinController.getModel().getActiveWalletWalletInfo().getWalletVersion() == MultiBitWalletVersion.PROTOBUF_ENCRYPTED) {
                // Save a backup copy of the private keys, encrypted with the passwordToUse.
                PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(this.bitcoinController.getModel()
                        .getNetworkParameters());
                String privateKeysBackupFilename = createBackupFilename(new File(this.bitcoinController.getModel()
                        .getActiveWalletFilename()), PRIVATE_KEY_BACKUP_DIRECTORY_NAME, false, false, BitcoinModel.PRIVATE_KEY_FILE_EXTENSION);
                privateKeysBackupFile = new File(privateKeysBackupFilename);
                BlockChain blockChain = null;
                if (this.bitcoinController.getMultiBitService() != null) {
                    blockChain = this.bitcoinController.getMultiBitService().getChain();
                }

                privateKeysHandler.exportPrivateKeys(privateKeysBackupFile, this.bitcoinController.getModel().getActiveWallet(),
                        blockChain, true, passwordToUse, passwordToUse);
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

    /**
     * Copy an existing wallet to a backup file and delete the original.
     * Used in rolling backups
     * 
     * @param walletFile
     * @return
     * @throws IOException
     */
    private String copyExistingWalletToBackupAndDeleteOriginal(File walletFile) throws IOException {
        String newWalletBackupFilename = createBackupFilename(walletFile, ROLLING_WALLET_BACKUP_DIRECTORY_NAME, false, false, BitcoinModel.WALLET_FILE_EXTENSION);
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

                if (!walletFileSize.equals(walletInfo.getProperty(BitcoinModel.WALLET_FILE_SIZE))) {
                    haveFilesChanged = true;
                }

                if (!walletFileLastModified.equals(walletInfo.getProperty(BitcoinModel.WALLET_FILE_LAST_MODIFIED))) {
                    haveFilesChanged = true;
                }

                if (!walletInfoFileSize.equals(walletInfo.getProperty(BitcoinModel.WALLET_INFO_FILE_SIZE))) {
                    haveFilesChanged = true;
                }

                if (haveFilesChanged) {
                    log.debug("Result of check of whether files have changed for wallet filename "
                            + perWalletModelData.getWalletFilename() + " was " + haveFilesChanged + ".");
                    log.debug(BitcoinModel.WALLET_FILE_SIZE + " " + walletFileSize + " ," + BitcoinModel.WALLET_FILE_LAST_MODIFIED
                            + " " + walletFileLastModified + " ," + BitcoinModel.WALLET_INFO_FILE_SIZE + " " + walletInfoFileSize
                            + " ," + BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED + " " + walletInfoFileLastModified);
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
        // Get the files' last modified data and sizes and store them in the
        // wallet properties.
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
                if ("".equals(perWalletModelData.getWalletFilename()) || perWalletModelData.getWalletFilename() == null
                        || perWalletModelData.getWalletInfo() == null) {
                    continue;
                }

                // Do not save deleted wallets
                if (perWalletModelData.getWalletInfo().isDeleted()) {
                    log.debug("Not writing out info about wallet '" + perWalletModelData.getWalletFilename()
                            + "' as it has been deleted");
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
                controller.getModel().setUserPreference(BitcoinModel.PROTOBUF3_WALLET_FILENAME_PREFIX + protobuf3Count,
                        walletFilename);
                protobuf3Count++;
            }
            controller.getModel().setUserPreference(BitcoinModel.NUMBER_OF_PROTOBUF3_WALLETS, "" + protobuf3List.size());
            controller.getModel().setUserPreference(BitcoinModel.ACTIVE_WALLET_FILENAME,
                    bitcoinController.getModel().getActiveWalletFilename());
        }

        Properties userPreferences = controller.getModel().getAllUserPreferences();

        // If the view is marked as also requiring a backwards compatible
        // numeric field write that.
        @SuppressWarnings("deprecation")
        int currentViewNumericFormat = View.toOldViewNumeric(controller.getCurrentView());
        if (currentViewNumericFormat != 0) {
            userPreferences.put(CoreModel.SELECTED_VIEW, "" + currentViewNumericFormat);
        } else {
            // Make sure the old numeric value for a view is not in the user
            // properties.
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
     * Create a backup filename the format is: original file: filename.suffix.
     * backup file: 
     * (without subDirectorySuffix) filename-yyyymmddhhmmss.suffix
     * (with subDirectorySuffix) filename-data/subDirectorySuffix/filename-yyyymmddhhmmss.suffix
     * 
     *  (Any intermediate directories are automatically created if necessary)
     *
     * @param file
     * @param subDirectorySuffix - subdirectory to add to backup file e.g key-backup. null for no subdirectory.
     * @param saveBackupDate - save the backup date for use later
     * @param reusePreviousBackupDate
     *            Reuse the previously created backup date so that wallet and wallet info names match
     * @param suffixToUse
     *            the suffix text to use
     * @return String the name of the created filename.
     * @throws IOException
     */
    public String createBackupFilename(File file, String subDirectorySuffix, boolean saveBackupDate, boolean reusePreviousBackupDate, String suffixToUse)
            throws IOException {
        String filenameLong = file.getAbsolutePath(); // Full path.
        String filenameShort = file.getName(); // Just the filename.
        
        String topLevelBackupDirectoryName = calculateTopLevelBackupDirectoryName(file);
        createDirectoryIfNecessary(topLevelBackupDirectoryName);

        // Find suffix and stems of filename.
        int suffixSeparatorLong = filenameLong.lastIndexOf(".");
        String stemLong = filenameLong.substring(0, suffixSeparatorLong);

        int suffixSeparatorShort= filenameShort.lastIndexOf(".");
        String stemShort = filenameShort.substring(0, suffixSeparatorShort);

        String suffix;
        if (suffixToUse != null) {
            suffix = "." + suffixToUse;
        } else {
            suffix = filenameLong.substring(suffixSeparatorLong); // Includes separating dot.
        }
        
        Date backupDateToUse = new Date();

        if (saveBackupDate) {
            dateForBackupName = backupDateToUse;
        }

        if (reusePreviousBackupDate) {
            backupDateToUse = dateForBackupName;
        }
        String backupFilename;
        
        if (subDirectorySuffix != null && subDirectorySuffix.length() > 0) {
            String backupFilenameShort = stemShort + SEPARATOR + dateFormat.format(backupDateToUse) + suffix;
            String subDirectoryName =  topLevelBackupDirectoryName + File.separator + subDirectorySuffix;
            createDirectoryIfNecessary(subDirectoryName);
            backupFilename = subDirectoryName + File.separator + backupFilenameShort;
        } else {
            backupFilename = stemLong + SEPARATOR + dateFormat.format(backupDateToUse) + suffix;
        }

        return backupFilename;
    }
    /**
     * To support multiple users on the same machine, the checkpoints file is
     * installed into the program installation directory and is then copied to
     * the user's application data directory when MultiBit is first used.
     * 
     * Thus each user has their own copy of the blockchain.
     */
    public void copyCheckpointsFromInstallationDirectory(String destinationCheckpointsFilename) throws IOException {
        if (destinationCheckpointsFilename == null) {
            return;
        }

        // See if the block chain in the user's application data directory
        // exists.
        File destinationCheckpoints = new File(destinationCheckpointsFilename);

        if (!destinationCheckpoints.exists()) {
            // Work out the source checkpoints (put into the program
            // installation directory by the installer).
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
                            + destinationCheckpointsFilename + "' is of length " + destinationLength;
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
                            + destinationBlockChainFilename + "' is of length " + destinationLength;
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
    
    public void fileLevelEncryptUnencryptedWalletBackups(WalletData perWalletModelData, CharSequence passwordToUse) {
        // See if there are any unencrpyted wallet backups.
        String topLevelBackupDirectoryName = calculateTopLevelBackupDirectoryName(new File(perWalletModelData.getWalletFilename()));
        String unencryptedWalletBackupDirectoryName = topLevelBackupDirectoryName + File.separator
                + UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
        File unencryptedWalletBackupDirectory = new File(unencryptedWalletBackupDirectoryName);

        File[] listOfFiles = unencryptedWalletBackupDirectory.listFiles();

        Collection<File> unencryptedWalletBackups = new ArrayList<File>();
        // Look for filenames with format "text"-YYYYMMDDHHMMSS.wallet<eol>.
        if (listOfFiles != null) {
            for (int i = 0; i < listOfFiles.length; i++) {
                if (listOfFiles[i].isFile()) {
                    if (listOfFiles[i].getName().matches(".*-\\d{14}\\.wallet$")) {
                        unencryptedWalletBackups.add(listOfFiles[i]);
                    }
                } 
            }
            
            // Copy and encrypt each file and secure delete the original.
            for(File loopFile : unencryptedWalletBackups) {
                try {
                    String encryptedFilename = loopFile.getAbsolutePath() + "." + FILE_ENCRYPTED_WALLET_SUFFIX;
                    copyFileAndEncrypt(loopFile, new File(encryptedFilename), passwordToUse);
                    secureDelete(loopFile);
                } catch (IOException ioe) {
                    log.error(ioe.getClass().getName() + " " + ioe.getMessage());
                } catch (IllegalArgumentException iae) {
                    log.error(iae.getClass().getName() + " " + iae.getMessage());
                } catch (IllegalStateException ise) {
                    log.error(ise.getClass().getName() + " " + ise.getMessage());
                }catch (KeyCrypterException kce) {
                    log.error(kce.getClass().getName() + " " + kce.getMessage());
                }
            }
        }
    }

    public static void copyFileAndEncrypt(File sourceFile, File destinationFile, CharSequence passwordToUse) throws IOException {
        if (passwordToUse == null || passwordToUse.length() == 0) {
            throw new IllegalArgumentException("Password cannot be blank");
        }
       
        if (destinationFile.exists()) {
            throw new IllegalArgumentException("The destination file '" + destinationFile.getAbsolutePath() + "' already exists.");            
        }
        
        // Read in the source file.
        byte[] sourceFileUnencrypted = read(sourceFile);
        
        // Create the destination file.
        if (!destinationFile.exists()) {
            destinationFile.createNewFile();
        }
        
        // Encrypt the data.
        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        System.out.println(Utils.bytesToHexString(salt));
        
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder()
        .setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();
        KeyCrypterScrypt keyCrypter = new KeyCrypterScrypt(scryptParameters);
        EncryptedPrivateKey encryptedData = keyCrypter.encrypt(sourceFileUnencrypted, keyCrypter.deriveKey(passwordToUse));
        
        // The format of the encrypted data is:
        // 7 magic bytes 'mendoza' in ASCII.
        // 1 byte version number of format - initially set to 0
        // 8 bytes salt
        // 16 bytes iv
        // rest of file is the encrypted byte data
        
        FileOutputStream fileOutputStream = null;
        try {
            fileOutputStream = new FileOutputStream(destinationFile);
            fileOutputStream.write(ENCRYPTED_FILE_FORMAT_MAGIC_BYTES);
            
            // file format version.
            fileOutputStream.write((byte) 0x00);
            
            fileOutputStream.write(salt); // 8 bytes.
            fileOutputStream.write(encryptedData.getInitialisationVector()); // 16 bytes.
            System.out.println(Utils.bytesToHexString(encryptedData.getInitialisationVector()));
            
            fileOutputStream.write(encryptedData.getEncryptedBytes());
            System.out.println(Utils.bytesToHexString(encryptedData.getEncryptedBytes()));
        } finally {
            if (fileOutputStream != null) {
                fileOutputStream.flush();
                fileOutputStream.close();
            }
        }
        
        // Read in the file again and decrypt it to make sure everything was ok.
        byte[] phoenix = readFileAndDecrypt(destinationFile, passwordToUse);
        
        if (!Arrays.areEqual(sourceFileUnencrypted, phoenix)) {
            throw new IOException("File '" + sourceFile.getAbsolutePath() + "' was not correctly encrypted to file '" + destinationFile.getAbsolutePath());
        }
    }

    public static byte[] readFileAndDecrypt(File encryptedFile, CharSequence passwordToUse) throws IOException {
        // Read in the encrypted file.
        
        byte[] sourceFileEncrypted = read(encryptedFile);
        
        // Check the first bytes match the magic number.
        if (!Arrays.areEqual(ENCRYPTED_FILE_FORMAT_MAGIC_BYTES, Arrays.copyOfRange(sourceFileEncrypted, 0, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length))) {
            throw new IOException("File '" + encryptedFile.getAbsolutePath() + "' did not start with the correct magic bytes.");            
        }
        
        // Check the format version.
        String versionNumber = "" + sourceFileEncrypted[ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length];
        if (!("0".equals(versionNumber))) {
            throw new IOException("File '" + encryptedFile.getAbsolutePath() + "' did not have the expected version number of 0. It was " + versionNumber);            
        }

        // Extract the salt.
        byte[] salt = Arrays.copyOfRange(sourceFileEncrypted, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH);
        System.out.println(Utils.bytesToHexString(salt));
        
        // Extract the IV.
        byte[] iv = Arrays.copyOfRange(sourceFileEncrypted, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH , ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH + KeyCrypterScrypt.BLOCK_LENGTH);
        System.out.println(Utils.bytesToHexString(iv));
        
        // Extract the encrypted bytes.
        byte[] encryptedBytes = Arrays.copyOfRange(sourceFileEncrypted, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH + KeyCrypterScrypt.BLOCK_LENGTH , sourceFileEncrypted.length);
        System.out.println(Utils.bytesToHexString(encryptedBytes));
         
        // Decrypt the data.
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();
        KeyCrypter keyCrypter = new KeyCrypterScrypt(scryptParameters);
        EncryptedPrivateKey encryptedPrivateKey = new EncryptedPrivateKey(iv, encryptedBytes);
        return keyCrypter.decrypt(encryptedPrivateKey, keyCrypter.deriveKey(passwordToUse));
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
     * Set bit patterns are used rather than random numbers to avoid a
     * futex_wait_queue_me error on Linux systems (related to /dev/random usage)
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
                // Make sure it is writable and any references to it are garbage
                // collected and finalized.
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

    public void createBackupDirectories(File walletFile) {
        if (walletFile == null) {
            return;
        }

        // Create the top-level directory for the wallet specific data, if necessary.
        String topLevelBackupDirectoryName = calculateTopLevelBackupDirectoryName(walletFile);
        createDirectoryIfNecessary(topLevelBackupDirectoryName);

        // Create the backup directories for the private keys, rolling backup and wallets.
        String privateKeysBackupDirectoryName = topLevelBackupDirectoryName + File.separator + PRIVATE_KEY_BACKUP_DIRECTORY_NAME;
        createDirectoryIfNecessary(privateKeysBackupDirectoryName);

        String rollingWalletBackupDirectoryName = topLevelBackupDirectoryName + File.separator
                + ROLLING_WALLET_BACKUP_DIRECTORY_NAME;
        createDirectoryIfNecessary(rollingWalletBackupDirectoryName);

        String unencryptedWalletBackupDirectoryName = topLevelBackupDirectoryName + File.separator
                + UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
        createDirectoryIfNecessary(unencryptedWalletBackupDirectoryName);

        String encryptedWalletBackupDirectoryName = topLevelBackupDirectoryName + File.separator
                + ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
        createDirectoryIfNecessary(encryptedWalletBackupDirectoryName);
    }
 
    private String calculateTopLevelBackupDirectoryName(File walletFile) {
        // Work out the name of the top level wallet backup directory.
        String walletPath = walletFile.getAbsolutePath();
        // Remove any trailing ".wallet" or .info text.
        String walletSuffixSearchText = "." + BitcoinModel.WALLET_FILE_EXTENSION;
        if (walletPath.endsWith(walletSuffixSearchText)) {
            walletPath = walletPath.substring(0, walletPath.length() - walletSuffixSearchText.length());
        }

        walletSuffixSearchText = "." + INFO_FILE_SUFFIX_STRING;
        if (walletPath.endsWith(walletSuffixSearchText)) {
            walletPath = walletPath.substring(0, walletPath.length() - walletSuffixSearchText.length());
        }

        // Create the top-level directory for the wallet specific data
        return walletPath + TOP_LEVEL_WALLET_BACKUP_SUFFIX;
    }

    private void createDirectoryIfNecessary(String directoryName) {
        File directory = new File(directoryName);
        if (!directory.exists()) {
            boolean createSuccess = directory.mkdir();
            log.debug("Result of create of directory + '" + directoryName + "' was " + createSuccess);
        }
    }
    
    public static byte[] read(File file) throws IOException {
        if (file == null) {
            throw new IllegalArgumentException("File must be provided");
        }
        
        if ( file.length() > MAX_FILE_SIZE ) {
            throw new IOException("File '" + file.getAbsolutePath() + "' is too large to input");
        }

        byte []buffer = new byte[(int) file.length()];
        InputStream ios = null;
        try {
            ios = new FileInputStream(file);
            if ( ios.read(buffer) == -1 ) {
                throw new IOException("EOF reached while trying to read the whole file");
            }        
        } finally { 
            try {
                 if ( ios != null )  {
                      ios.close();
                 }
            } catch ( IOException e) {
                log.error(e.getClass().getName() + " " + e.getMessage());
            }
        }

        return buffer;
    }
}
