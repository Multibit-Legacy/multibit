package org.multibit.file;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.SecureRandom;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.util.Arrays;

import com.google.dogecoin.core.Utils;
import com.google.dogecoin.core.Wallet;
import com.google.dogecoin.crypto.EncryptedPrivateKey;
import com.google.dogecoin.crypto.KeyCrypter;
import com.google.dogecoin.crypto.KeyCrypterException;
import com.google.dogecoin.crypto.KeyCrypterScrypt;
import com.google.protobuf.ByteString;


/**
 * Class to manage creation and reading back of the wallet backups.
 */
public enum BackupManager {
    INSTANCE;
   
    private static final Logger log = LoggerFactory.getLogger(BackupManager.class);

    transient private static SecureRandom secureRandom = new SecureRandom();

    public static final String BACKUP_SUFFIX_FORMAT = "yyyyMMddHHmmss";
    private static final String SEPARATOR = "-";
    private DateFormat dateFormat;
    private Date dateForBackupName = null;

    public static final String TOP_LEVEL_WALLET_BACKUP_SUFFIX = "-data";
    public static final String PRIVATE_KEY_BACKUP_DIRECTORY_NAME = "key-backup";
    public static final String ROLLING_WALLET_BACKUP_DIRECTORY_NAME = "rolling-backup";
    public static final String ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME = "wallet-backup";
    public static final String UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME = "wallet-unenc-backup";

    public static final int MAXIMUM_NUMBER_OF_BACKUPS = 60; // Chosen so that you will have about weekly backups for a year, fortnightly over two years.
    public static final int NUMBER_OF_FIRST_WALLETS_TO_ALWAYS_KEEP = 2; 
    public static final int NUMBER_OF_LAST_WALLETS_TO_ALWAYS_KEEP = 8; // Must be at least 1.
    
    
    public static final String REGEX_FOR_WALLET_SUFFIX = ".*\\.wallet$";
    public static final String REGEX_FOR_TIMESTAMP_AND_KEY_SUFFIX = ".*-\\d{" + BACKUP_SUFFIX_FORMAT.length() + "}\\.key$";
    public static final String REGEX_FOR_TIMESTAMP_AND_WALLET_SUFFIX = ".*-\\d{" + BACKUP_SUFFIX_FORMAT.length() + "}\\.wallet$";
    public static final String REGEX_FOR_TIMESTAMP_AND_INFO_SUFFIX = ".*-\\d{" + BACKUP_SUFFIX_FORMAT.length() + "}\\.info$";
    public static final String REGEX_FOR_TIMESTAMP_AND_WALLET_AND_CIPHER_SUFFIX = ".*-\\d{" + BACKUP_SUFFIX_FORMAT.length() + "}\\.wallet\\.cipher$";
    public static final int EXPECTED_LENGTH_OF_SALT = 8;
    public static final int EXPECTED_LENGTH_OF_IV = 16;
    
    public static final String INFO_FILE_SUFFIX_STRING = "info";
    public static final String FILE_ENCRYPTED_WALLET_SUFFIX = "cipher";
    public static final byte FILE_ENCRYPTED_VERSION_NUMBER = (byte) 0x00;

    public static final byte[] ENCRYPTED_FILE_FORMAT_MAGIC_BYTES = new byte[]{(byte) 0x6D, (byte) 0x65, (byte) 0x6E, (byte) 0x64, (byte) 0x6F, (byte) 0x7A, (byte) 0x61}; // mendoza in ASCII
     
    /**
     * Backup the perWalletModelData to the <wallet>-data/wallet-backup (encrypted) or wallet-unenc-backup (unencrypted) directories.
     * 
     * @param perWalletModelData
     */
    public void backupPerWalletModelData(FileHandler fileHandler, WalletData perWalletModelData) {
        if (perWalletModelData == null || fileHandler == null) {
            return;
        }
        
        // Write to backup files.
        try {
            String backupSuffixText;
            if (EncryptionType.UNENCRYPTED == perWalletModelData.getWallet().getEncryptionType()) {
                backupSuffixText = UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
            } else {
                backupSuffixText = ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME;
            }
            String walletBackupFilename = createBackupFilename(new File(perWalletModelData.getWalletFilename()), backupSuffixText, true, false, BitcoinModel.WALLET_FILE_EXTENSION);
            perWalletModelData.setWalletBackupFilename(walletBackupFilename);

            String walletInfoBackupFilename = walletBackupFilename.replaceAll(BitcoinModel.WALLET_FILE_EXTENSION + "$", INFO_FILE_SUFFIX_STRING);
            perWalletModelData.setWalletInfoBackupFilename(walletInfoBackupFilename);

            // If the backup directory is needs thinning, do so.
            thinBackupDirectory(perWalletModelData.getWalletFilename(), backupSuffixText);
            
            fileHandler.saveWalletAndWalletInfoSimple(perWalletModelData, walletBackupFilename, walletInfoBackupFilename);

            log.info("Written backup wallet files to '" + walletBackupFilename + "', '" + walletInfoBackupFilename + "'");
        } catch (IOException ioe) {
            log.error(ioe.getClass().getCanonicalName() + " " + ioe.getMessage());
            throw new WalletSaveException("Cannot backup wallet '" + perWalletModelData.getWalletFilename(), ioe);
        } catch (Exception e) {
            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
            throw new WalletSaveException("Cannot backup wallet '" + perWalletModelData.getWalletFilename(), e);
        }
    }
    
    public void fileLevelEncryptUnencryptedWalletBackups(WalletData perWalletModelData, CharSequence passwordToUse) {
        // See if there are any unencrypted wallet backups.
        Collection<File> unencryptedWalletBackups = getWalletsInBackupDirectory(perWalletModelData.getWalletFilename(),
                UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME);

        // Copy and encrypt each file and secure delete the original.
        for (File loopFile : unencryptedWalletBackups) {
            try {
                String encryptedFilename = loopFile.getAbsolutePath() + "." + FILE_ENCRYPTED_WALLET_SUFFIX;
                copyFileAndEncrypt(loopFile, new File(encryptedFilename), passwordToUse);
                FileHandler.secureDelete(loopFile);
            } catch (IOException ioe) {
                log.error(ioe.getClass().getName() + " " + ioe.getMessage());
            } catch (IllegalArgumentException iae) {
                log.error(iae.getClass().getName() + " " + iae.getMessage());
            } catch (IllegalStateException ise) {
                log.error(ise.getClass().getName() + " " + ise.getMessage());
            } catch (KeyCrypterException kce) {
                log.error(kce.getClass().getName() + " " + kce.getMessage());
            }
        }
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
    String createBackupFilename(File file, String subDirectorySuffix, boolean saveBackupDate, boolean reusePreviousBackupDate, String suffixToUse)
            throws IOException {
        String filenameLong = file.getAbsolutePath(); // Full path.
        String filenameShort = file.getName(); // Just the filename.
        
        String topLevelBackupDirectoryName = calculateTopLevelBackupDirectoryName(file);
        createDirectoryIfNecessary(topLevelBackupDirectoryName);

        // Find suffix and stems of filename.
        int suffixSeparatorLong = filenameLong.lastIndexOf('.');
        String stemLong = filenameLong.substring(0, suffixSeparatorLong);

        int suffixSeparatorShort= filenameShort.lastIndexOf('.');
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
        
        if (dateFormat == null) {
            dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        }
        
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
     * Thin the wallet backups when they reach the MAXIMUM_NUMBER_OF_BACKUPS setting.
     * Thinning is done by removing the most quickly replaced backup, except for the first and last few 
     * (as they are considered to be more valuable backups).
     * 
     * @param backupDirectoryName
     */
    void thinBackupDirectory(String walletFilename, String backupSuffixText) {
        if (dateFormat == null) {
            dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        }
        
        if (walletFilename == null || backupSuffixText == null) {
            return;
        }
        
        // Find out how many wallet backups there are.
        List<File> backupWallets = getWalletsInBackupDirectory(walletFilename, backupSuffixText);
        
        if (backupWallets.size() < MAXIMUM_NUMBER_OF_BACKUPS) {
            // No thinning required.
            return;
        }
        
        // Work out the date the backup was made for each of the wallet.
        // This is done using the timestamp rather than the write time of the file.
        Map<File, Date> mapOfFileToBackupTimes = new HashMap<File, Date>();
        for (int i = 0; i < backupWallets.size(); i++) {
            String filename = backupWallets.get(i).getName();
            if (filename.length() > 22) { // 22 = 1 for hyphen + 14 for timestamp + 1 for dot + 6 for wallet.
                int startOfTimestamp = filename.length() - 21; // 21 = 14 for timestamp + 1 for dot + 6 for wallet.
                String timestampText = filename.substring(startOfTimestamp, startOfTimestamp + BACKUP_SUFFIX_FORMAT.length() );
                try {
                    Date parsedTimestamp = dateFormat.parse(timestampText);
                    mapOfFileToBackupTimes.put(backupWallets.get(i), parsedTimestamp);
                } catch (ParseException pe) {
                    // Cannot parse text - may be some other type of file the user has put in the directory.
                    log.debug("For wallet '" + filename + " could not parse the timestamp of '" + timestampText + "'.");
                }
            }
        }

        // See which wallet is most quickly replaced by another backup - this will be thinned.
        int walletBackupToDeleteIndex = -1; // Not set yet.
        long walletBackupToDeleteReplacementTimeMillis = Integer.MAX_VALUE; // How quickly the wallet was replaced by a later one.
        
        for (int i = 0; i < backupWallets.size(); i++) {
            if ((i < NUMBER_OF_FIRST_WALLETS_TO_ALWAYS_KEEP)
                    || (i >= backupWallets.size() - NUMBER_OF_LAST_WALLETS_TO_ALWAYS_KEEP)) {
                // Keep the very first and last wallets always.
            } else {
                // If there is a data directory for the backup then it may have been opened
                // in MultiBit so we will skip considering it for deletion.
                String possibleDataDirectory = calculateTopLevelBackupDirectoryName(backupWallets.get(i));
                boolean theWalletHasADataDirectory = (new File(possibleDataDirectory)).exists();

                // Work out how quickly the wallet is replaced by the next backup.
                Date thisWalletTimestamp = mapOfFileToBackupTimes.get(backupWallets.get(i));
                Date nextWalletTimestamp = mapOfFileToBackupTimes.get(backupWallets.get(i + 1));
                if (thisWalletTimestamp != null && nextWalletTimestamp != null) {
                    long deltaTimeMillis = nextWalletTimestamp.getTime() - thisWalletTimestamp.getTime();
                    if (deltaTimeMillis < walletBackupToDeleteReplacementTimeMillis && !theWalletHasADataDirectory) {
                        // This is the best candidate for deletion so far.
                        walletBackupToDeleteIndex = i;
                        walletBackupToDeleteReplacementTimeMillis = deltaTimeMillis;
                    }
                }
            }
        }
        
        if (walletBackupToDeleteIndex > -1) {
           try {
                // Secure delete the chosen backup wallet and its info file if present.
                log.debug("To save space, secure deleting backup wallet '"
                        + backupWallets.get(walletBackupToDeleteIndex).getAbsolutePath() + "'.");
                FileHandler.secureDelete(backupWallets.get(walletBackupToDeleteIndex));

                String walletInfoBackupFilename = backupWallets.get(walletBackupToDeleteIndex).getAbsolutePath()
                        .replaceAll(BitcoinModel.WALLET_FILE_EXTENSION + "$", INFO_FILE_SUFFIX_STRING);
                File walletInfoBackup = new File(walletInfoBackupFilename);
                if (walletInfoBackup.exists()) {
                    log.debug("To save space, secure deleting backup info file '" + walletInfoBackup.getAbsolutePath() + "'.");
                    FileHandler.secureDelete(walletInfoBackup);
                }
            } catch (IOException ioe) {
                log.error(ioe.getClass().getName() + " " + ioe.getMessage());
            }
        }
    }
        
    void copyFileAndEncrypt(File sourceFile, File destinationFile, CharSequence passwordToUse) throws IOException {
        if (passwordToUse == null || passwordToUse.length() == 0) {
            throw new IllegalArgumentException("Password cannot be blank");
        }
       
        if (destinationFile.exists()) {
            throw new IllegalArgumentException("The destination file '" + destinationFile.getAbsolutePath() + "' already exists.");            
        } else {
            // Attempt to create it
            if (!destinationFile.createNewFile()) {
                throw new IllegalArgumentException("The destination file '" + destinationFile.getAbsolutePath() + "' could not be created. Check permissions.");
            }
        }
        
        // Read in the source file.
        byte[] sourceFileUnencrypted = FileHandler.read(sourceFile);

        // Encrypt the data.
        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        
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
            fileOutputStream.write(FILE_ENCRYPTED_VERSION_NUMBER);
            
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
    
    public byte[] readFileAndDecrypt(File encryptedFile, CharSequence passwordToUse) throws IOException {
        // Read in the encrypted file.
        byte[] sourceFileEncrypted = FileHandler.read(encryptedFile);
        
        // Check the first bytes match the magic number.
        if (!Arrays.areEqual(ENCRYPTED_FILE_FORMAT_MAGIC_BYTES, Arrays.copyOfRange(sourceFileEncrypted, 0, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length))) {
            throw new IOException("File '" + encryptedFile.getAbsolutePath() + "' did not start with the correct magic bytes.");            
        }
        
        // If the file is too short don't process it.
        if (sourceFileEncrypted.length < ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH + KeyCrypterScrypt.BLOCK_LENGTH) {
            throw new IOException("File '" + encryptedFile.getAbsolutePath() + "' is too short to decrypt. It is " + sourceFileEncrypted.length + " bytes long.");                        
        }
        
        // Check the format version.
        String versionNumber = "" + sourceFileEncrypted[ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length];
        //System.out.println("FileHandler - versionNumber = " + versionNumber);
        if (!("0".equals(versionNumber))) {
            throw new IOException("File '" + encryptedFile.getAbsolutePath() + "' did not have the expected version number of 0. It was " + versionNumber);            
        }

        // Extract the salt.
        byte[] salt = Arrays.copyOfRange(sourceFileEncrypted, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH);
        //System.out.println("FileHandler - salt = " + Utils.bytesToHexString(salt));
        
        // Extract the IV.
        byte[] iv = Arrays.copyOfRange(sourceFileEncrypted, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH , ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH + KeyCrypterScrypt.BLOCK_LENGTH);
        //System.out.println("FileHandler - iv = " + Utils.bytesToHexString(iv));
        
        // Extract the encrypted bytes.
        byte[] encryptedBytes = Arrays.copyOfRange(sourceFileEncrypted, ENCRYPTED_FILE_FORMAT_MAGIC_BYTES.length + 1 + KeyCrypterScrypt.SALT_LENGTH + KeyCrypterScrypt.BLOCK_LENGTH , sourceFileEncrypted.length);
        //System.out.println("FileHandler - encryptedBytes = " + Utils.bytesToHexString(encryptedBytes));
         
        // Decrypt the data.
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();
        KeyCrypter keyCrypter = new KeyCrypterScrypt(scryptParameters);
        EncryptedPrivateKey encryptedPrivateKey = new EncryptedPrivateKey(iv, encryptedBytes);
        return keyCrypter.decrypt(encryptedPrivateKey, keyCrypter.deriveKey(passwordToUse));
    }
    
    void createBackupDirectories(File walletFile) {
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
 
    /**
     * Work out the best wallet backups to try to load
     * @param walletFile
     * @return Collection<String> The best wallets to try to load, in order of goodness.
     */
    Collection<String> calculateBestWalletBackups(File walletFile, WalletInfoData walletInfo) {
        Collection<String> backupWalletsToTry = new ArrayList<String>();
        
        // Get the name of the rolling backup file.
        String walletBackupFilenameLong = walletInfo.getProperty(BitcoinModel.WALLET_BACKUP_FILE);
        String walletBackupFilenameShort = null;
        if (walletBackupFilenameLong != null && !"".equals(walletBackupFilenameLong)) {
            File walletBackupFile = new File(walletBackupFilenameLong);
            walletBackupFilenameShort = walletBackupFile.getName();
            if (!walletBackupFile.exists()) {
                walletBackupFilenameLong = null;
                walletBackupFilenameShort = null;
            }
        } else {
            // No backup file was listed in the info file. Maybe it is damaged so take the most recent
            // file in the rolling backup directory, if there is one.
            Collection<File> rollingWalletBackups = getWalletsInBackupDirectory(walletFile.getAbsolutePath(),
                    ROLLING_WALLET_BACKUP_DIRECTORY_NAME);
            if (rollingWalletBackups != null && !rollingWalletBackups.isEmpty()) {
                List<String> rollingWalletBackupFilenames = new ArrayList<String>();
                for (File file  : rollingWalletBackups) {
                    rollingWalletBackupFilenames.add(file.getAbsolutePath());
                }
                Collections.sort(rollingWalletBackupFilenames);
                walletBackupFilenameLong = rollingWalletBackupFilenames.get(rollingWalletBackupFilenames.size() - 1);
                walletBackupFilenameShort = (new File(walletBackupFilenameLong)).getName();
            }
        }
        
        Collection<File> unencryptedWalletBackups = getWalletsInBackupDirectory(walletFile.getAbsolutePath(),
                UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME);
        Collection<File> encryptedWalletBackups = getWalletsInBackupDirectory(walletFile.getAbsolutePath(),
                ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME);

        // Make a list of ALL the unencrypted and encrypted backup names and sort them.
        // Because the backups have a timestamp YYYYMMDDHHMMSS sort in ascending order gives most recent - we will use this one.
        List<String> encryptedAndUnencryptedFilenames = new ArrayList<String>();
        
        // Sorting is done by the filename, keep track of the corresponding absolute path.
        Map<String, String> shortNamesToLongMap = new HashMap<String, String>();
        
        if (unencryptedWalletBackups != null) {
            for (File file  : unencryptedWalletBackups) {
                encryptedAndUnencryptedFilenames.add(file.getName());
                shortNamesToLongMap.put(file.getName(), file.getAbsolutePath());
            }
        }
        if (encryptedWalletBackups != null) {
            for (File file  : encryptedWalletBackups) {
                encryptedAndUnencryptedFilenames.add(file.getName());
                // If there is a duplicate, encrypted wallets are preferred.
                shortNamesToLongMap.put(file.getName(), file.getAbsolutePath());
            }
        }
        
        Collections.sort(encryptedAndUnencryptedFilenames);
        
        String bestCandidateShort = null;
        String bestCandidateLong = null;
        if (encryptedAndUnencryptedFilenames.size() > 0) {
            bestCandidateShort = encryptedAndUnencryptedFilenames.get(encryptedAndUnencryptedFilenames.size() - 1);
            if (bestCandidateShort != null) {
                bestCandidateLong = shortNamesToLongMap.get(bestCandidateShort);
            }
        }
        log.debug("For wallet '" + walletFile + "' the rolling backup file was '" + walletBackupFilenameLong + "' and the best encrypted/ unencrypted backup was '" + bestCandidateLong + "'");
        
        if (walletBackupFilenameLong == null) {
            if (bestCandidateLong == null) {
                // No backups to try.
            } else {
                // bestCandidate only.
                backupWalletsToTry.add(bestCandidateLong);
            }
        } else {
          if (bestCandidateLong == null) {
              // WalletBackupFilename only.
              backupWalletsToTry.add(walletBackupFilenameLong);
            } else {
                // Have both. Try the most recent first (preferring the backups to the rolling backups if there is a tie).
                if (walletBackupFilenameShort.compareTo(bestCandidateShort) <= 0) {
                    backupWalletsToTry.add(bestCandidateLong);
                    backupWalletsToTry.add(walletBackupFilenameLong);
                } else {
                    backupWalletsToTry.add(walletBackupFilenameLong);
                    backupWalletsToTry.add(bestCandidateLong);                    
                }
            }
        }
        return backupWalletsToTry;
    }

    public String calculateTopLevelBackupDirectoryName(File walletFile) {
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
    
    public void moveSiblingTimestampedKeyAndWalletBackups(String walletFilename) {
        // Work out the stem of the wallet i.e. stripped of a .wallet suffix
        File walletFile = new File(walletFilename);
        String stemShort= walletFile.getName();
        if (walletFilename.matches(REGEX_FOR_WALLET_SUFFIX)) {
            stemShort = walletFile.getName().substring(0, walletFile.getName().length() - ("." + BitcoinModel.WALLET_FILE_EXTENSION).length());
        }
        
        String topLevelWalletDirectory = BackupManager.INSTANCE.calculateTopLevelBackupDirectoryName(new File(walletFilename));

        // Get the files in the directory.
        File containingDirectory = walletFile.getParentFile();
        File[] siblingFiles = containingDirectory.listFiles();
        
        // See if there are any files matching stem + timestamp + .key, .wallet and .info.
        Collection<String> privateKeyFilesShort = new ArrayList<String>();
        Collection<String> walletFilesShort = new ArrayList<String>();
        Collection<String> infoFilesShort = new ArrayList<String>();
        if (siblingFiles != null) {
            for (int i = 0; i < siblingFiles.length; i++) {
                // Dont process directories.
                if (!siblingFiles[i].isDirectory()) {
                    String siblingFilenameShort = siblingFiles[i].getName();
                    if (siblingFilenameShort.matches(REGEX_FOR_TIMESTAMP_AND_KEY_SUFFIX)) {
                        // It has a timestamp and the key suffix.
                        if (siblingFilenameShort.startsWith(stemShort)
                                && siblingFilenameShort.length() == (stemShort.length() + 19)) {
                            // 19 = length of hyphen + timestamp + dot + key
                            privateKeyFilesShort.add(siblingFilenameShort);
                        }
                    } else if (siblingFilenameShort.matches(REGEX_FOR_TIMESTAMP_AND_WALLET_SUFFIX)) {
                        // It has a timestamp and the wallet suffix.
                        if (siblingFilenameShort.startsWith(stemShort)
                                && siblingFilenameShort.length() == (stemShort.length() + 22)) {
                            // 22 = length of hyphen + timestamp + dot + wallet
                            walletFilesShort.add(siblingFilenameShort);
                        }
                    }  else if (siblingFilenameShort.matches(REGEX_FOR_TIMESTAMP_AND_INFO_SUFFIX)) {
                        // It has a timestamp and the info suffix.
                        if (siblingFilenameShort.startsWith(stemShort)
                                && siblingFilenameShort.length() == (stemShort.length() + 20)) {
                            // 20 = length of hyphen + timestamp + dot + info
                            infoFilesShort.add(siblingFilenameShort);
                        }
                    }
                }
            }
            
            // Move the sibling key files to the data/key-backup directory.
            for (String keyFilename : privateKeyFilesShort) {
                File sourceFile = new File(containingDirectory + File.separator + keyFilename);
                File destinationFile = new File(topLevelWalletDirectory + File.separator + PRIVATE_KEY_BACKUP_DIRECTORY_NAME + File.separator + keyFilename);
                try {
                    sourceFile.renameTo(destinationFile);
                } catch (SecurityException se) {
                    // Just log the error message.
                    log.error(se.getClass().getName() + " " + se.getMessage());
                } catch (NullPointerException npe) {
                    // Just log the error message.
                    log.error(npe.getClass().getName() + " " + npe.getMessage());
                }
            }
            
            // Move the sibling wallet files (and their info files) to the data/wallet-backup or data/wallet-unenc-backup directory.
            for (String loopWalletFilename : walletFilesShort) {
                File walletSourceFile = new File(containingDirectory + File.separator + loopWalletFilename);
                File walletDestinationFileUnencrypted = new File(topLevelWalletDirectory + File.separator
                        + UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME + File.separator + loopWalletFilename);
                File walletDestinationFileEncrypted = new File(topLevelWalletDirectory + File.separator
                        + ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME + File.separator + loopWalletFilename);

                // See if there is a matching info file for the wallet.
                String infoFilenameShort = WalletInfoData.createWalletInfoFilename(loopWalletFilename);
                boolean alsoRenameInfoFile = infoFilesShort.contains(infoFilenameShort);
                File infoFileSourceFile = new File(containingDirectory + File.separator + infoFilenameShort);
                File infoFileDestinationFileUnencrypted = new File(topLevelWalletDirectory + File.separator
                        + UNENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME + File.separator + infoFilenameShort);
                File infoFileDestinationFileEncrypted = new File(topLevelWalletDirectory + File.separator
                        + ENCRYPTED_WALLET_BACKUP_DIRECTORY_NAME + File.separator + infoFilenameShort);
                
                File destinationWalletFile = null;
                File destinationInfoFile = null;

                FileInputStream fileInputStream = null;
                InputStream stream = null;
                try {
                    // Try to load the wallet to see if it is encrypted or not.
                    fileInputStream = new FileInputStream(walletSourceFile);
                    stream = new BufferedInputStream(fileInputStream);
                    Wallet loadedWallet = Wallet.loadFromFileStream(stream);
                    if (loadedWallet != null) {
                        if (EncryptionType.UNENCRYPTED == loadedWallet.getEncryptionType()) {
                            destinationWalletFile = walletDestinationFileUnencrypted;
                            destinationInfoFile = infoFileDestinationFileUnencrypted;
                        } else {
                            destinationWalletFile = walletDestinationFileEncrypted;
                            destinationInfoFile = infoFileDestinationFileEncrypted;
                        }
                    }
                } catch (Exception e) {
                    // Just log the error message - we will treat the wallet as
                    // unencrypted.
                    log.error(e.getClass().getName() + " " + e.getMessage());
                } finally {
                    if (stream != null) {
                        try {
                            stream.close();
                            stream = null;
                        } catch (IOException e) {
                            log.error(e.getClass().getName() + " " + e.getMessage());
                        }
                    } if (fileInputStream != null) {
                        try {
                            fileInputStream.close();
                            fileInputStream = null;
                        } catch (IOException e) {
                            log.error(e.getClass().getName() + " " + e.getMessage());
                        }
                    }
                    try {
                        // Rename the wallet.
                        if (destinationWalletFile != null) {
                            walletSourceFile.renameTo(destinationWalletFile);
                        }
                        
                        // Rename the info file.
                        if (alsoRenameInfoFile && destinationInfoFile != null) {
                            infoFileSourceFile.renameTo(destinationInfoFile);
                        }
                    } catch (SecurityException se) {
                        // Just log the error message.
                        log.error(se.getClass().getName() + " " + se.getMessage());
                    } catch (NullPointerException npe) {
                        // Just log the error message.
                        log.error(npe.getClass().getName() + " " + npe.getMessage());
                    }
                }
            }
        }
    }
    
    List<File> getWalletsInBackupDirectory(String walletFilename, String directorySuffix) {
        // See if there are any wallet backups.
        String topLevelBackupDirectoryName = calculateTopLevelBackupDirectoryName(new File(walletFilename));
        String walletBackupDirectoryName = topLevelBackupDirectoryName + File.separator
                + directorySuffix;
        File walletBackupDirectory = new File(walletBackupDirectoryName);

        File[] listOfFiles = walletBackupDirectory.listFiles();

        List<File> walletBackups = new ArrayList<File>();
        // Look for filenames with format "text"-YYYYMMDDHHMMSS.wallet<eol> and are not empty.
        if (listOfFiles != null) {
            for (int i = 0; i < listOfFiles.length; i++) {
                if (listOfFiles[i].isFile()) {
                    if (listOfFiles[i].getName().matches(REGEX_FOR_TIMESTAMP_AND_WALLET_SUFFIX)) {
                        if (listOfFiles[i].length() > 0) {
                            walletBackups.add(listOfFiles[i]);
                        }
                    }
                } 
            }
        }
        
        return walletBackups;
    }

    private void createDirectoryIfNecessary(String directoryName) {
        File directory = new File(directoryName);
        if (!directory.exists()) {
            boolean createSuccess = directory.mkdir();
            log.debug("Result of create of directory + '" + directoryName + "' was " + createSuccess);
        }
    }
}
