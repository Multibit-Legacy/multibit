package org.multibit.viewsystem.swing;

import javax.swing.ImageIcon;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ImageLoader {
    private static final Logger log = LoggerFactory.getLogger(ImageLoader.class);

    public static final String COPY_ICON_FILE = "/images/copy.png";
    public static final String PASTE_ICON_FILE = "/images/paste.png";
    public static final String TICK_ICON_FILE = "/images/tick.png";
    public static final String RED_CROSS_ICON_FILE = "/images/redCross.png";
    public static final String YOUR_WALLETS_ICON_FILE = "/images/yourWallets.png";
    public static final String SINGLE_WALLET_ICON_FILE = "/images/singleWallet.png";
    public static final String CREATE_NEW_ICON_FILE = "/images/createNew.png";
    public static final String OPEN_WALLET_ICON_FILE = "/images/openWallet.png";
    public static final String SEND_BITCOIN_ICON_FILE = "/images/send.jpg";
    public static final String RECEIVE_BITCOIN_ICON_FILE = "/images/receive.jpg";
    public static final String PREFERENCES_ICON_FILE = "/images/preferences.png";
    public static final String HELP_CONTENTS_ICON_FILE = "/images/helpContents.png";
    public static final String MULTIBIT_SMALL_ICON_FILE = "/images/multibit-small.png";
    public static final String MULTIBIT_ICON_FILE = "/images/multibit.png";
    public static final String MULTIBIT_128_ICON_FILE = "/images/multibit128.png";
    public static final String QUESTION_MARK_ICON_FILE = "/images/questionMark.png";
    public static final String WALLET_ICON_FILE = "/images/wallet.png";
    public static final String RTL_WALLET_ICON_FILE = "/images/rtl_wallet.png";
    public static final String EXCLAMATION_MARK_ICON_FILE = "/images/exclamationMark.png";
    public static final String IMPORT_PRIVATE_KEYS_ICON_FILE = "/images/importKey.png";
    public static final String EXPORT_PRIVATE_KEYS_ICON_FILE = "/images/exportKey.png";
    public static final String RESET_TRANSACTIONS_ICON_FILE = "/images/resetTransactions.png";
       

    public ImageLoader() {
        
    }
    
    /**
     * Returns an ImageIcon, or null if the path was invalid.
     */
    public static ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            log.error("org.multibit.MultiBitFrame#createImageIcon: Could not find file: " + path);
            return null;
        }
    }

}
