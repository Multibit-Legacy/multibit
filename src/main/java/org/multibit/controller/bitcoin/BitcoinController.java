/**
 * Copyright 2012 multibit.org
 *
 * Licensed under the MIT license (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License
 * at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package org.multibit.controller.bitcoin;

import com.google.dogecoin.core.*;
import com.google.dogecoin.uri.BitcoinURI;
import com.google.dogecoin.uri.BitcoinURIParseException;
import org.multibit.controller.AbstractController;
import org.multibit.controller.AbstractEventHandler;
import org.multibit.controller.core.CoreController;
import org.multibit.file.FileHandler;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletBusyListener;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * The MVC controller for MultiBit.
 * 
 * @author jim
 */
public class BitcoinController extends AbstractController<CoreController> implements WalletEventListener, TransactionConfidence.Listener {

    public static final String ENCODED_SPACE_CHARACTER = "%20";
    private Logger log = LoggerFactory.getLogger(BitcoinController.class);

    /**
     * The WalletBusy listeners
     */
    private final Collection<WalletBusyListener> walletBusyListeners;
    
    private EventHandler eventHandler;
    
    /**
     * The bitcoinj network interface.
     */
    private MultiBitService multiBitService;
    /**
     * Class encapsulating File IO.
     */
    private final FileHandler fileHandler;
    
    /**
     * The listener handling Peer events.
     */
    private final PeerEventListener peerEventListener;

    /**
     * The data model backing the views.
     */
    private BitcoinModel model;
    
    /**
     * Used for testing only.
     */
    public BitcoinController(CoreController coreController) {
        super(coreController);

        this.walletBusyListeners = new ArrayList<WalletBusyListener>();
        this.fileHandler = new FileHandler(this);
        this.eventHandler = new EventHandler(this);
        this.peerEventListener = new BitcoinPeerEventListener(this);
        
        this.addEventHandler(this.getEventHandler());
    }
    
    @Override
    public BitcoinModel getModel() {
        return model;
    }

    public void setModel(BitcoinModel model) {
        this.model = model;
    }

    /**
     * Register a new WalletBusyListener.
     */
    public void registerWalletBusyListener(WalletBusyListener walletBusyListener) {
        walletBusyListeners.add(walletBusyListener);
    }
    
    /**
     * Clear the wallet busy listeners
     */
    public void clearWalletBusyListeners() {
        walletBusyListeners.clear();
    }
    
    /**
     * Log the number of wallet busy listeners
     */
    public void logNumberOfWalletBusyListeners() {
        log.debug("There are " + walletBusyListeners.size() + " walletBusyListeners.");
    }

    /**
     * Add a wallet to multibit from a filename.
     * 
     * @param walletFilename The wallet filename
     * 
     * @return The model data
     */
    public WalletData addWalletFromFilename(String walletFilename) throws IOException {
        WalletData perWalletModelDataToReturn = null;
        if (multiBitService != null) {
            perWalletModelDataToReturn = multiBitService.addWalletFromFilename(walletFilename);
        }
        return perWalletModelDataToReturn;
    }

    public void fireFilesHaveBeenChangedByAnotherProcess(WalletData perWalletModelData) {
        //log.debug("fireFilesHaveBeenChangedByAnotherProcess called");
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        }

        fireDataChangedUpdateNow();
    }
       
    /**
     * Fire that a wallet has changed its busy state.
     */
    public void fireWalletBusyChange(boolean newWalletIsBusy) {
        //log.debug("fireWalletBusyChange called");
        for( Iterator<WalletBusyListener> it = walletBusyListeners.iterator(); it.hasNext();) {
            WalletBusyListener walletBusyListener = it.next();
            walletBusyListener.walletBusyChange(newWalletIsBusy);
        }
    }

    /**
     * Method called by downloadListener whenever a block is downloaded.
     */
    public void fireBlockDownloaded() {
        //log.debug("Fire blockdownloaded");
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.blockDownloaded();
        }
        
        // Mark all the wallets as dirty as their lastBlockSeenHeight will need changing.
        if (getModel() != null) {
            List<WalletData> perWalletModelDataList = getModel().getPerWalletModelDataList();
            if (perWalletModelDataList != null) {
                for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                    if (loopPerWalletModelData.getWalletInfo() != null) {
                        synchronized(loopPerWalletModelData.getWalletInfo()) {
                            loopPerWalletModelData.setDirty(true);
                        }
                    } else {
                        loopPerWalletModelData.setDirty(true);                        
                    }
                }
            }
        }
    }

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        //log.debug("onCoinsReceived called");
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        //log.debug("onCoinsSent called");
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onCoinsSent(wallet, transaction, prevBalance, newBalance);
        }
    }
    
    @Override
    public void onWalletChanged(Wallet wallet) {
        if (wallet == null) {
            return;
        }
        // log.debug("onWalletChanged called");
        final int walletIdentityHashCode = System.identityHashCode(wallet);
        for (WalletData loopPerWalletModelData : getModel().getPerWalletModelDataList()) {
            // Find the wallet object and mark as dirty.
            if (System.identityHashCode(loopPerWalletModelData.getWallet()) == walletIdentityHashCode) {
                loopPerWalletModelData.setDirty(true);
                break;
            }
        }

        fireDataChangedUpdateLater();
    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
        //log.debug("onTransactionConfidenceChanged called");
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onTransactionConfidenceChanged(wallet, transaction);
        }
    }
    
    @Override
    public void onKeysAdded(Wallet wallet, List<ECKey> keys) {
        log.debug("Keys added : " + keys.toString());
    }

    @Override
    public void onReorganize(Wallet wallet) {
        log.debug("onReorganize called");
        List<WalletData> perWalletModelDataList = getModel().getPerWalletModelDataList();
        for (WalletData loopPerWalletModelData : perWalletModelDataList) {
            if (loopPerWalletModelData.getWallet().equals(wallet)) {
                loopPerWalletModelData.setDirty(true);
                log.debug("Marking wallet '" + loopPerWalletModelData.getWalletFilename() + "' as dirty.");
            }
        }
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onReorganize(wallet);
        }
    }

    public MultiBitService getMultiBitService() {
        return multiBitService;
    }

    public void setMultiBitService(MultiBitService multiBitService) {
        this.multiBitService = multiBitService;
    }

    public FileHandler getFileHandler() {
        return fileHandler;
    }

    public synchronized void handleOpenURI() {
        log.debug("handleOpenURI.1 called and rawBitcoinURI ='" + eventHandler.rawBitcoinURI + "'");
        if (eventHandler.rawBitcoinURI != null) {
            handleOpenURI(eventHandler.rawBitcoinURI.toString());
        }
    }

    public synchronized void handleOpenURI(String rawBitcoinURIString) {
        log.debug("handleOpenURI.2 called and rawBitcoinURIString ='" + rawBitcoinURIString + "'");

        // get the open URI configuration information
        String showOpenUriDialogText = getModel().getUserPreference(BitcoinModel.OPEN_URI_SHOW_DIALOG);
        String useUriText = getModel().getUserPreference(BitcoinModel.OPEN_URI_USE_URI);

        if (Boolean.FALSE.toString().equalsIgnoreCase(useUriText)
                && Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // ignore open URI request
            log.debug("Bitcoin URI ignored because useUriText = '" + useUriText + "', showOpenUriDialogText = '"
                    + showOpenUriDialogText + "'");
            org.multibit.message.Message message = new org.multibit.message.Message(super.getLocaliser().getString("showOpenUriView.paymentRequestIgnored"));
            MessageManager.INSTANCE.addMessage(message);
            
            return;
        }
        if (rawBitcoinURIString == null || rawBitcoinURIString.equals("")) {
            log.debug("No Bitcoin URI found to handle");
            return;
        }
        // Process the URI
        // TODO Consider handling the possible runtime exception at a suitable
        // level for recovery.

        // Early MultiBit versions did not URL encode the label hence may
        // have illegal embedded spaces - convert to ENCODED_SPACE_CHARACTER i.e
        // be lenient
        String uriString = rawBitcoinURIString.replace(" ", ENCODED_SPACE_CHARACTER);
        BitcoinURI bitcoinURI;
        try {
            bitcoinURI = new BitcoinURI(getModel().getNetworkParameters(), uriString);
        } catch (BitcoinURIParseException pe) {
            log.error("Could not parse the uriString '" + uriString + "', aborting");
            return;
        }

        // Convert the URI data into suitably formatted view data.
        String address = bitcoinURI.getAddress().toString();
        String label = "";
        try {
            // No label? Set it to a blank String otherwise perform a URL decode
            // on it just to be sure.
            label = null == bitcoinURI.getLabel() ? "" : URLDecoder.decode(bitcoinURI.getLabel(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            log.error("Could not decode the label in UTF-8. Unusual URI entry or platform.");
        }
        // No amount? Set it to zero.
        BigInteger numericAmount = null == bitcoinURI.getAmount() ? BigInteger.ZERO : bitcoinURI.getAmount();
        String amount = getLocaliser().bitcoinValueToStringNotLocalised(numericAmount, false, false);

        if (Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // Do not show confirm dialog - go straight to send view.
            // Populate the model with the URI data.
            getModel().setActiveWalletPreference(BitcoinModel.SEND_ADDRESS, address);
            getModel().setActiveWalletPreference(BitcoinModel.SEND_LABEL, label);
            getModel().setActiveWalletPreference(BitcoinModel.SEND_AMOUNT, amount);
            getModel().setActiveWalletPreference(BitcoinModel.SEND_PERFORM_PASTE_NOW, "true");
            log.debug("Routing straight to send view for address = " + address);

            getModel().setUserPreference(BitcoinModel.BRING_TO_FRONT, "true");
            displayView(View.SEND_BITCOIN_VIEW);
            return;
        } else {
            // Show the confirm dialog to see if the user wants to use URI.
            // Populate the model with the URI data.
            getModel().setUserPreference(BitcoinModel.OPEN_URI_ADDRESS, address);
            getModel().setUserPreference(BitcoinModel.OPEN_URI_LABEL, label);
            getModel().setUserPreference(BitcoinModel.OPEN_URI_AMOUNT, amount);
            log.debug("Routing to show open uri view for address = " + address);

            displayView(View.SHOW_OPEN_URI_DIALOG_VIEW);
            return;
        }
    }

    public PeerEventListener getPeerEventListener() {
        return peerEventListener;
    }

    @Override
    public final AbstractEventHandler getEventHandler() {
        return this.eventHandler;
    }
    
    private class EventHandler extends AbstractEventHandler<BitcoinController> {

    /**
         * Multiple threads will write to this variable so require it to be
         * volatile to ensure that latest write is what gets read
     */
        private volatile URI rawBitcoinURI = null;

        public EventHandler(BitcoinController coreController) {
            super(coreController);
    }

        @Override
        public void handleOpenURIEvent(URI rawBitcoinURI) {
            this.rawBitcoinURI = rawBitcoinURI;
            handleOpenURI();

        }

        @Override
        public void handleQuitEvent(ExitAction exitAction) {
            exitAction.setBitcoinController(super.controller);
        }
    }

    public void onConfidenceChanged(Transaction tx) {
    }

    @Override
    public void onConfidenceChanged(Transaction tx, ChangeReason reason) {
    }
}
