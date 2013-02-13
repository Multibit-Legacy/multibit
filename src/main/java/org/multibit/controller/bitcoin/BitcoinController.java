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
package org.multibit.controller.bitcoin;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URLDecoder;
import java.util.List;
import java.util.Set;

import org.multibit.controller.AbstractEventHandeler;
import org.multibit.controller.AbstractController;
import org.multibit.controller.core.CoreController;
import org.multibit.file.FileHandler;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.PeerEventListener;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletEventListener;
import com.google.bitcoin.uri.BitcoinURI;
import com.google.bitcoin.uri.BitcoinURIParseException;



/**
 * The MVC controller for MultiBit.
 * 
 * @author jim
 */
public class BitcoinController extends AbstractController<CoreController> implements WalletEventListener {

    public static final String ENCODED_SPACE_CHARACTER = "%20";

    private Logger log = LoggerFactory.getLogger(BitcoinController.class);

    /**
     * The data model backing the views.
     */
    private MultiBitModel model;

    /**
     * The bitcoinj network interface.
     */
    private MultiBitService multiBitService;

    /**
     * Class encapsulating File IO.
     */
    private FileHandler fileHandler;
    
    /**
     * The listener handling Peer events.
     */
    private PeerEventListener peerEventListener;
    
    private Set<AbstractEventHandeler> eventHandelers;
    
    private EventHandeler eventHandeler;



    private volatile boolean applicationStarting = true;


    public BitcoinController(CoreController coreController) {
        super(coreController);

        fileHandler = new FileHandler(this);
        peerEventListener = new BitcoinEventListener(this);
        
        this.eventHandeler = new EventHandeler(this);
        super.addEventHandler(this.getEventHandeler());
    }

    /**
     * Add a wallet to multibit from a filename.
     * 
     * @param walletFilename
     *            The wallet filename
     * 
     * @return The model data
     */
    public PerWalletModelData addWalletFromFilename(String walletFilename) throws IOException {
        PerWalletModelData perWalletModelDataToReturn = null;
        if (multiBitService != null) {
            perWalletModelDataToReturn = multiBitService.addWalletFromFilename(walletFilename);
        }
        return perWalletModelDataToReturn;
    }
    
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        }

        fireDataChangedUpdateNow();
    }

    /**
     * Method called by downloadListener whenever a block is downloaded.
     */
    public void fireBlockDownloaded() {
        // log.debug("Fire blockdownloaded");
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.blockDownloaded();
        }
    }

    private void checkForDirtyWallets(Transaction transaction) {
        List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();
        for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
            try {
                if (loopPerWalletModelData.getWallet().isTransactionRelevant(transaction, true)) {
                    loopPerWalletModelData.setDirty(true);
                    //log.debug("Marking wallet '" + loopPerWalletModelData.getWalletFilename() + "' as dirty.");
                }
            } catch (ScriptException e) {
                log.debug(e.getMessage());
            }
        }
    }

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
        }
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onCoinsSent(wallet, transaction, prevBalance, newBalance);
        }
    }
    
    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {       
        // Set the depth in blocks as this does not seem to get updated anywhere.
//        if (getMultiBitService().getChain() != null && transaction.getConfidence().getConfidenceType() == ConfidenceType.BUILDING) {
//            transaction.getConfidence().setDepthInBlocks(getMultiBitService().getChain().getBestChainHeight() - transaction.getConfidence().getAppearedAtChainHeight() + 1);
//        }
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.onTransactionConfidenceChanged(wallet, transaction);
        }
        checkForDirtyWallets(transaction);
    }

    @Override
    public void onKeyAdded(ECKey ecKey) {
        log.debug("Key added : " + ecKey.toString());
    }

    @Override
    public void onReorganize(Wallet wallet) {
        List<PerWalletModelData> perWalletModelDataList = getModel().getPerWalletModelDataList();
        for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
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
        log.debug("handleOpenURI called and rawBitcoinURI ='" + eventHandeler.rawBitcoinURI + "'");

        // Get the open URI configuration information
        String showOpenUriDialogText = getModel().getUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        String useUriText = getModel().getUserPreference(MultiBitModel.OPEN_URI_USE_URI);

        if (Boolean.FALSE.toString().equalsIgnoreCase(useUriText)
                && Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // Ignore open URI request.
            log.debug("Bitcoin URI ignored because useUriText = '" + useUriText + "', showOpenUriDialogText = '"
                    + showOpenUriDialogText + "'");
            org.multibit.message.Message message = new org.multibit.message.Message(super.getLocaliser().getString("showOpenUriView.paymentRequestIgnored"));
            MessageManager.INSTANCE.addMessage(message);
            
            return;
        }
        if (eventHandeler.rawBitcoinURI == null || "".equals(eventHandeler.rawBitcoinURI)) {
            log.debug("No Bitcoin URI found to handle");
            // displayView(getCurrentView());
            return;
        }
        // Process the URI.
        // TODO Consider handling the possible runtime exception at a suitable
        // level for recovery

        // Early MultiBit versions did not URL encode the label hence may
        // have illegal embedded spaces - convert to ENCODED_SPACE_CHARACTER i.e
        // be lenient.
        String uriString = eventHandeler.rawBitcoinURI.toString().replace(" ", ENCODED_SPACE_CHARACTER);
        BitcoinURI bitcoinURI = null;
        try {
            bitcoinURI = new BitcoinURI(this.getModel().getNetworkParameters(), uriString);
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
            getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, address);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, label);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, amount);
            getModel().setActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW, "true");
            log.debug("Routing straight to send view for address = " + address);

            getModel().setUserPreference(MultiBitModel.BRING_TO_FRONT, "true");
            displayView(View.SEND_BITCOIN_VIEW);
            return;
        } else {
            // Show the confirm dialog to see if the user wants to use URI.
            // Populate the model with the URI data.
            getModel().setUserPreference(MultiBitModel.OPEN_URI_ADDRESS, address);
            getModel().setUserPreference(MultiBitModel.OPEN_URI_LABEL, label);
            getModel().setUserPreference(MultiBitModel.OPEN_URI_AMOUNT, amount);
            log.debug("Routing to show open uri view for address = " + address);

            displayView(View.SHOW_OPEN_URI_DIALOG_VIEW);
            return;
        }
    }

    public PeerEventListener getPeerEventListener() {
        return peerEventListener;
    }
    
    @Override
    public final AbstractEventHandeler getEventHandeler() {
        return this.eventHandeler;
    }
    
    private class EventHandeler extends AbstractEventHandeler<BitcoinController> {

        private volatile URI rawBitcoinURI = null;

        public EventHandeler(BitcoinController multiBitController) {
            super(multiBitController);
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
}
