/**
 * Copyright 2011 multibit.org
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
package org.multibit.viewsystem;

import java.math.BigInteger;
import java.net.URI;

import org.multibit.model.PerWalletModelData;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;


/**
 * an interface describing a collection of views that are used to render the MultiBit application
 * @author jim
 *
 */
public interface ViewSystem { 
    public static final int NEW_VIEW_IS_CHILD_OF_PREVIOUS = 10000;
    public static final int NEW_VIEW_IS_PARENT_OF_PREVIOUS = 10001;
    public static final int NEW_VIEW_IS_SIBLING_OF_PREVIOUS = 10002;
       
    /**
     * display the view specified
     * @param view to display - one of the View constants
     */   
    public void displayView(int viewToDisplay);
    
    /**
     * navigate away from a view - gives the view the opportunity to tidy up/ disappear etc
     * @param viewToNavigateAwayFrom - current view to navigate away from -one of the View constants
     * @param nextView - next view - one of the View constants
     * @param relationshipOfNewViewToPrevious - one of ViewSystem.isChild, isParent, isSibling
     */   
    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextView, int relationshipOfNewViewToPrevious);
    
    /**
     * display a message to the user - using the current localiser
     * @param messageKey the key to localise for the message
     * @param messageData the data used in the messag
     * @param titleKey the key to localise for the title
     */   
    public void displayMessage(String messageKey, Object[] messageData, String titleKey);
    
    /**
     * tells the views a new wallet has been created
     */
    public void newWalletCreated();
    
    /**
     * tells the view system to recreate all views e.g. after a language change
     */   
    public void recreateAllViews(boolean clearCache, boolean initUI);
       
    /**
     * tells the view system that the model data has changed
     */   
    public void fireDataChanged();
    
    /**
     * tells the view system that an external process has modified one of the wallets
     */
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData);

    /**
     * a method called when MultiBit is now online i.e. now has a peer when it did not before
     */
    public void nowOnline();

    /**
     * a method called when MultiBit is now offline i.e. the last peer has disconnecte
     */
    public void nowOffline();
    
    /**
     * update download status
     * @param updateDownloadStatus
     */
    public void updateStatusLabel(String updateDownloadStatus);
  
    /**
     * notification that a block has been downloaded
     */
    public void blockDownloaded();
  
    /**
     * WalletEventListener callback method
     * @param wallet
     * @param transaction
     * @param prevBalance
     * @param newBalance
     */
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance,
            BigInteger newBalance);
    
    /**
     * WalletEventListener callback method
     * @param wallet
     * @param transaction
     */
    public void onPendingCoinsReceived(Wallet wallet, Transaction transaction);
    
    /**
     * WalletEventListener callback method
     * @param wallet
     */
    public void onReorganise(Wallet wallet);
}
