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

import org.multibit.model.PerWalletModelData;

import com.google.bitcoin.core.WalletEventListener;

/**
 * an interface describing a collection of views that are used to render the MultiBit application
 * @author jim
 *
 */
public interface ViewSystem extends WalletEventListener { 
    /**
     * display the view specified
     * @param view to display - one of the View constants
     */   
    public void displayView(int viewToDisplay);
    
    /**
     * navigate away from a view - gives the view the opportunity to tidy up/ disappear etc
     * @param viewToNavigateAwayFrom - current view to navigate away from -one of the View constants
     */   
    public void navigateAwayFromView(int viewToNavigateAwayFrom);
        
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
     * notification that a block has been downloaded
     */
    public void blockDownloaded();
 
    /**
     * update download status
     * @param updateDownloadStatus
     * @param clearAutomatically
     */
    public void updateStatusLabel(String updateDownloadStatus, boolean clearAutomatically);
    /**
     * update download status
     * @param updateDownloadStatus
     * @param percentComplete
     */
    public void updateStatusLabel(String updateDownloadStatus, double percentComplete);

}
