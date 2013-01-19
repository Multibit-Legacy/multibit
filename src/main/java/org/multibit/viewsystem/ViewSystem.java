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
import org.multibit.model.StatusEnum;

import com.google.bitcoin.core.WalletEventListener;

/**
 * An interface describing a collection of views that are used to render the MultiBit application
 * @author jim
 *
 */
public interface ViewSystem extends WalletEventListener { 
    /**
     * display the view specified
     * @param view to display - one of the View constants
     */   
    public void displayView(View viewToDisplay);
    
    /**
     * navigate away from a view - gives the view the opportunity to tidy up/ disappear etc
     * @param viewToNavigateAwayFrom - current view to navigate away from -one of the View constants
     */   
    public void navigateAwayFromView(View viewToNavigateAwayFrom);
           
    /**
     * tells the view system that the model data has changed (but the wallet is still the same)
     */   
    public void fireDataChanged();
    
    /**
     * tells the view system to recreate all views e.g. after a language change or wallet change
     * @param initUI Completely redraw everything on all screens = true
     */   
    public void recreateAllViews(boolean initUI);
       
     /**
     * tells the view system that an external process has modified one of the wallets
     */
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData);

    /**
     * a method called when MultiBit online status changes between online, connecting. error
     */
    public void setOnlineStatus(StatusEnum statusEnum);

    /**
     * notification that a block has been downloaded
     * (this is typically called from a peer thread)
     */
    public void blockDownloaded();
 
    /**
     * Set the help context to display
     * @param helpContextToDisplay
     */
    public void setHelpContext(String helpContextToDisplay);
}
