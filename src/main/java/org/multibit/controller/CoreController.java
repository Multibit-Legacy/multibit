/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.controller;

import com.google.bitcoin.uri.BitcoinURI;
import com.google.bitcoin.uri.BitcoinURIParseException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Localiser;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.platform.listener.GenericAboutEvent;
import org.multibit.platform.listener.GenericOpenURIEvent;
import org.multibit.platform.listener.GenericPreferencesEvent;
import org.multibit.viewsystem.core.View;
import org.multibit.viewsystem.ViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public class CoreController implements Controller {
    
    private Logger log = LoggerFactory.getLogger(CoreController.class);
    
    
    /**
     * The view systems under control of the CoreController.
     */
    private Collection<ViewSystem> viewSystems;
    
    
    /**
     * The data model backing the views.
     */
    private MultiBitModel model;
    
    /**
     * The localiser used to localise everything.
     */
    private Localiser localiser;

    /**
     * Class encapsulating the location of the Application Data Directory.
     */
    private ApplicationDataDirectoryLocator applicationDataDirectoryLocator;
    
    /**
     * Multiple threads will write to this variable so require it to be volatile
     * to ensure that latest write is what gets read
     */
    private volatile URI rawBitcoinURI = null;

    private volatile boolean applicationStarting = true;
    
    /**
     * Used for testing only.
     */
    public CoreController()
    {
        this(null);
    }
    
    public CoreController(ApplicationDataDirectoryLocator applicationDataDirectoryLocator)
    {
        this.applicationDataDirectoryLocator = applicationDataDirectoryLocator;

        viewSystems = new ArrayList<ViewSystem>();

        // by default localise to English
        localiser = new Localiser(Locale.ENGLISH);
    }
    
    
    /**
     * @return the viewSystems
     */
    @Override
    public Collection<ViewSystem> getViewSystems() {
        return viewSystems;
    }
    
    /**
     * Display the view specified.
     * 
     * @param viewToDisplay
     *            View to display. Must be one of the View constants
     */
    @Override
    public void displayView(int viewToDisplay) {
        // log.debug("Displaying view '" + viewToDisplay + "'");

        // tell all views to close the current view
        for (ViewSystem viewSystem : getViewSystems()) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(viewToDisplay);

        // tell all views which view to display
        for (ViewSystem viewSystem : getViewSystems()) {
            viewSystem.displayView(getCurrentView());
        }
    }

    /**
     * Display the help context specified.
     * 
     * @param helpContextToDisplay
     *            The help context to display. A path in the help
     */
    @Override
    public void displayHelpContext(String helpContextToDisplay) {
        log.debug("Displaying help context '" + helpContextToDisplay + "'");
        // tell all views to close the current view
        for (ViewSystem viewSystem : getViewSystems()) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(View.HELP_CONTENTS_VIEW);

        // tell all views which view to display
        for (ViewSystem viewSystem : getViewSystems()) {
            viewSystem.setHelpContext(helpContextToDisplay);
            viewSystem.displayView(View.HELP_CONTENTS_VIEW);
        }
    }

    /**
     * Register a new ViewSystem from the list of views that are managed.
     * 
     * @param viewSystem
     *            system
     */
    @Override
    public void registerViewSystem(ViewSystem viewSystem) {
        getViewSystems().add(viewSystem);
    }
    
    @Override
    public MultiBitModel getModel() {
        return model;
    }

    @Override
    public void setModel(MultiBitModel model) {
        this.model = model;
    }


    /**
     * Fire that all the views need recreating.
     */
    @Override
    public void fireRecreateAllViews(boolean initUI) {
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : getViewSystems()) {
            viewSystem.recreateAllViews(initUI);
        }
    }

    /**
     * Fire that the model data has changed.
     */
    public void fireDataChanged() {
        for (ViewSystem viewSystem : this.viewSystems) {
            viewSystem.fireDataChanged();
        }
    }
    
     /**
     * The language has been changed.
     */
    public void fireDataStructureChanged() {
        Locale newLocale = new Locale(this.getModel().getUserPreference(MultiBitModel.USER_LANGUAGE_CODE));
        this.getLocaliser().setLocale(newLocale);

        int viewToDisplay = getCurrentView();

        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : this.viewSystems) {
            viewSystem.recreateAllViews(true);
        }

        setCurrentView(viewToDisplay);
        fireDataChanged();
    }
    

    @Override
    public Localiser getLocaliser() {
        return localiser;
    }

    @Override
    public void setLocaliser(Localiser localiser) {
        this.localiser = localiser;
    }
    
    @Override
    public ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator() {
        return applicationDataDirectoryLocator;
    }

    @Override
    public int getCurrentView() {
        if (getModel() != null) {
            return getModel().getCurrentView();
        } else {
            return View.DEFAULT_VIEW;
        }
    }

    @Override
    public void setCurrentView(int view) {
        // log.debug("setCurrentView = " + view);
        if (getModel() != null) {
            getModel().setCurrentView(view);
        }
    }

    @Override
    public void setApplicationStarting(boolean applicationStarting) {
        this.applicationStarting = applicationStarting;
    }

    @Override
    public synchronized void onOpenURIEvent(GenericOpenURIEvent event) {
        rawBitcoinURI = event.getURI();
        log.debug("Controller received open URI event with URI='{}'", rawBitcoinURI.toASCIIString());
        if (!applicationStarting) {
            log.debug("Open URI event handled immediately");
            handleOpenURI();
        } else {
            log.debug("Open URI event not handled immediately because application is still starting");
        }
    }

    @Override
    public synchronized void handleOpenURI() {
        log.debug("handleOpenURI called and rawBitcoinURI ='" + rawBitcoinURI + "'");

        // get the open URI configuration information
        String showOpenUriDialogText = getModel().getUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG);
        String useUriText = getModel().getUserPreference(MultiBitModel.OPEN_URI_USE_URI);

        if (Boolean.FALSE.toString().equalsIgnoreCase(useUriText)
                && Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
            // ignore open URI request
            log.debug("Bitcoin URI ignored because useUriText = '" + useUriText + "', showOpenUriDialogText = '"
                    + showOpenUriDialogText + "'");
            org.multibit.message.Message message = new org.multibit.message.Message(localiser.getString("showOpenUriView.paymentRequestIgnored"));
            MessageManager.INSTANCE.addMessage(message);
            
            return;
        }
        if (rawBitcoinURI == null || rawBitcoinURI.equals("")) {
            log.debug("No Bitcoin URI found to handle");
            // displayView(getCurrentView());
            return;
        }
        // Process the URI
        // TODO Consider handling the possible runtime exception at a suitable
        // level for recovery.

        // Early MultiBit versions did not URL encode the label hence may
        // have illegal embedded spaces - convert to ENCODED_SPACE_CHARACTER i.e
        // be lenient
        String uriString = rawBitcoinURI.toString().replace(" ", ENCODED_SPACE_CHARACTER);
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

        // temp.
        
        return;
        
//        if (Boolean.FALSE.toString().equalsIgnoreCase(showOpenUriDialogText)) {
//            // Do not show confirm dialog - go straight to send view.
//            // Populate the model with the URI data.
//            getModel().setActiveWalletPreference(MultiBitModel.SEND_ADDRESS, address);
//            getModel().setActiveWalletPreference(MultiBitModel.SEND_LABEL, label);
//            getModel().setActiveWalletPreference(MultiBitModel.SEND_AMOUNT, amount);
//            getModel().setActiveWalletPreference(MultiBitModel.SEND_PERFORM_PASTE_NOW, "true");
//            log.debug("Routing straight to send view for address = " + address);
//
//            getModel().setUserPreference(MultiBitModel.BRING_TO_FRONT, "true");
//            displayView(View.SEND_BITCOIN_VIEW);
//            return;
//        } else {
//            // Show the confirm dialog to see if the user wants to use URI.
//            // Populate the model with the URI data.
//            getModel().setUserPreference(MultiBitModel.OPEN_URI_ADDRESS, address);
//            getModel().setUserPreference(MultiBitModel.OPEN_URI_LABEL, label);
//            getModel().setUserPreference(MultiBitModel.OPEN_URI_AMOUNT, amount);
//            log.debug("Routing to show open uri view for address = " + address);
//
//            displayView(View.SHOW_OPEN_URI_DIALOG_VIEW);
//            return;
//        }
    }

    @Override
    public void onPreferencesEvent(GenericPreferencesEvent event) {
        displayView(View.PREFERENCES_VIEW);
    }

    @Override
    public void onAboutEvent(GenericAboutEvent event) {
        displayView(View.HELP_ABOUT_VIEW);
    }
    
}
