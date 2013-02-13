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
package org.multibit.controller.core;

import java.net.URI;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.AbstractEventHandeler;
import org.multibit.controller.AbstractEventHandeler;
import org.multibit.controller.BaseController;
import org.multibit.controller.BaseController;
import org.multibit.model.MultiBitModel;
import org.multibit.platform.listener.GenericAboutEvent;
import org.multibit.platform.listener.GenericAboutEventListener;
import org.multibit.platform.listener.GenericOpenURIEvent;
import org.multibit.platform.listener.GenericOpenURIEventListener;
import org.multibit.platform.listener.GenericPreferencesEvent;
import org.multibit.platform.listener.GenericPreferencesEventListener;
import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitEventListener;
import org.multibit.platform.listener.GenericQuitResponse;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public class CoreController extends BaseController<CoreController> implements GenericOpenURIEventListener, GenericPreferencesEventListener,
        GenericAboutEventListener, GenericQuitEventListener {
    
    private Logger log = LoggerFactory.getLogger(CoreController.class);
    
    private Set<AbstractEventHandeler> eventHandelers;
    private CoreController.EventHandeler eventHandeler;
    
    /**
     * The data model backing the views.
     */
    private MultiBitModel model;
    
    public CoreController() {
        this(null);
    }
    
    public CoreController(ApplicationDataDirectoryLocator applicationDataDirectoryLocator) {
        super(applicationDataDirectoryLocator);
        
        this.eventHandelers = new HashSet<AbstractEventHandeler>();
        this.eventHandeler = new EventHandeler(this);
        
        this.addEventHandler(this.getEventHandeler());
    }
    
    
    @Override
    public MultiBitModel getModel() {
        return model;
    }

    public void setModel(MultiBitModel model) {
        this.model = model;
    }
    
    
    
    /**
     * Display the view specified.
     * 
     * @param viewToDisplay
     *            View to display. Must be one of the View constants
     */
    @Override
    public void displayView(View viewToDisplay) {
        log.debug("Displaying view '" + viewToDisplay + "'");

        // Tell all views to close the current view.
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(viewToDisplay);

        // Tell all views which view to display.
        for (ViewSystem viewSystem : super.getViewSystem()) {
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
        //log.debug("Displaying help context '" + helpContextToDisplay + "'");
        
        // Tell all views to close the current view.
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.navigateAwayFromView(getCurrentView());
        }

        setCurrentView(View.HELP_CONTENTS_VIEW);

        // Tell all views which view to display.
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.setHelpContext(helpContextToDisplay);
            viewSystem.displayView(View.HELP_CONTENTS_VIEW);
        }
    }
    
    /**
     * The language has been changed.
     */
    @Override
    public void fireDataStructureChanged() {
        //log.debug("fireDataStructureChanged called");

        Locale newLocale = new Locale(model.getUserPreference(MultiBitModel.USER_LANGUAGE_CODE));
        super.getLocaliser().setLocale(newLocale);

        View viewToDisplay = getCurrentView();

        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : super.getViewSystem()) {
            viewSystem.recreateAllViews(true, viewToDisplay);
        }

        setCurrentView(viewToDisplay);
        fireDataChangedUpdateNow();
    }
    
    @Override
    public View getCurrentView() {
        View view = (null == getModel()) ? null : getModel().getCurrentView();
        
        return (null == view) ? View.DEFAULT_VIEW() : view;
    }

    @Override
    public void setCurrentView(View view) {
        // log.debug("setCurrentView = " + view);
        if (getModel() != null) {
            getModel().setCurrentView(view);
        }
    }
    
    @Override
    public void onPreferencesEvent(GenericPreferencesEvent event) {
        displayView(View.PREFERENCES_VIEW);
    }

    @Override
    public void onAboutEvent(GenericAboutEvent event) {
        displayView(View.HELP_ABOUT_VIEW);
    }
    
    @Override
    public synchronized void onOpenURIEvent(GenericOpenURIEvent event) {
        log.debug("Controller received open URI event with URI='{}'", event.getURI().toASCIIString());
           if (!getApplicationStarting()) {
            log.debug("Open URI event handled immediately");

            for (AbstractEventHandeler theEventHandeler : this.eventHandelers) {
                theEventHandeler.handleOpenURIEvent(event.getURI());
            }
               
        } else {
            log.debug("Open URI event not handled immediately because application is still starting");
        }
    }
    
    @Override
    public void onQuitEvent(GenericQuitEvent event, GenericQuitResponse response) {
        if (isOKToQuit()) {
            ExitAction exitAction = new ExitAction(this,null);

            for (AbstractEventHandeler theEventHandeler : this.eventHandelers) {
                theEventHandeler.handleQuitEvent(exitAction);
            }
            
            exitAction.actionPerformed(null);
            response.performQuit();
            
        } else {
            response.cancelQuit();
        }
    }
    
    
    @Override
    protected final void addEventHandler(AbstractEventHandeler eventHandeler) {
        this.eventHandelers.add(eventHandeler);
    }
    
    @Override
    public final AbstractEventHandeler getEventHandeler() {
        return this.eventHandeler;
    }
    
    private class EventHandeler extends AbstractEventHandeler<CoreController> {

        private volatile URI rawBitcoinURI = null;
        
        public EventHandeler(CoreController coreController){
            super(coreController);
        }

        @Override
        public void handleOpenURIEvent(URI rawBitcoinURI) {
            // do nothing
        }

        @Override
        public void handleQuitEvent(ExitAction exitAction) {
            exitAction.setCoreController(super.controller);
        }
    }
    
    
    
}
