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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import java.util.concurrent.CopyOnWriteArrayList;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Localiser;
import org.multibit.model.core.StatusEnum;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public abstract class BaseController<C extends BaseController<C>> implements Controller{
    
    private Logger log = LoggerFactory.getLogger(BaseController.class);
    
    /**
     * The view systems under control of the MultiBitController.
     */
    private final Collection<ViewSystem> viewSystems;
    
    /**
     * The localiser used to localise everything.
     */
    private Localiser localiser;
    
    /**
     * Class encapsulating the location of the Application Data Directory.
     */
    private final ApplicationDataDirectoryLocator applicationDataDirectoryLocator;

    
    private volatile boolean applicationStarting = true;
    
    protected BaseController(){
        this(null);
    }
    
    protected BaseController(ApplicationDataDirectoryLocator applicationDataDirectoryLocator){
        this.applicationDataDirectoryLocator = applicationDataDirectoryLocator;
        
        viewSystems = new CopyOnWriteArrayList<ViewSystem>();
        
        // By default localise to English.
        localiser = new Localiser(Locale.ENGLISH);
    }
    

    @Override
    public final Collection<ViewSystem> getViewSystem() {
        return viewSystems;
    }
    
    /**
     * Register a new MultiBitViewSystem from the list of views that are managed.
     * 
     * @param viewSystem
     *            system
     */
    public final void registerViewSystem(ViewSystem viewSystem) {
        viewSystems.add(viewSystem);
    }
    
    @Override
    public final Localiser getLocaliser() {
        return localiser;
    }
    
    public final void setLocaliser(Localiser localiser) {
        this.localiser = localiser;
    }
    
    @Override
    public final void setOnlineStatus(StatusEnum statusEnum) {
        //log.debug("setOnlineStatus called");
        for (ViewSystem viewSystem : this.getViewSystem()) {
            viewSystem.setOnlineStatus(statusEnum);
        }
    }
    
    @Override
    public final boolean getApplicationStarting(){
        return this.applicationStarting;
    }
    
    @Override
    public final ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator(){
        return applicationDataDirectoryLocator;
    }
    
    public final void setApplicationStarting(boolean applicationStarting){
        this.applicationStarting = applicationStarting;
    }

    /**
     * Fire that the model data has changed.
     */
    @Override
    public final void fireDataChangedUpdateNow() {
        //log.debug("fireDataChangedUpdateNow called");
        for (ViewSystem viewSystem : this.getViewSystem()) {
            viewSystem.fireDataChangedUpdateNow(DisplayHint.COMPLETE_REDRAW);
        }
    }
    
    /**
     * Fire that the model data has changed and similar events are to be collapsed.
     */
    @Override
    public final void fireDataChangedUpdateLater() {
        for (ViewSystem viewSystem : this.getViewSystem()) {
            viewSystem.fireDataChangedUpdateLater(DisplayHint.WALLET_TRANSACTIONS_HAVE_CHANGED);
        }
    }
    
    /**
     * Fire that all the views need recreating.
     */
    @Override
    public final void fireRecreateAllViews(boolean initUI) {
        //log.debug("fireRecreateAllViews called");
        // tell the viewSystems to refresh their views
        for (ViewSystem viewSystem : this.getViewSystem()) {
            viewSystem.recreateAllViews(initUI, getCurrentView());
        }
    }
    
    @Override
    abstract public void fireDataStructureChanged();
    
    /**
     * @return True if the application can quit
     */
    protected final boolean isOKToQuit() {
        return true;
    }

    @Override
    abstract public View getCurrentView();
    
    @Override
    abstract public void setCurrentView(View view);
    
    @Override
    abstract public void displayView(View viewToDisplay);
    
    @Override
    abstract public void displayHelpContext(String helpContextToDisplay);
    
    abstract protected void addEventHandler(AbstractEventHandler eventHandler);

    

}
