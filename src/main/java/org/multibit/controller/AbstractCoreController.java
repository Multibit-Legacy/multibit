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

import java.util.Collection;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Localiser;
import org.multibit.model.MultiBitModel;
import org.multibit.platform.listener.GenericAboutEvent;
import org.multibit.platform.listener.GenericOpenURIEvent;
import org.multibit.platform.listener.GenericPreferencesEvent;
import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitEventListener;
import org.multibit.platform.listener.GenericQuitResponse;
import org.multibit.viewsystem.ViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public abstract class AbstractCoreController implements ICoreController, GenericQuitEventListener {
    
    private ICoreController coreController = null;
    
    public AbstractCoreController(ICoreController coreController)
    {
        this.coreController = coreController;
    }

    @Override
    public Collection<ViewSystem> getViewSystems() {
        return coreController.getViewSystems();
    }
    
    @Override
    public void displayView(int viewToDisplay) {
        coreController.displayView(viewToDisplay);
    }

    @Override
    public void displayHelpContext(String helpContextToDisplay) {
        coreController.displayHelpContext(helpContextToDisplay);
    }

    @Override
    public void registerViewSystem(ViewSystem viewSystem) {
        coreController.registerViewSystem(viewSystem);
    }

    @Override
    public MultiBitModel getModel() {
        return coreController.getModel();
    }

    @Override
    public void setModel(MultiBitModel model) {
        coreController.setModel(model);
    }

    @Override
    public void fireDataStructureChanged() {
        coreController.fireDataChanged();;
    }

    @Override
    public void fireRecreateAllViews(boolean initUI) {
        coreController.fireRecreateAllViews(initUI);
    }

    @Override
    public void fireDataChanged() {
        coreController.fireDataChanged();
    }

    @Override
    public Localiser getLocaliser() {
        return coreController.getLocaliser();
    }

    @Override
    public void setLocaliser(Localiser localiser) {
        coreController.setLocaliser(localiser);
    }

    @Override
    public ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator() {
        return coreController.getApplicationDataDirectoryLocator();
    }

    @Override
    public int getCurrentView() {
        return coreController.getCurrentView();
    }

    @Override
    public void setCurrentView(int view) {
        coreController.setCurrentView(view);
    }

    @Override
    public void setApplicationStarting(boolean applicationStarting) {
        coreController.setApplicationStarting(applicationStarting);
    }

    @Override
    public void onOpenURIEvent(GenericOpenURIEvent event) {
        coreController.onOpenURIEvent(event);
    }
    
    @Override
    public void handleOpenURI()
    {
        coreController.handleOpenURI();
    }

    @Override
    public void onPreferencesEvent(GenericPreferencesEvent event) {
        coreController.onPreferencesEvent(event);
    }

    @Override
    public void onAboutEvent(GenericAboutEvent event) {
        coreController.onAboutEvent(event);
    }

    @Override
    public abstract void onQuitEvent(GenericQuitEvent event, GenericQuitResponse response);
 
    /**
     * @return True if the application can quit
     */
    protected boolean isOKToQuit() {
        return true;
    }
    
}
