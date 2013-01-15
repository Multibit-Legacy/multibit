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
import org.multibit.platform.listener.GenericAboutEventListener;
import org.multibit.platform.listener.GenericOpenURIEvent;
import org.multibit.platform.listener.GenericOpenURIEventListener;
import org.multibit.platform.listener.GenericPreferencesEventListener;
import org.multibit.platform.listener.GenericQuitEventListener;
import org.multibit.viewsystem.ViewSystem;

/**
 *
 * @author Cameron Garnham
 */
public interface Controller extends GenericOpenURIEventListener, GenericPreferencesEventListener,
        GenericAboutEventListener {
    
    public static final String ENCODED_SPACE_CHARACTER = "%20";
    
    public Collection<ViewSystem> getViewSystems();
    public void displayView(int viewToDisplay);
    public void displayHelpContext(String helpContextToDisplay);
    public void registerViewSystem(ViewSystem viewSystem);
    public MultiBitModel getModel();
    public void setModel(MultiBitModel model);
    public void fireRecreateAllViews(boolean initUI);
    public void fireDataChanged();
    public void fireDataStructureChanged();
    public Localiser getLocaliser();
    public void setLocaliser(Localiser localiser);
    public ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator();
    public int getCurrentView();
    public void setCurrentView(int view);
    public void setApplicationStarting(boolean applicationStarting);
    public void handleOpenURI();
    
}
