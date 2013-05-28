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
import org.multibit.model.Model;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.StatusEnum;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;

/**
 *
 * @author Cameron Garnham
 */
public interface Controller {
    
    Collection<ViewSystem> getViewSystem();
    
    Model getModel();
    
    Localiser getLocaliser();
    
    AbstractEventHandler getEventHandler();
    
    boolean getApplicationStarting();
    
    ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator();
    
    View getCurrentView();
    
    void setCurrentView(View view);
    
    void displayView(View viewToDisplay);
    
    void displayHelpContext(String helpContextToDisplay);
    
    void setOnlineStatus(StatusEnum statusEnum);
    
    void fireDataChangedUpdateNow();
    
    void fireDataChangedUpdateLater();
    
    void fireRecreateAllViews(boolean initUI);
    
    void fireDataStructureChanged();
    
}
