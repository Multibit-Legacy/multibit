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
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.StatusEnum;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;

/**
 *
 * @author Cameron Garnham
 */
public abstract class AbstractController<C extends BaseController<C>> implements Controller {
    
    
    public static final String ENCODED_SPACE_CHARACTER = "%20";
    
    private final C baseController;

    
    protected AbstractController(C baseController){
        this.baseController = baseController;
    }
    
    @Override
    public final Collection<ViewSystem> getViewSystem() {
        return this.baseController.getViewSystem();
    }

    @Override
    public final Localiser getLocaliser() {
        return this.baseController.getLocaliser();
    }

    @Override
    public abstract AbstractEventHandler getEventHandler();

    @Override
    public final boolean getApplicationStarting() {
        return this.baseController.getApplicationStarting();
    }

    @Override
    public final ApplicationDataDirectoryLocator getApplicationDataDirectoryLocator() {
        return this.baseController.getApplicationDataDirectoryLocator();
    }

    @Override
    public final View getCurrentView() {
        return this.baseController.getCurrentView();
    }

    @Override
    public final void setCurrentView(View view) {
        this.baseController.setCurrentView(view);
    }

    @Override
    public final void displayView(View viewToDisplay) {
        this.baseController.displayView(viewToDisplay);
    }

    @Override
    public final void displayHelpContext(String helpContextToDisplay) {
        this.baseController.displayHelpContext(helpContextToDisplay);
    }

    @Override
    public final void setOnlineStatus(StatusEnum statusEnum) {
        this.baseController.setOnlineStatus(statusEnum);
    }

    @Override
    public final void fireDataChangedUpdateNow() {
        this.baseController.fireDataChangedUpdateNow();
    }

    @Override
    public final void fireDataChangedUpdateLater() {
        this.baseController.fireDataChangedUpdateLater();
    }

    @Override
    public final void fireRecreateAllViews(boolean initUI) {
        this.baseController.fireRecreateAllViews(initUI);
    }

    @Override
    public final void fireDataStructureChanged() {
        this.baseController.fireDataStructureChanged();
    }
    
    protected final void addEventHandler(AbstractEventHandler eventHandler) {
        this.baseController.addEventHandler(eventHandler);
    }
    
}
