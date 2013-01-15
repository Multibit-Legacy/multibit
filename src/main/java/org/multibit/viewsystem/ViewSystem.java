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
package org.multibit.viewsystem;

import org.multibit.viewsystem.swing.FramePlugin;

/**
 *
 * @author Cameron Garnham
 */
public interface ViewSystem {

    /**
     * display the view specified
     * @param view to display - one of the View constants
     */
    void displayView(int viewToDisplay);
    

    /**
     * tells the view system that the model data has changed (but the wallet is still the same)
     */   
    public void fireDataChanged();
    
    /**
     * navigate away from a view - gives the view the opportunity to tidy up/ disappear etc
     * @param viewToNavigateAwayFrom - current view to navigate away from -one of the View constants
     */
    void navigateAwayFromView(int viewToNavigateAwayFrom);

    /**
     * tells the view system to recreate all views e.g. after a language change or wallet change
     * @param initUI Completely redraw everything on all screens = true
     */
    void recreateAllViews(boolean initUI);
    

    /**
     * Set the help context to display
     * @param helpContextToDisplay
     */
    void setHelpContext(String helpContextToDisplay);
    
}
