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

/**
 *
 * This defines a extention that can extend a View
 * 
 * 
 * @author Cameron Garnham
 */
public interface Module<P extends ModularView, K extends Enum<K>> {
    
    /**
     * Sets parent view.
     * 
     * @param view
     */
    void setParentView(Viewable view);
    
    /**
     * Get Viewable parent
     * 
     * @return
     * @throws NullPointerException
     */
    Viewable getParentView() throws NullPointerException;
    
    /**
     * Called when this module is added.
     * 
     * @param panel
     */
    void onBegin(final P panel);

    
    /**
     * Called a the end of the main 'init'
     * 
     * @param panel
     */
    void onFinishInit(final P panel);
    
    void displayView();
    
    Boolean getIsInitialised();
   
    K getModuleEnum();
    
}
