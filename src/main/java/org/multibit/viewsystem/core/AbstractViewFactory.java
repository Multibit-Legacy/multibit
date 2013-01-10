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
package org.multibit.viewsystem.core;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Cameron Garnham
 */
public abstract class AbstractViewFactory {
    protected Map<Integer, IView> viewMap;

    public AbstractViewFactory() {
        initialise();
    }
    
    public final void initialise() {
        viewMap = new HashMap<Integer, IView>();
    }

    public void addView(int viewNumber, MultiBitView view) {
        viewMap.put(viewNumber, view);
    }

    public IView getView(int viewNumber) {
        IView viewToReturn = viewMap.get(viewNumber);
        if (viewToReturn == null) {
            viewToReturn = createView(viewNumber);
        }
        return viewToReturn;
    }
    
    protected abstract IView createView(int viewNumber);
}
