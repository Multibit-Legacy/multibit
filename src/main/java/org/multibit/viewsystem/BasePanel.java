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

import java.util.HashSet;
import java.util.Set;
import javax.swing.JPanel;
import org.multibit.controller.MultiBitController;

/**
 *
 * This interface defines base views that can be extended.
 * Base views that can be extended have a two-step init stage.
 * 
 * First the constructor is called, this will set up the minimal
 * required state.
 * 
 * Then the extentions are added to the extendable class.
 * 
 * Finally, the finishInit() is called to build the view.
 * 
 * @author Cameron Garnham
 */
public abstract class BasePanel<E extends Extension> extends JPanel implements Viewable{
    
    
    protected Set<E> extentions;
    
    abstract protected MultiBitController getCoreController();
    
    
    protected BasePanel()
    {
        extentions = new HashSet<E>();
    }
    
    
    /**
     * Call Before any Extentions are added.
     */
    protected void beginInit()
    {
      for (E extention : extentions)
        {
            extention.Init(this);
        }
    }
    
    /**
     * Call after all Extentions have been added.
     * Must be overridden.
     */
    abstract protected void finishInit();
    
    
    
    public void addExtention(E ext)
    {
        extentions.add(ext);
    }
    
    @Override
    public void displayView() {
        
        for (E extention : extentions)
        {
            extention.displayView();
        }
    }
    
    
    
}
