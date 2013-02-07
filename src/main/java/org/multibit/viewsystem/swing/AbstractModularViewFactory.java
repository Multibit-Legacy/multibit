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
package org.multibit.viewsystem.swing;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.ModularView;
import org.multibit.viewsystem.ModularViewFactory;
import org.multibit.viewsystem.Module;

/**
 *
 * @author Cameron Garnham
 */
public abstract class AbstractModularViewFactory<V extends ModularView<M>, M extends Module<V,K>, K extends Enum<K>> implements ModularViewFactory<V,M,K> {


    private final Map<K, M> moduleMap;
    
    protected final Class<V> viewClass;
    protected final Class<K> enumClass;
    
    protected final MultiBitController controller;
    protected final MultiBitFrame mainFrame;
    
    
    protected AbstractModularViewFactory(MultiBitController controller, MultiBitFrame mainFrame, Class<V> viewClass, Class<K> enumClass)
    {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.viewClass = viewClass;
        this.enumClass = enumClass;
        
        this.moduleMap = new EnumMap<K, M>(enumClass);
    }
    
    @Override
    public void addAllOf(Set<M> modules) {
        for (M m : modules)
        {
            this.moduleMap.put(m.getModuleEnum(), m);
        }
    }

    @Override
    public void addModule(M module) {
        this.moduleMap.put(module.getModuleEnum(), module);
    }

    @Override
    public void reset() {
        this.moduleMap.clear();
    }

    @Override
    public Collection<M> getModuleCollection() {
        return this.moduleMap.values();
    }

    @Override
    public M getModule(K moduleEnum) {
        return this.moduleMap.get(moduleEnum);
    }

    @Override
    public V getModularView() {

        V view = this.createNewModularView();
        
        if (null != view) {
            for (M m : this.getModuleCollection()) {
                view.addModule(m);
            }

            view.finishInit();

            return view;
        }
        else {
            throw new NullPointerException();
        }
    }

    abstract protected V createNewModularView();
        
// demo code for making this not abstract.
//        try {
//            try {
//                return this.viewClass.getConstructor(MultiBitController.class,MultiBitFrame.class).newInstance(this.controller, this.mainFrame);
//            } catch (NoSuchMethodException ex) {
//                Logger.getLogger(AbstractModularViewFactory.class.getName()).log(Level.SEVERE, null, ex);
//            } catch (SecurityException ex) {
//                Logger.getLogger(AbstractModularViewFactory.class.getName()).log(Level.SEVERE, null, ex);
//            }
//        } catch (InstantiationException ex) {
//            Logger.getLogger(AbstractModularViewFactory.class.getName()).log(Level.SEVERE, null, ex);
//        } catch (IllegalAccessException ex) {
//            Logger.getLogger(AbstractModularViewFactory.class.getName()).log(Level.SEVERE, null, ex);
//        } catch (IllegalArgumentException ex) {
//            Logger.getLogger(AbstractModularViewFactory.class.getName()).log(Level.SEVERE, null, ex);
//        } catch (InvocationTargetException ex) {
//            Logger.getLogger(AbstractModularViewFactory.class.getName()).log(Level.SEVERE, null, ex);
//        }
//        return null;

}
