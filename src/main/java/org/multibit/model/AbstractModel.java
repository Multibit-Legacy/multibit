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
package org.multibit.model;

import java.util.Properties;
import org.multibit.viewsystem.View;

/**
 *
 * @author Cameron Garnham
 */
public abstract class AbstractModel<M extends BaseModel<M>> implements Model {

    private final M baseModel;

    protected AbstractModel(M baseModel) {
        if (null == baseModel) {
            throw new NullPointerException();
        }
        this.baseModel = baseModel;
    }

    @Override
    public String getUserPreference(String key) {
        return this.baseModel.getUserPreference(key);
    }

    @Override
    public void setUserPreference(String key, String value) {
        this.baseModel.setUserPreference(key, value);
    }

    @Override
    public Properties getAllUserPreferences() {
        return this.baseModel.getAllUserPreferences();
    }

    @Override
    public View getCurrentView() {
        return this.baseModel.getCurrentView();
    }

    @Override
    public void setCurrentView(View view) {
        this.baseModel.setCurrentView(view);
    }
    
}
