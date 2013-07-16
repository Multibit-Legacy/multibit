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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public abstract class BaseModel<M extends BaseModel<M>> implements Model {

    private static final Logger log = LoggerFactory.getLogger(BaseModel.class);
    
    // User preferences.
    public static final String SELECTED_VIEW = "selectedView";
    public static final String SELECTED_VIEW_ENUM = "selectedViewEnum";
    public static final String PREVIOUSLY_SELECTED_VIEW = "previousView";
    
    
    /**
     * Where most of the local-user settings are stored.
     */
    protected Properties userPreferences;
    
    /**
     * The currently displayed view. One of the View constants.
     */
    private View currentView = null;

    protected BaseModel(Properties userPreferences) {

        if (null == userPreferences) {
            throw new NullPointerException();
        }

        this.userPreferences = userPreferences;

        setSavedView();
    }

    private void setSavedView() {
        // Initialize everything to look at the stored opened view.
        // If no properties passed in just initialize to the default view.
        if (userPreferences != null) {
            // first try and find a old view setting.
            View initialViewInProperties = null;
            Object oldViewObject = userPreferences.get(BaseModel.SELECTED_VIEW);

            String oldViewString = (null != oldViewObject) ? (String) oldViewObject : null;

            if (null != oldViewString) {
                Integer oldViewInt = null;
                try {
                    oldViewInt = Integer.parseInt(oldViewString);
                } catch (NumberFormatException nfe) {
                    // do nothing
                } finally {
                    initialViewInProperties = View.parseOldView(oldViewInt);
                    
                    // Remove the old view property from the properties - replaced by enum.
                    // (It may be put back in for backwads compatibility in FileHandler#writeUserPreferences.
                    userPreferences.remove(BaseModel.SELECTED_VIEW);
                }
            }

            // If oldViewInProperties is still null,  try and find the view.
            if (null == initialViewInProperties) {
                Object viewObject = userPreferences.get(BaseModel.SELECTED_VIEW_ENUM);

                String viewString = (null != viewObject) ? (String) viewObject : null;

                if (viewString != null) {
                    try {
                        View viewEnum = View.valueOf(viewString);
                        initialViewInProperties = (!viewEnum.isObsolete()) ? viewEnum : null;

                    } catch (IllegalArgumentException nfe) {
                        // do nothing.
                    }
                }
            }
            setCurrentView((null != initialViewInProperties) ? initialViewInProperties : View.DEFAULT_VIEW());
            log.debug("Initial view from properties file is '" + getCurrentView().toString() + "'");
        }
    }

    /**
     * Get a user preference.
     *
     * @param key String key of property
     * @return String property value
     */
    @Override
    public final String getUserPreference(String key) {
        return userPreferences.getProperty(key);
    }

    /**
     * Set a user preference.
     *
     * @return
     */
    @Override
    public void setUserPreference(String key, String value) {
        if (key != null && value != null) {
            userPreferences.put(key, value);
        }
    }

    /**
     * Get all user preference.
     *
     * @return
     */
    @Override
    public Properties getAllUserPreferences() {
        return userPreferences;
    }

    /**
     * Set all user preferences.
     */
    public void setAllUserPreferences(Properties properties) {
        userPreferences = properties;
    }

    @Override
    public final View getCurrentView() {
        return currentView;
    }

    @Override
    public final void setCurrentView(View view) {
        this.currentView = view;
        this.setUserPreference(BaseModel.SELECTED_VIEW_ENUM, view.name());
    }
}
