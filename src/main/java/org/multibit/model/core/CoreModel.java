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
package org.multibit.model.core;

import java.util.Properties;
import org.multibit.model.BaseModel;
import org.multibit.model.ModelEnum;
import org.multibit.viewsystem.View;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Cameron Garnham
 */
public class CoreModel extends BaseModel<CoreModel> {

    private static final Logger log = LoggerFactory.getLogger(CoreModel.class);
    
    // User preferences.
    public static final String SELECTED_VIEW = "selectedView";
    public static final String SELECTED_VIEW_ENUM = "selectedViewEnum";
    public static final String PREVIOUSLY_SELECTED_VIEW = "previousView";

    public static final String USER_LANGUAGE_CODE = "languageCode";
    public static final String USER_LANGUAGE_IS_DEFAULT = "isDefault";
    
    public static final String LOOK_AND_FEEL = "lookAndFeel";
    public static final String SYSTEM_LOOK_AND_FEEL = "system";
    
    public static final int SCROLL_INCREMENT = 12;
    
    // User preference font.
    public static final String FONT = "font";
    public static final String FONT_NAME = "fontName";
    public static final String FONT_STYLE = "fontStyle";
    public static final String FONT_SIZE = "fontSize";

    public static final String PREVIOUS_FONT_NAME = "previousFontName";
    public static final String PREVIOUS_FONT_STYLE = "previousFontStyle";
    public static final String PREVIOUS_FONT_SIZE = "previousFontSize";
    
    // User preferences undo.
    public static final String PREVIOUS_USER_LANGUAGE_CODE = "previousLanguageCode";
    public static final String PREVIOUS_UNDO_CHANGES_TEXT = "previousUndoChangesText";
    public static final String CAN_UNDO_PREFERENCES_CHANGES = "canUndoPreferencesChanges";
    
    
    /**
     * The currently displayed view. One of the View constants.
     */
    private View currentView = null;

    public CoreModel() {
        this(null);
    }

    public CoreModel(Properties userPreferences) {
        
        super((null != userPreferences) ? userPreferences : new Properties());

        // Initialize everything to look at the stored opened view.
        // If no properties passed in just initialize to the default view.
        if (super.userPreferences != null) {
            // first try and find a old view setting.
            View initialViewInProperties = null;
            Object oldViewObject = super.userPreferences.get(CoreModel.SELECTED_VIEW);

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
                    super.userPreferences.remove(CoreModel.SELECTED_VIEW);

                }
            }

            // If oldViewInProperties is still null,  try and find the view.
            if (null == initialViewInProperties) {

                Object viewObject = super.userPreferences.get(CoreModel.SELECTED_VIEW_ENUM);
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

    @Override
    public ModelEnum getModelEnum() {
        return ModelEnum.CORE;
    }
    

    public View getCurrentView() {
        return currentView;
    }

    public final void setCurrentView(View view) {
        this.currentView = view;
        super.setUserPreference(SELECTED_VIEW_ENUM, view.name());
    }

    /**
     * Get all user preference.
     *
     * @return
     */
    public Properties getAllUserPreferences() {
        return super.userPreferences;
    }

    /**
     * Set all user preferences.
     */
    public void setAllUserPreferences(Properties properties) {
        super.userPreferences = properties;
    }


}
