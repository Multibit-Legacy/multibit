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

/**
 *
 * @author Cameron Garnham
 */
public class CoreModel extends BaseModel<CoreModel> {
    
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
    
    public static final String PREVIOUS_USER_LANGUAGE_CODE = "previousLanguageCode";
    public static final String PREVIOUS_UNDO_CHANGES_TEXT = "previousUndoChangesText";
    public static final String CAN_UNDO_PREFERENCES_CHANGES = "canUndoPreferencesChanges";

    public static final String PREVIOUS_WINDOW_SIZE_H = "previousWindowSizeH";
    public static final String PREVIOUS_WINDOW_SIZE_W = "previousWindowSizeW";
    public static final String PREVIOUS_WINDOW_POS_X = "previousWindowPosX";
    public static final String PREVIOUS_WINDOW_POS_Y = "previousWindowPosY";
    public static final String PREVIOUS_WINDOW_MAX = "previousWindowMax";
    public static final String PREVIOUS_WINDOW_TRAY = "previousWindowTray";

    public static final String MINIMIZE_TO_TRAY = "minimizeToTray";
    
    public CoreModel() {
        this(null);
    }

    public CoreModel(Properties userPreferences) {
        super((null != userPreferences) ? userPreferences : new Properties());
    }

    @Override
    public ModelEnum getModelEnum() {
        return ModelEnum.CORE;
    }
    
}
