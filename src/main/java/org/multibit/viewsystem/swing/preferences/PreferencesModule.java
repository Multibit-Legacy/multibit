/*
 * The MIT License
 *
 * Copyright 2013 Development.
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
package org.multibit.viewsystem.swing.preferences;

import java.awt.FontMetrics;
import java.util.Set;
import javax.swing.JPanel;
import org.multibit.viewsystem.swing.preferences.PreferencesPanel.RedrawCallback;

/**
 *
 * @author Cameron Garnham
 */
public interface PreferencesModule {
    
    void Setup(RedrawCallback redrawCallback, FontMetrics fontMetrics);
    
    Set<JPanel> Init() throws SetupNotCalledException;
    
    void Update() throws SetupNotCalledException;

    void Submit() throws SetupNotCalledException, ValidationException;

    void Undo() throws SetupNotCalledException;

    
    class SetupNotCalledException extends Exception{
        public SetupNotCalledException(String message) {
            super(message);
        }
    }
    
    class ValidationException extends Exception {
        public ValidationException(String message) {
            super(message);
        }
    }
}
