/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.viewsystem.swing.browser;

/*
 * Animation of loading of URLs:
 * Copyright (c) 2000 David Flanagan.  All rights reserved.
 * This code is from the book Java Examples in a Nutshell, 2nd Edition.
 * It is provided AS-IS, WITHOUT ANY WARRANTY either expressed or implied.
 * You may study, use, and modify it for any non-commercial purpose.
 * You may distribute it non-commercially as long as you retain this notice.
 * For a commercial use license, or to purchase the book (recommended),
 * visit http://www.davidflanagan.com/javaexamples2.
 */

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URL;

import javax.swing.text.Document;
import javax.swing.text.html.HTMLEditorKit;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.HelpContentsPanel;

public class Browser extends javax.swing.JEditorPane implements PropertyChangeListener {
    private static final long serialVersionUID = 1L;

    private String loadingMessage;
    
    /**
     * Creates a new <code>JEditorPane</code>. The document model is set to
     * <code>null</code>.
     */
    public Browser(MultiBitController controller, MultiBitFrame mainFrame, String currentHref) {
        super();

        addHyperlinkListener(new ActivatedHyperlinkListener(mainFrame, this, currentHref));

        loadingMessage = controller.getLocaliser().getString("browser.loadingMessage");
        setEditable(false);
        addPropertyChangeListener(this);
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

        String fontName = controller.getModel().getUserPreference(MultiBitModel.FONT_NAME);
        if (fontName == null || "".equals(fontName)) {
            fontName = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_NAME;
        }
        // add in san-serif as a fallback
        fontName = fontName + ", san-serif";

        int fontSize = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        if (fontSizeString != null && !"".equals(fontSizeString)) {
            try {
                fontSize = Integer.parseInt(fontSizeString);
            } catch (NumberFormatException nfe) {
                // use default
            }
        }
        
        HTMLEditorKit kit = new HTMLEditorKit();
        setEditorKit(kit);
        javax.swing.text.html.StyleSheet styleSheet = kit.getStyleSheet();
        styleSheet.addRule("body {font-size:" + fontSize + "pt; font-family:" + fontName + ";}");
        Document doc = kit.createDefaultDocument();
        setDocument(doc);
        visit(currentHref);
    }

    public static String getLoadingMessage(String href, String loadingMessage) {
        return HelpContentsPanel.SPACER + href + HelpContentsPanel.SPACER + loadingMessage + "...";
    }

    /**
     * This internal method attempts to load and display the specified URL. 
     **/
    public boolean visit(String newHref) {
        try {
            String currentHref = null;
            URL currentUrl = getPage();
            if (currentUrl != null) {
                currentHref = currentUrl.toString();
            }
            // Start animating. Animation is stopped in propertyChanged()
            if (!newHref.equals(currentHref)) {
                startAnimation(getLoadingMessage(newHref, loadingMessage));
            }
            setPage(new URL(newHref)); // Load and display the URL
            return true; // Return success
        } catch (IOException ex) { // If page loading fails
            stopAnimation();
            Message message = new Message("Cannot load page: " + ex.getMessage(), true);
            MessageManager.INSTANCE.addMessage(message);
            return false; // Return failure
        }
    }

    /**
     * This method implements java.beans.PropertyChangeListener. It is invoked
     * whenever a bound property changes in the JEditorPane object. The property
     * we are interested in is the "page" property, because it tells us when a
     * page has finished loading.
     **/
    public void propertyChange(PropertyChangeEvent e) {
        if (e.getPropertyName().equals("page")) {
          // If the page property changed
          // then stop the "loading..." animation
          stopAnimation();
        }
    }

    /**
     * The fields and methods below implement a simple animation in the web
     * browser message line; they are used to provide user feedback while web
     * pages are loading.
     **/
    String animationMessage; // The "loading..." message to display
    int animationFrame = 0; // What "frame" of the animation are we on
    String[] animationFrames = new String[] { // The content of each "frame"
    "-", "\\", "|", "/", "-", "\\", "|", "/", ",", ".", "o", "0", "O", "#", "*", "+" };

    /** This object calls the animate() method 8 times a second */
    javax.swing.Timer animator = new javax.swing.Timer(125, new ActionListener() {
        public void actionPerformed(ActionEvent e) {
            animate();
        }
    });

    /** Display the next frame. Called by the animator timer */
    void animate() {
//        String frame = animationFrames[animationFrame++]; // Get next frame
//        mainFrame.updateStatusLabel(animationMessage + " " + frame, true); // Update
                                                                     // msgline
        animationFrame = animationFrame % animationFrames.length;
    }

    /** Start the animation. Called by the visit() method. */
    void startAnimation(String msg) {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

        animationMessage = msg; // Save the message to display
        animationFrame = 0; // Start with frame 0 of the animation
        animator.start(); // Tell the timer to start firing.
    }

    /** Stop the animation. Called by propertyChanged() method. */
    void stopAnimation() {
        animator.stop(); // Tell the timer to stop firing events
//        mainFrame.updateStatusLabel(" ", true); // Clear the message line
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
}
