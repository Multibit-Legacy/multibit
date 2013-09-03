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

import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.MultiBitFrame;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import java.awt.*;
import java.net.URL;

public class ActivatedHyperlinkListener implements HyperlinkListener {

    private static final String MULTIBIT_HOST_NAME = "www.multibit.org";
    private static final String MULTIBIT_HOST_NAME2 = "test.multibit.org";
    private static final String MULTIBIT_HOST_NAME3 = "188.138.113.201";
    private static final String MULTIBIT_HOST_NAME4 = "localhost";
    
    private static final String HTTP_PROTOCOL = "http";
    private static final String HTTPS_PROTOCOL = "https";

    private static final String SPACER = " "; // 3 spaces

    MultiBitFrame mainFrame;

    Browser browser;

    public ActivatedHyperlinkListener(MultiBitFrame frame, Browser browser) {
        this.mainFrame = frame;
        this.browser = browser;
    }

    @Override
    public void hyperlinkUpdate(HyperlinkEvent hyperlinkEvent) {
        HyperlinkEvent.EventType type = hyperlinkEvent.getEventType();
        final URL url = hyperlinkEvent.getURL();
        if (type == HyperlinkEvent.EventType.ENTERED) {
            Message message = new Message(url.toString(), true);
            message.setShowInMessagesTab(false);
            MessageManager.INSTANCE.addMessage(message);
            if (browser.isLoading()) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        browser.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    }});      
            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        browser.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                    }});      
            }
        } else if (type == HyperlinkEvent.EventType.EXITED) {
            Message message = new Message(SPACER, true);
            message.setShowInMessagesTab(false);
            MessageManager.INSTANCE.addMessage(message);
            if (browser.isLoading()) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        browser.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    }});      
            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        browser.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    }});      
            }
        } else if (type == HyperlinkEvent.EventType.ACTIVATED) {
            Runnable runner = new Runnable() {
                @Override
                public void run() {
                    if ((HTTP_PROTOCOL.equals(url.getProtocol()) || HTTPS_PROTOCOL.equals(url.getProtocol())) && 
                            (MULTIBIT_HOST_NAME.equals(url.getHost()) || MULTIBIT_HOST_NAME2.equals(url.getHost()) 
                                    || MULTIBIT_HOST_NAME3.equals(url.getHost()) || MULTIBIT_HOST_NAME4.equals(url.getHost()))) {
                        browser.visit(url.toString(), false);
                    } else {
                        StringBuilder text = new StringBuilder();
                        text.append("The help contents can only show HTTP content from ")
                            .append(MULTIBIT_HOST_NAME)
                            .append(", ")
                            .append(MULTIBIT_HOST_NAME2)
                            .append(" and ")
                            .append(MULTIBIT_HOST_NAME4)
                            .append(".\nPlease use your main browser to view the URL:\n\n")
                            .append(url.toString());
                        JTextArea textArea = new JTextArea(text.toString());
                        textArea.setEditable(false);
                        textArea.setOpaque(false);
                        JOptionPane.showMessageDialog(mainFrame, textArea,
                                "Cannot follow link", JOptionPane.INFORMATION_MESSAGE, ImageLoader.createImageIcon(ImageLoader.EXCLAMATION_MARK_ICON_FILE));

                    }
                }
            };
            SwingUtilities.invokeLater(runner);
        }
    }
}
