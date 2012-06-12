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

import java.net.URL;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class ActivatedHyperlinkListener implements HyperlinkListener {

    private static final String MULTIBIT_HOST_NAME = "www.multibit.org";

    private static final String HTTP_PROTOCOL = "http";

    private static final String SPACER = "   "; // 3 spaces

    MultiBitFrame mainFrame;

    Browser browser;

    String currentUrl;

    public ActivatedHyperlinkListener(MultiBitFrame frame, Browser browser, String currentUrl) {
        this.mainFrame = frame;
        this.browser = browser;
        this.currentUrl = currentUrl;
    }

    public void hyperlinkUpdate(HyperlinkEvent hyperlinkEvent) {
        HyperlinkEvent.EventType type = hyperlinkEvent.getEventType();
        final URL url = hyperlinkEvent.getURL();
        if (type == HyperlinkEvent.EventType.ENTERED) {
            Message message = new Message(SPACER + url.toString(), true);
            MessageManager.INSTANCE.addMessage(message);
        } else if (type == HyperlinkEvent.EventType.EXITED) {
            Message message = new Message(SPACER + currentUrl, true);
            MessageManager.INSTANCE.addMessage(message);
        } else if (type == HyperlinkEvent.EventType.ACTIVATED) {
            Runnable runner = new Runnable() {
                public void run() {
                    if (HTTP_PROTOCOL.equals(url.getProtocol()) && MULTIBIT_HOST_NAME.equals(url.getHost())) {
                        browser.visit(url.toString());
                        currentUrl = url.toString();
                    } else {
                        JOptionPane.showMessageDialog(mainFrame, "The help contents can only show HTTP content from "
                                + MULTIBIT_HOST_NAME + "\nPlease use your main browser to view the URL:\n" + url.toString(),
                                "Cannot follow link", JOptionPane.INFORMATION_MESSAGE);

                    }
                }
            };
            SwingUtilities.invokeLater(runner);
        }
    }
}
