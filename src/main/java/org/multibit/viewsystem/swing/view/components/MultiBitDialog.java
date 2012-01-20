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
package org.multibit.viewsystem.swing.view.components;

import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Method;
/**
 * JDialog with some utility methods to position on screen

 * @author jim
 *
 */
abstract public class MultiBitDialog extends JDialog {

    private static final Logger log = LoggerFactory.getLogger(MultiBitDialog.class);

    private static final long serialVersionUID = -917160539607329862L;

    protected JFrame mainFrame;
    
    public MultiBitDialog(JFrame mainFrame) {
        this.mainFrame = mainFrame;
    }
    
    /**
     * Positions the specified dialog at a position relative to its parent.
     * 
     * @param dialog
     *            the dialog to be positioned.
     * @param horizontalPercent
     *            the relative location.
     * @param verticalPercent
     *            the relative location.
     */
    protected void positionDialogRelativeToParent(final JDialog dialog,
            final double horizontalPercent, final double verticalPercent) {
        final Dimension d = dialog.getSize();
        final Dimension p = mainFrame.getSize();

        final int baseX = mainFrame.getX() - d.width;
        final int baseY = mainFrame.getY() - d.height;
        final int w = d.width + p.width;
        final int h = d.height + p.height;
        int x = baseX + (int) (horizontalPercent * w);
        int y = baseY + (int) (verticalPercent * h);

        // make sure the dialog fits completely on the screen...
        final Rectangle s = getMaximumWindowBounds();
        x = Math.min(x, (s.width - d.width));
        x = Math.max(x, 0);
        y = Math.min(y, (s.height - d.height));
        y = Math.max(y, 0);

        dialog.setBounds(x + s.x, y + s.y, d.width, d.height);

    }

    /**
     * Computes the maximum bounds of the current screen device. If this method
     * is called on JDK 1.4, Xinerama-aware results are returned. (See
     * Sun-Bug-ID 4463949 for details).
     * 
     * @return the maximum bounds of the current screen.
     */
    protected Rectangle getMaximumWindowBounds() {
        final GraphicsEnvironment localGraphicsEnvironment = GraphicsEnvironment
                .getLocalGraphicsEnvironment();
        try {
            final Method method = GraphicsEnvironment.class.getMethod("getMaximumWindowBounds",
                    (Class[]) null);
            return (Rectangle) method.invoke(localGraphicsEnvironment, (Object[]) null);
        } catch (Exception e) {
            // ignore ... will fail if this is not a JDK 1.4 ..
        }

        final Dimension s = Toolkit.getDefaultToolkit().getScreenSize();
        return new Rectangle(0, 0, s.width, s.height);
    }
    
    /** Returns an ImageIcon, or null if the path was invalid. */
    protected ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            log.error("MultiBitDialog#createImageIcon: Could not find file: '{}'", path);
            return null;
        }
    } 
}
