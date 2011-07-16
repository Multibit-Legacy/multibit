/*
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 
 * - Redistribution in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials
 *   provided with the distribution.
 * 
 * Neither the name of Sun Microsystems, Inc. or the names of
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * 
 * This software is provided "AS IS," without a warranty of any
 * kind. ALL EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND
 * WARRANTIES, INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY
 * EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE LIABLE FOR ANY
 * DAMAGES OR LIABILITIES SUFFERED BY LICENSEE AS A RESULT OF OR
 * RELATING TO USE, MODIFICATION OR DISTRIBUTION OF THIS SOFTWARE OR
 * ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE
 * FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT, INDIRECT,
 * SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
 * CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF
 * THE USE OF OR INABILITY TO USE THIS SOFTWARE, EVEN IF SUN HAS
 * BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * You acknowledge that this software is not designed, licensed or
 * intended for use in the design, construction, operation or
 * maintenance of any nuclear facility.
 */

package org.multibit.watermark;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.net.URL;

import javax.imageio.ImageIO;

/**
 * This class is the abstract base class for objects that know
 * how to paint a <code>WatermarkViewport</code>. Minimally,
 * subclasses must override the <code>paint</code> method.
 * <p>
 * Note: <code>WatermarkPainter</code>s are not designed to
 * be shared between viewports.
 *
 * @version 1.2 10/25/2006
 * @author Shannon Hickey
 */
public abstract class WatermarkPainter implements ActionListener, HierarchyListener {

    /** The <code>WatermarkViewport</code> that this painter is associated with. */
    private WatermarkViewport paintComp;

    /**
     * Called when this painter is registered with a viewport,
     * to notify us of the component that we'll be painting on.
     * This will replace the previous component if non-null.
     *
     * @param comp the viewport that this painter is associated with,
     *             or null to simply remove the existing component
     */
    final void setComponent(WatermarkViewport comp) {
        if (paintComp != null) {
            paintComp.removeHierarchyListener(this);
            stop();
        }
        
        paintComp = comp;
        
        if (paintComp != null) {
            if (paintComp.isShowing()) {
                start();
            }
            paintComp.addHierarchyListener(this);
        }
    }
    
    /**
     * Return the viewport associated with this painter.
     *
     * @return the viewport associated with this painter
     */
    protected final WatermarkViewport getComponent() {
        return paintComp;
    }

    /**
     * Return the list of commands that can be sent to this
     * painter. These commands will be sent in the form
     * of an actionCommand on an <code>java.awt.ActionEvent</code>
     * sent to <code>actionPerformed</code>.
     *
     *
     * @return the commands which this painter understands
     */
    public String[] getCommands() {
        return null;
    };

    /**
     * Perform the action given by the actionCommand
     * of the specified <code>ActionEvent</code>. Should
     * support the command list returned by
     * <code>getCommands</code>.
     *
     * @param ae the <code>ActionEvent</code> containing
     *           a command to perform
     */
    public void actionPerformed(ActionEvent ae) {
    }
    
    /**
     * Starts any animation that this painter might perform.
     * This is called when this painter is registered with a
     * new viewport, if the viewport's showing status is true.
     * It will then be called every time that component's status
     * changes to true.
     */
    protected void start() {
    }
    
    /**
     * Stops any animation that this painter might be performing.
     * This is called when this painter is un-registered, or when
     * the showing status of the current viewport changes to false.
     */
    protected void stop() {
    }

    /**
     * Paint onto the graphics object indicated by the
     * parameter. This method can query the component
     * returned by <code>getComponent</code> for size
     * information. The component returned is
     * guaranteed to be non-null as <code>setComponent</code>
     * will always be called with a non-null component
     * before any painting is done.
     *
     * @param g the graphics object on which to paint
     */
    protected abstract void paint(Graphics g);
    
    /**
     * Convenience method to load an image from the given URL.
     * This implementation uses <CODE>ImageIO</CODE> to load
     * the image and thus returns <CODE>BufferedImages</CODE>.
     *
     * @param imageURL the URL to an image
     * @return the image or null if the image couldn't be loaded
     */
    protected static Image getImage(URL imageURL) {
        Image image = null;

        try {
            // use ImageIO to read in the image
            image = ImageIO.read(imageURL);
        } catch (Exception ioe) {
            ioe.printStackTrace();
        }

        return image;
    }
    
    /**
     * Listens for hierarchy events on the current viewport to start
     * or stop this painter when the component's showing state changes.
     */
    public void hierarchyChanged(HierarchyEvent he) {
        if ((he.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != HierarchyEvent.SHOWING_CHANGED) {
            return;
        }
        
        if (paintComp.isShowing()) {
            start();
        } else {
            stop();
        }
    }

}
