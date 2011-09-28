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

package org.multibit.viewsystem.swing.watermark;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JViewport;

/**
 * An extension of <code>javax.swing.JViewport</code>
 * that allows pluggable custom painting of its background
 * and foreground. Since the painting is done on the
 * viewport itself, it will not scroll when the view is
 * scrolled. The resulting effect is that the view seems
 * to float in space between the fixed background and
 * foreground.
 * <p> 
 * It is important to remember to set the opacity of
 * the component that this viewport displays to false,
 * so that the viewport's background can show through.
 * <p>
 * Example of use with a JEditorPane:
 *
 * <pre>
 * JEditorPane ep = new JEditorPane();
 *
 * // important so that we can see through the JEditorPane
 * ep.setOpaque(false);
 *
 * JScrollPane sp = new JScrollPane();
 * WatermarkViewport vp = new WatermarkViewport(bgPainter, fgPainter);
 * vp.setView(ep);
 * sp.setViewport(vp);
 * </pre>
 *
 * @version 1.2 10/25/2006
 * @author Shannon Hickey
 */
public class WatermarkViewport extends JViewport {
    
    private static final long serialVersionUID = -2551126151376191210L;

    /** The background painter */
    private WatermarkPainter bgPainter;
    
    /** The foreground painter */
    private WatermarkPainter fgPainter;

    /**
     * Creates a WatermarkViewport with the specified background
     * and foreground painters.
     *
     * @param  bgPainter   the painter that will paint the background
     * @param  fgPainter   the painter that will paint the foreground
     */
    public WatermarkViewport(WatermarkPainter bgPainter, WatermarkPainter fgPainter) {
        setBackgroundPainter(bgPainter);
        setForegroundPainter(fgPainter);
        setBackground(Color.WHITE);
    }

    /**
     * overriding paintComponent allows us to paint
     * the custom background below the scrolling content.
     */
    public void paintComponent(Graphics g) {
        // do the superclass behavior first
        super.paintComponent(g);
        
        // any custom background painting should occur here
        // we'll call the background painter to paint the custom background
        if (bgPainter != null) {
            bgPainter.paint(g);
        }
    }
    
    /**
     * overriding paintChildren allows us to paint
     * the custom foreground above the scrolling content
     */
    public void paintChildren(Graphics g) {
        // paint the children first
        super.paintChildren(g);
        
        // any custom foreground painting should occur here
        // we'll call the foreground painter to paint the custom foreground
        if (fgPainter != null) {
            fgPainter.paint(g);
        }
    }

    /**
     * Set the painter to use to paint the background.
     *
     * @param  painter  the painter that will paint the background
     */
    public void setBackgroundPainter(WatermarkPainter painter) {
        if (bgPainter != null) {
            bgPainter.setComponent(null);
        }
        
        bgPainter = painter;
        
        if (bgPainter != null) {
            bgPainter.setComponent(this);
        }

        repaint();
    }

    /**
     * Set the painter to use to paint the foregreound.
     *
     * @param  painter  the painter that will paint the foreground
     */
    public void setForegroundPainter(WatermarkPainter painter) {
        if (fgPainter != null) {
            fgPainter.setComponent(null);
        }
        
        fgPainter = painter;
        
        if (fgPainter != null) {
            fgPainter.setComponent(this);
        }

        repaint();
    }
    
    /**
     * Return the painter that paints the background.
     *
     * @return the painter used for the background
     */
    public WatermarkPainter getBackgroundPainter() {
        return bgPainter;
    }
    
    /**
     * Return the painter that paints the foreground.
     *
     * @return the painter used for the foreground
     */    
    public WatermarkPainter getForegroundPainter() {
        return fgPainter;
    }

}
