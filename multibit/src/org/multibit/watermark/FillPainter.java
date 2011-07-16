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

/**
 * An extension of <code>WatermarkPainter</code> that
 * tiles an image to fill the entire component.
 *
 * @version 1.2 10/25/2006
 * @author Shannon Hickey
 */
public class FillPainter extends WatermarkPainter {

    /** The image to paint in the background */
    private Image bgImage;

    public FillPainter() {
        bgImage = getImage(getClass().getResource("/images/bitcoin-watermark.jpg"));
    }

    public String[] getCommands() {
        return new String[] {"beach.jpg",
                             "rocks_waves.jpg",
                             "green_swirl.jpg",
                             "fire_ring.jpg"};
    }

    public void paint(Graphics g) {
        // if a background image exists, paint it
        if (bgImage != null) {
            int width = getComponent().getWidth();
            int height = getComponent().getHeight();
            int imageW = bgImage.getWidth(null);
            int imageH = bgImage.getHeight(null);

            // we'll tile the image to fill our area
            for (int x = 0; x < width; x += imageW) {
                for (int y = 0; y < height; y += imageH) {
                    g.drawImage(bgImage, x, y, getComponent());
                }
            }
        }
    }

    public void actionPerformed(ActionEvent ae) {
        bgImage = getImage(getClass().getResource(ae.getActionCommand()));
        getComponent().repaint();
    }

}
