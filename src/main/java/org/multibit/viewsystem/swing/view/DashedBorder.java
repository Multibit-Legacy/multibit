package org.multibit.viewsystem.swing.view;

import javax.swing.border.Border;
import java.awt.*;

class DashedBorder implements Border {
    public static final int THICKNESS = 2;
    Color color;
    int dashWidth;
    int dashHeight;

    public DashedBorder() {
        this(Color.LIGHT_GRAY, 4, 4);
    }

    public DashedBorder(Color c, int width, int height) {
        if (width < 1) {
            throw new IllegalArgumentException("Invalid width: " + width);
        }
        if (height < 1) {
            throw new IllegalArgumentException("Invalid height: " + height);
        }
        color = c;
        dashWidth = width;
        dashHeight = height;
    }

    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        Insets insets = getBorderInsets(c);
        g.setColor(color);
//        int numWide = (int) Math.round(width / dashWidth);
        int numHigh = Math.round(height / dashHeight);
        int startPoint;
//        for (int i = 0; i <= numWide; i += 2) {
//            startPoint = x + dashWidth * i;
//            g.fillRect(startPoint, y, dashWidth, THICKNESS);
//            g.fillRect(startPoint, y + height - insets.bottom, dashWidth, THICKNESS);
//        }
        for (int i = 0; i <= numHigh; i += 2) {
            startPoint = x + dashHeight * i;
            //g.fillRect(x, startPoint, THICKNESS, dashHeight);
            g.fillRect(x + width - insets.right, startPoint, THICKNESS, dashHeight);
        }
    }

    public Insets getBorderInsets(Component c) {
        return new Insets(THICKNESS, THICKNESS, THICKNESS, THICKNESS);
    }

    public boolean isBorderOpaque() {
        return false;
    }
}