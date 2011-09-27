package org.multibit.viewsystem.swing.view;

import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;

import org.multibit.viewsystem.swing.MultiBitFrame;

public class AddressesPanel extends JPanel {

    private static final long serialVersionUID = 1992327373249499976L;

    protected void paintComponent(Graphics g)
    {
        Graphics2D g2d = (Graphics2D)g;
        Dimension d = this.getSize();

        g2d.setPaint(new GradientPaint(0, 0, MultiBitFrame.BACKGROUND_COLOR,
            0, d.height, MultiBitFrame.DARK_BACKGROUND_COLOR, true));
        g2d.fillRect(0, 0, d.width , d.height);
    }
}
