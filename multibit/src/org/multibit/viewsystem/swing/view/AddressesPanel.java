package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;

public class AddressesPanel extends JPanel {

    private static final long serialVersionUID = 1992327373249499976L;

    protected void paintComponent(Graphics g)
    {
        Graphics2D g2d = (Graphics2D)g;
        Dimension d = this.getSize();

        Color lighter = new Color(240, 240, 240);
        Color darker = new Color(210, 210, 210);
        g2d.setPaint(new GradientPaint(0, 0, lighter,
            0, d.height, darker, true));
        g2d.fillRect(0, 0, d.width , d.height);
    }
}
