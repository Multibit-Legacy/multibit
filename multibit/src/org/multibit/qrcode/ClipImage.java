package org.multibit.qrcode;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Image;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.TransferHandler;

public class ClipImage {

    public static void main(String args[]) {

        JFrame frame = new JFrame("Clip Image");
        Container contentPane = frame.getContentPane();

        final Clipboard clipboard = frame.getToolkit().getSystemClipboard();

        Icon icon = new ImageIcon("/images/openWallet.png");
        final JLabel label = new JLabel(icon);
        label.setTransferHandler(new ImageSelection2());

        JScrollPane pane = new JScrollPane(label);
        contentPane.add(pane, BorderLayout.CENTER);

        JButton copy = new JButton("Copy");
        copy.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                TransferHandler handler = label.getTransferHandler();
                handler.exportToClipboard(label, clipboard, TransferHandler.COPY);
            }
        });

        JButton clear = new JButton("Clear");
        clear.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                label.setIcon(null);
            }
        });

        JButton paste = new JButton("Paste");
        paste.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                Transferable clipData = clipboard.getContents(clipboard);
                if (clipData != null) {
                    if (clipData.isDataFlavorSupported(DataFlavor.imageFlavor)) {
                        TransferHandler handler = label.getTransferHandler();
                        handler.importData(label, clipData);
                    }
                }
            }
        });

        JPanel p = new JPanel();
        p.add(copy);
        p.add(clear);
        p.add(paste);
        contentPane.add(p, BorderLayout.SOUTH);
        frame.setSize(300, 300);
        frame.show();
    }
}

class ImageSelection2 extends TransferHandler implements Transferable {

    private static final DataFlavor flavors[] = { DataFlavor.imageFlavor };

    private JLabel source;

    private Image image;

    public int getSourceActions(JComponent c) {
        return TransferHandler.COPY;
    }

    public boolean canImport(JComponent comp, DataFlavor flavor[]) {
        if (!(comp instanceof JLabel)) {
            return false;
        }
        for (int i = 0, n = flavor.length; i < n; i++) {
            for (int j = 0, m = flavors.length; j < m; j++) {
                if (flavor[i].equals(flavors[j])) {
                    return true;
                }
            }
        }
        return false;
    }

    public Transferable createTransferable(JComponent comp) {
        // Clear
        source = null;
        image = null;

        if (comp instanceof JLabel) {
            JLabel label = (JLabel) comp;
            Icon icon = label.getIcon();
            if (icon instanceof ImageIcon) {
                image = ((ImageIcon) icon).getImage();
                source = label;
                return this;
            }
        }
        return null;
    }

    public boolean importData(JComponent comp, Transferable t) {
        if (comp instanceof JLabel) {
            JLabel label = (JLabel) comp;
            if (t.isDataFlavorSupported(flavors[0])) {
                try {
                    image = (Image) t.getTransferData(flavors[0]);
                    ImageIcon icon = new ImageIcon(image);
                    label.setIcon(icon);
                    return true;
                } catch (UnsupportedFlavorException ignored) {
                } catch (IOException ignored) {
                }
            }
        }
        return false;
    }

    // Transferable
    public Object getTransferData(DataFlavor flavor) {
        if (isDataFlavorSupported(flavor)) {
            return image;
        }
        return null;
    }

    public DataFlavor[] getTransferDataFlavors() {
        return flavors;
    }

    public boolean isDataFlavorSupported(DataFlavor flavor) {
        return flavor.equals(DataFlavor.imageFlavor);
    }
}
