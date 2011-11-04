package org.multibit.viewsystem.swing.view;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.StringTokenizer;

import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.TransferHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ImageSelection extends TransferHandler implements Transferable {
    private static final long serialVersionUID = 756395092284264645L;

    private static final Logger log = LoggerFactory.getLogger(ImageSelection.class);

    private DataFlavor urlFlavor;
    private DataFlavor uriListAsStringFlavor;
    private DataFlavor uriListAsReaderFlavor;

    private DataFlavor flavors[];

    private Image image;

    private AbstractTradePanel abstractTradePanel;
    private boolean canImport;

    public ImageSelection(AbstractTradePanel abstractTradePanel, boolean canImport) {
        this.abstractTradePanel = abstractTradePanel;
        this.canImport = canImport;

        try {
            urlFlavor = new DataFlavor("application/x-java-url; class=java.net.URL");
            uriListAsStringFlavor = new DataFlavor("text/uri-list; class=java.lang.String");
            uriListAsReaderFlavor = new DataFlavor("text/uri-list;class=java.io.Reader");
            flavors = new DataFlavor[] { DataFlavor.imageFlavor, urlFlavor, uriListAsStringFlavor, uriListAsReaderFlavor };
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    public int getSourceActions(JComponent c) {
        return TransferHandler.COPY;
    }

    public boolean canImport(JComponent comp, DataFlavor flavor[]) {
        if (!canImport) {
            return false;
        }

        if (!(comp instanceof JLabel) && !(comp instanceof AbstractButton)) {
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
        image = null;

        if (comp instanceof JLabel) {
            JLabel label = (JLabel) comp;
            Icon icon = label.getIcon();
            if (icon instanceof ImageIcon) {
                image = ((ImageIcon) icon).getImage();
                return this;
            }
        }
        return null;
    }

    public boolean importData(JComponent comp, Transferable transferable) {
        if (comp instanceof JLabel) {
            log.debug("importData - 1");

            JLabel label = (JLabel) comp;
            image = getDropData(transferable, label);
            log.debug("importData - 2 - image = " + image);

            return abstractTradePanel.processDroppedImage(image);

        }
        return false;
    }

    // Transferable
    public Object getTransferData(DataFlavor flavor) {
        if (isDataFlavorSupported(flavor)) {
            if (DataFlavor.imageFlavor.equals(flavor)) {
                return image;
            }

            if (DataFlavor.javaFileListFlavor.equals(flavor)) {
                java.util.List<File> list = new java.util.LinkedList<File>();

                // write the image to the output stream
                File swatchFile;
                try {
                    swatchFile = File.createTempFile("swatch", ".png");
                    ImageIO.write(toBufferedImage(image, -1, -1), "png", swatchFile);
                    list.add(swatchFile);
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
                return list;
            }

            if (uriListAsStringFlavor.equals(flavor)) {
                log.debug("uriListAsStringFlavor is supported");
                // write the image to the output stream
                File swatchFile;
                try {
                    swatchFile = File.createTempFile("swatch", ".png");
                    ImageIO.write(toBufferedImage(image, -1, -1), "png", swatchFile);
                    return "file://" + swatchFile.getAbsolutePath() + "\r\n";
                 } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }

        }
        return null;
        
        //java.awt.datatransfer.DataFlavor[mimetype=image/x-pict;representationclass=java.io.InputStream], 
        //java.awt.datatransfer.DataFlavor[mimetype=image/x-java-image;representationclass=java.awt.Image], 

    }

    public DataFlavor[] getTransferDataFlavors() {
        return flavors;
    }

    public boolean isDataFlavorSupported(DataFlavor flavor) {
        for (int j = 0, m = flavors.length; j < m; j++) {
            if (flavor.equals(flavors[j])) {
                return true;
            }
        }
        return false;
    }

    @SuppressWarnings("rawtypes")
    private Image getDropData(Transferable transferable, JComponent label) {
        try {
            // try to get an image
            if (transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
                log.debug("image flavor is supported");
                Image img = (Image) transferable.getTransferData(DataFlavor.imageFlavor);
                if (img != null && img.getWidth(null) != -1) {
                    return img;
                }
            }
            if (transferable.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                log.debug("javaFileList is supported");
                java.util.List list = (java.util.List) transferable.getTransferData(DataFlavor.javaFileListFlavor);
                for (Object aList : list) {
                    File f = (File) aList;
                    ImageIcon icon = new ImageIcon(f.getAbsolutePath());
                    if (icon.getImage() != null) {
                        return icon.getImage();
                    }
                }
            }

            if (transferable.isDataFlavorSupported(uriListAsStringFlavor)) {
                log.debug("uriListAsStringFlavor is supported");
                String uris = (String) transferable.getTransferData(uriListAsStringFlavor);

                // url-lists are defined by rfc 2483 as crlf-delimited
                // TODO iterate over list for all of them
                StringTokenizer izer = new StringTokenizer(uris, "\r\n");
                if (izer.hasMoreTokens()) {
                    String uri = izer.nextToken();

                    if (uri.startsWith("#") || uri.isEmpty()) {
                        // comment line, by RFC 2483
                    } else {
                        log.debug("uri = " + uri);
                        java.awt.Image image = getURLImage(new URL(uri));

                        if (image != null) {
                            return image;
                        }

                        ImageIcon uriIcon = new ImageIcon(uri);
                        if (uriIcon.getImage() != null) {
                            return uriIcon.getImage();
                        }
                    }
                }
            }

            if (transferable.isDataFlavorSupported(uriListAsReaderFlavor)) {
                log.debug("uriListAsReaderFlavor is supported");

                BufferedReader read = new BufferedReader(uriListAsReaderFlavor.getReaderForText(transferable));
                // Remove 'file://' from file name
                String fileName = read.readLine().substring(7).replace("%20", " ");
                // Remove 'localhost' from OS X file names
                if (fileName.substring(0, 9).equals("localhost")) {
                    fileName = fileName.substring(9);
                }
                read.close();

                java.awt.Image image = getFileImage(new File(fileName));

                if (image != null) {
                    return image;
                }
            }

            if (transferable.isDataFlavorSupported(urlFlavor)) {
                log.debug("urlFlavor is supported");
                URL url = (URL) transferable.getTransferData(urlFlavor);
                log.debug("url = " + url);
                java.awt.Image image = getURLImage(url);

                if (image != null) {
                    return image;
                }

                ImageIcon urlIcon = new ImageIcon(url);
                if (urlIcon.getImage() != null) {
                    return urlIcon.getImage();
                }
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (UnsupportedFlavorException e) {

            e.printStackTrace();
        }
        return null;
    }

    private Image getURLImage(URL url) {
        Image imageToReturn = null;

        try {
            imageToReturn = ImageIO.read(url);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return imageToReturn;
    }

    private Image getFileImage(File file) {
        Image imageToReturn = null;

        try {
            imageToReturn = ImageIO.read(file);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return imageToReturn;
    }

    private BufferedImage toBufferedImage(Image image, int width, int height) {
        log.debug("toBufferedImage - 1");
        if (image == null) {
            return null;
        }
        if (width == -1) {
            width = image.getWidth(null);
        }
        if (height == -1) {
            height = image.getHeight(null);
        }
        // draw original image to thumbnail image object and
        // scale it to the new size on-the-fly
        log.debug("toBufferedImage - 2.2, image = " + image + ",width = " + width + ", height = " + height);

        BufferedImage bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        log.debug("toBufferedImage - 2.3, bufferedImage = " + bufferedImage);

        Graphics2D g2 = bufferedImage.createGraphics();

        log.debug("toBufferedImage - 3");
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2.drawImage(image, 0, 0, width, height, null);
        log.debug("toBufferedImage - 4");
        g2.dispose();
        return bufferedImage;
    }
}
