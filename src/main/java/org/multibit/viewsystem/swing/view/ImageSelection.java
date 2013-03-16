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
package org.multibit.viewsystem.swing.view;

import org.multibit.viewsystem.swing.view.panels.AbstractTradePanel;
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
import java.util.StringTokenizer;

import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
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
            flavors = new DataFlavor[] { DataFlavor.imageFlavor, DataFlavor.javaFileListFlavor, urlFlavor, uriListAsStringFlavor, uriListAsReaderFlavor };
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    @Override
    public int getSourceActions(JComponent c) {
        return TransferHandler.COPY;
    }

    @Override
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

    @Override
    public Transferable createTransferable(JComponent comp) {
        // Clear.
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

    @Override
    public boolean importData(JComponent comp, Transferable transferable) {
        if (comp instanceof JLabel) {
            //log.debug("importData - 1");

            JLabel label = (JLabel) comp;
            image = getDropData(transferable, label);
            //log.debug("importData - 2 - image = {}", image);

            return abstractTradePanel.processDroppedImage(image);

        }
        return false;
    }

    // Transferable.
    @Override
    public Object getTransferData(DataFlavor flavor) {
        if (isDataFlavorSupported(flavor)) {
            if (DataFlavor.imageFlavor.equals(flavor) && image != null && image.getHeight(null) > 0) {
                return image;
            }

            if (DataFlavor.javaFileListFlavor.equals(flavor)) {
                java.util.List<File> list = new java.util.LinkedList<File>();

                File qrCodeFile = writeImageToFile();
                if (qrCodeFile != null) {
                    list.add(qrCodeFile);
                }

                return list;
            }

            if (uriListAsStringFlavor.equals(flavor)) {
                log.debug("uriListAsStringFlavor is supported");
                File qrCodeFile = writeImageToFile();
                if (qrCodeFile != null) {
                    return "file://" + qrCodeFile.getAbsolutePath() + "\r\n";
                }
            }

        }
        return null;
    }
    
    private File writeImageToFile() {
        // Write the image to the output stream.
        File qrCodeFile = null;
        try {
            // Default name just in case.
            String filename = "";
            boolean areAdding = false;
            if (abstractTradePanel != null) {
                if (abstractTradePanel.getLabel() != null && !"".equals(abstractTradePanel.getLabel())) {
                    filename = abstractTradePanel.getLabel();
                    areAdding = true;
                }
                if (abstractTradePanel.getAmount() != null && !"".equals(abstractTradePanel.getAmount())) {
                    if (areAdding) {
                        filename = filename + " - ";
                    }
                    filename = filename + abstractTradePanel.getAmount();
                    areAdding = true;
                }
                if (abstractTradePanel.getAddress() != null && !"".equals(abstractTradePanel.getAddress())) {
                    if (areAdding) {
                        filename = filename + " - ";
                    }
                    filename = filename + abstractTradePanel.getAddress();
                    areAdding = true;
                }
            }
            
            // Default if everything is missing.
            if ("".equals(filename)) {
                filename = "qrcode" + System.currentTimeMillis() + ".png";
            }

            // Get the temporary directory location.
            File testFile = File.createTempFile("test", "txt");
            testFile.deleteOnExit();
            
            qrCodeFile = new File(testFile.getParent() + File.separator + filename + ".png");
            qrCodeFile.deleteOnExit();
            ImageIO.write(toBufferedImage(image, -1, -1), "png", qrCodeFile);
        } catch (IOException e1) {
            e1.printStackTrace();
        }

        return qrCodeFile;
    }

    @Override
    public DataFlavor[] getTransferDataFlavors() {
        return flavors;
    }

    @Override
    public boolean isDataFlavorSupported(DataFlavor flavor) {
        for (int j = 0, m = flavors.length; j < m; j++) {
            if (flavor.equals(flavors[j])) {
                if (DataFlavor.imageFlavor.equals(flavor)) {
                    if ( image != null && image.getHeight(null) > 0) {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                   return true;
                }
            }
        }
        return false;
    }

    @SuppressWarnings("rawtypes")
    private Image getDropData(Transferable transferable, JComponent label) {
        try {
            // Try to get an image.
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
                StringTokenizer tokenizer = new StringTokenizer(uris, "\r\n");
                if (tokenizer.hasMoreTokens()) {
                    String uri = tokenizer.nextToken();

                    // ignore comments
                    if (!uri.startsWith("#") && !uri.isEmpty()) {
                        log.debug("uri = '{}'", uri);
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
                // Remove 'file://' from file name.
                String fileName = read.readLine().substring(7).replace("%20", " ");
                // Remove 'localhost' from OS X file names.
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
                log.debug("url = '{}'", url);
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
        //log.debug("toBufferedImage - 1");
        if (image == null) {
            return null;
        }
        if (width == -1) {
            width = image.getWidth(null);
        }
        if (height == -1) {
            height = image.getHeight(null);
        }
        // Draw original image to thumbnail image object and
        // scale it to the new size on-the-fly.
        //log.debug("toBufferedImage - 2.2, image = {}, width = {}, height = {}", new Object[] {image,width,height});

        BufferedImage bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        //log.debug("toBufferedImage - 2.3, bufferedImage = {}", bufferedImage);

        Graphics2D g2 = bufferedImage.createGraphics();

        //log.debug("toBufferedImage - 3");
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2.drawImage(image, 0, 0, width, height, null);
        //log.debug("toBufferedImage - 4");
        g2.dispose();
        return bufferedImage;
    }
}
