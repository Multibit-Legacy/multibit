package org.multibit.scanner;

/**
 * This code requires a copy of ImageSnap to actually capture the images from the iSight camera
 * http://www.iharder.net/current/macosx/imagesnap/
 * 
 */
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.multibit.qrcode.QRCodeEncoderDecoder;

import com.izforge.izpack.gui.FlowLayout;

public class MacScannerImpl implements Scanner {
    public static final String IMAGESNAP_EXECUTABLE_NAME = "imagesnap";
    public static final int MAXIMUM_NUMBER_OF_SCANS = 20;

    public void setLabel(JLabel label) {
        this.label = label;
    }

    public JLabel getLabel() {
        return label;
    }

    private Process scanProcess = null;
    private boolean scanningInProgress = false;
    private boolean scanSuccessful = false;
    private String decodedResult = null;

    private ScanThread scanThread = null;

    private ScannerCallBack scannerCallBack = null;

    private JLabel label;

    private boolean keepGoing = true;

    private String scannerDirectory = null;
    
    public MacScannerImpl(String scannerDirectory) {
        // get the current working directory
        File directory = new File (".");
        String currentDirectory = directory.getAbsolutePath();
        this.scannerDirectory = currentDirectory + File.separator + scannerDirectory;  
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.multibit.scanner.Scanner2#startScan()
     */
    @Override
    public void startScan() {
        scanSuccessful = false;
        scanningInProgress = true;

        scanThread = new ScanThread();

        scanThread.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.multibit.scanner.Scanner2#stopScan()
     */
    @Override
    public void stopScan() {
        keepGoing = false;
    }

    public static void main(String[] args) {
        JFrame frame = new JFrame();
        frame.setMinimumSize(new Dimension(300, 300));

        JLabel label = new JLabel("Initialising...");
        label.setHorizontalAlignment(JLabel.CENTER);
        label.setMinimumSize(new Dimension(300, 300));
        frame.getContentPane().add(label, BorderLayout.CENTER);

        final Scanner macScanner = new MacScannerImpl("src/main/resources/imageSnap-v0.2.5");
        macScanner.setLabel(label);

        JPanel buttonPanel = new JPanel(new FlowLayout());
        JButton stopButton = new JButton("Stop scan");
        stopButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                macScanner.stopScan();
            }
        });
        buttonPanel.add(stopButton);
        frame.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        frame.pack();
        frame.setVisible(true);

        macScanner.startScan();

        while (macScanner.isScanningInProgress()) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e1) {
                e1.printStackTrace();
            }
        }
        System.out.println("Was scan successful ? : " + macScanner.wasScanSuccessful());
        System.out.println("Result of scan : " + macScanner.getDecodedResult());

        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        if (frame != null) {
            frame.dispose();
            frame.setVisible(false);
            frame = null;
        }
    }

    // This method returns a buffered image with the contents of an image
    private BufferedImage toBufferedImage(Image image, int width, int height) {
        if (width == -1) {
            width = image.getWidth(null);
        }
        if (height == -1) {
            height = image.getHeight(null);
        }

        // This code ensures that all the pixels in the image are loaded
        image = new ImageIcon(image).getImage();

        // Create a buffered image with a format that's compatible with the
        // screen
        BufferedImage bimage = null;
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        try {
            // Determine the type of transparency of the new buffered image
            int transparency = Transparency.OPAQUE;

            // Create the buffered image
            GraphicsDevice gs = ge.getDefaultScreenDevice();
            GraphicsConfiguration gc = gs.getDefaultConfiguration();
            bimage = gc.createCompatibleImage(width, height, transparency);
        } catch (HeadlessException e) {
            // The system does not have a screen
        }

        if (bimage == null) {
            // Create a buffered image using the default color model
            int type = BufferedImage.TYPE_INT_RGB;
            bimage = new BufferedImage(width, height, type);
        }

        // Copy image to buffered image
        Graphics2D g = bimage.createGraphics();

        g.drawImage(image, 0, 0, width, height, null);

        g.dispose();

        return bimage;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.multibit.scanner.Scanner2#wasScanSuccessful()
     */
    @Override
    public boolean wasScanSuccessful() {
        return scanSuccessful;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.multibit.scanner.Scanner2#getDecodedResult()
     */
    @Override
    public String getDecodedResult() {
        return decodedResult;
    }

    class ScanThread extends Thread {
        public void run() {
            scanningInProgress = true;
            Runtime runtime = Runtime.getRuntime();

            // check imagesnap executable is available
            String imageSnapExecutableFilename = scannerDirectory + File.separator  + IMAGESNAP_EXECUTABLE_NAME;
            boolean imageSnapExists;
            if ((new File(imageSnapExecutableFilename)).exists()) {
                imageSnapExists = true;
            } else {
                imageSnapExists = false;
            }

            if (imageSnapExists) {
                try {
                    for (int i = 0; i < MAXIMUM_NUMBER_OF_SCANS; i++) {
                        if (!keepGoing) {
                            break;
                        }

                        scanProcess = runtime.exec(imageSnapExecutableFilename + " -q -");

                        try {
                            Image image = ImageIO.read(scanProcess.getInputStream());
                            if (image != null) {
                                if (label != null) {
                                    label.setText("");

                                    // image scaled to size of label
                                    BufferedImage scannedBufferedImage = toBufferedImage(image, label.getWidth(),
                                            label.getHeight());

                                    // flip image horizonatally
                                    AffineTransform tx = AffineTransform.getScaleInstance(-1, 1);
                                    tx.translate(-scannedBufferedImage.getWidth(null), 0);
                                    AffineTransformOp op = new AffineTransformOp(tx, AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
                                    scannedBufferedImage = op.filter(scannedBufferedImage, null);

                                    label.setIcon(new ImageIcon(scannedBufferedImage));

                                    label.invalidate();
                                    label.validate();
                                    label.repaint();
                                }

                                // image maximum size - recognition seems better
                                BufferedImage scannedBufferedImageFullSize = toBufferedImage(image, -1, -1);
                                QRCodeEncoderDecoder qrEncoderDecoder = new QRCodeEncoderDecoder(
                                        scannedBufferedImageFullSize.getWidth(null),
                                        scannedBufferedImageFullSize.getHeight(null));
                                decodedResult = qrEncoderDecoder.decode(scannedBufferedImageFullSize);

                                if (decodedResult != null && !decodedResult.equals("") && decodedResult.startsWith("bitcoin:")) {
                                    // scan was successful
                                    Toolkit.getDefaultToolkit().beep();
                                    scanSuccessful = true;
                                    scanningInProgress = false;
                                    break;
                                }
                            }
                        } catch (IOException ioe) {
                            ioe.printStackTrace();
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            } else {
                System.out.println("No scanner available at : " + imageSnapExecutableFilename);
            }
            
            scanningInProgress = false;
            if (scannerCallBack != null) {
                scannerCallBack.scannerHasCompleted();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.multibit.scanner.Scanner2#isScanningInProgress()
     */
    @Override
    public boolean isScanningInProgress() {
        return scanningInProgress;
    }

    /**
     * is the scanner supported for the current operating system
     * 
     * @return
     */
    @Override
    public boolean isScannerSupported() {
        // only supported on Macs
        String operatingSystem = System.getProperty("os.name");
        return "Mac OS X".equals(operatingSystem);
    }

    @Override
    public void setScannerCallBack(ScannerCallBack scannerCallBack) {
        this.scannerCallBack = scannerCallBack;
    }

}
