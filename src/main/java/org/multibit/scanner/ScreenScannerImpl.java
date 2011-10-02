package org.multibit.scanner;

import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;

import org.apache.log4j.BasicConfigurator;
import org.multibit.qrcode.QRCodeEncoderDecoder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.awt.AWTUtilities;

/**
 * Created by IntelliJ IDEA. User: Jeffrey Perz Date: 7/24/11 Time: 9:24 PM
 */
public class ScreenScannerImpl implements Scanner {
    public static final Logger LOGGER = LoggerFactory.getLogger(ScreenScannerImpl.class);
    public static final int DEFAULT_FRAME_WIDTH = 400;
    public static final int DEFAULT_FRAME_HEIGHT = 400;
    public static final int DEFAULT_FRAME_X_POSITION = 200;
    public static final int DEFAULT_FRAME_Y_POSITION = 200;

    public static final int POLLING_SLEEP_IN_MILLIS = 333;

    // for window capture
    private JFrame frame;
    private JPanel panel;

    // for scanning progress
    private boolean scanningInProgress = false;
    private boolean scanSuccessful = false;
    private String decodedResult = null;

    // used for putting image into MultiBit
    private JLabel label;

    // scanner callback after scanning successful
    private ScannerCallBack scannerCallBack = null;

    private boolean keepGoing = true;

    /**
     * This is the default method for instantiating the ScannerFrame GUI
     * 
     * @param args
     *            runtime arguments for the ScannerFrame class
     */
    public static void main(String args[]) {
        BasicConfigurator.configure();

        new ScreenScannerImpl();
    }

    /**
     * Default constructor creates the GUI and a object instance to the
     * BitcoinConduitQRCode class
     */
    public ScreenScannerImpl() {
        panel = ScannerPanel.createScannerPanel();

        frame = new JFrame("Scanner Frame");
        frame.setSize(DEFAULT_FRAME_WIDTH, DEFAULT_FRAME_HEIGHT);
        frame.setLocation(DEFAULT_FRAME_X_POSITION, DEFAULT_FRAME_Y_POSITION);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        frame.getContentPane().setLayout(new BorderLayout());
        frame.getContentPane().add(panel, BorderLayout.CENTER);

        // work out which OS we are on
        String operatingSystem = System.getProperty("os.name");
        if ("Mac OS X".equals(operatingSystem)) {
            // make window translucent
            frame.setBackground(new Color(1.0f, 1.0f, 1.0f, 0.18f));
        } else {
            // transparencies don't work with window decorators
            frame.setUndecorated(false);
            // we want to use per-pixel transparency
            try {
                AWTUtilities.setWindowOpaque(frame, false);
            } catch (Exception e) {
                // not supported - window will not be transparent
            }
            // creates window decorations without using a window decorator...
            frame.getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
        }

        // add poll button to scanning frame
        JPanel buttonPanel = new JPanel(new FlowLayout());
        buttonPanel.setOpaque(false);
        JButton pollButton = new JButton("Poll");
        buttonPanel.add(pollButton);
        pollButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                pollWindow();
            }
        });
        frame.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Starts the polling for QR codes using the window's boundaries
     */
    public void pollWindow() {
        pollWindow(frame.getBounds());
    }

    @Override
    public void setLabel(JLabel label) {
        this.label = label;

    }

    @Override
    public JLabel getLabel() {
        return label;
    }

    @Override
    public void setScannerCallBack(ScannerCallBack scannerCallBack) {
        this.scannerCallBack = scannerCallBack;
    }

    @Override
    public void startScan() {
        frame.setVisible(true);
        scanSuccessful = false;
        scanningInProgress = true;
    }

    @Override
    public void stopScan() {
        keepGoing = false;
    }

    @Override
    public boolean wasScanSuccessful() {
        return scanSuccessful;
    }

    @Override
    public String getDecodedResult() {
        return decodedResult;
    }

    @Override
    public boolean isScanningInProgress() {
        return scanningInProgress;
    }

    @Override
    public boolean isScannerSupported() {
        return true;
    }

    /**
     * Read the bounded area contents and process them using the Zebra crossing
     * 
     * @param bounds
     *            bounds a Rectangle defining what area of the screen to scan
     *            for QR codes TODO A zx-ing array of QR code results. It is
     *            possible for a single scan to contain more than one QR code.
     * @return decodedResult decode of first QR code seen
     */
    private String readWindow(Rectangle bounds) {
        Robot robot = null;
        try {
            robot = new Robot();
        } catch (AWTException e) {
            e.printStackTrace();
        }

        String decodedResult = null;

        if (robot != null) {
            BufferedImage screenCapture = robot.createScreenCapture(bounds);

            if (screenCapture != null) {
                if (label != null) {
                    label.setText("");

                    // image scaled to size of label
                    BufferedImage scannedBufferedImage = toBufferedImage(screenCapture, label.getWidth(), label.getHeight());

                    label.setIcon(new ImageIcon(scannedBufferedImage));

                    label.invalidate();
                    label.validate();
                    label.repaint();
                }

                // image maximum size - recognition seems better
                QRCodeEncoderDecoder qrEncoderDecoder = new QRCodeEncoderDecoder(screenCapture.getWidth(null),
                        screenCapture.getHeight(null));
                decodedResult = qrEncoderDecoder.decode(screenCapture);
            }
        }
        return decodedResult;
    }

    /**
     * Poll the bounded area until a valid QR code is found
     * 
     * @param bounds
     *            a Rectangle defining what area of the screen to scan for QR
     *            codes
     * @return A zx-ing array of QR code results. It is possible for a single
     *         scan to contain more than one QR code.
     */
    // this can be invoked directly if reading should start immediately
    // otherwise use the waitOnQrCodeWindowCapture
    private String pollWindow(Rectangle bounds) {
        decodedResult = null;
        while (keepGoing && decodedResult == null || "".equals(decodedResult)) {
            decodedResult = readWindow(bounds);
            if (decodedResult == null || "".equals(decodedResult)) {
                try {
                    Thread.sleep(ScreenScannerImpl.POLLING_SLEEP_IN_MILLIS);
                } catch (InterruptedException e) {
                    ScreenScannerImpl.LOGGER.error(e.getMessage());
                }
            } else {
                ScreenScannerImpl.LOGGER.info(decodedResult);
                scanSuccessful = true;
                scanningInProgress = false;
                keepGoing = false;

                Toolkit.getDefaultToolkit().beep();

                if (scannerCallBack != null) {
                    scannerCallBack.scannerHasCompleted();
                }
                break;
            }
        }
        return decodedResult;
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

}

/**
 * Defines a JPanel that uses per pixel transparencies
 */
final class ScannerPanel {
    /**
     * Default empty constructor
     */
    private ScannerPanel() {
    }

    /**
     * Creates a JPanel that implements per pixel transparencies
     * 
     * @return JPanel with per pixel transparencies
     */
    public static JPanel createScannerPanel() {
        return new JPanel() {
            private static final long serialVersionUID = 1L;

            /**
             * Paint method that in implements per pixel transparencies
             * 
             * @param g
             *            Graphics object for the JPanel
             */
            protected void paintComponent(Graphics g) {
                if (g instanceof Graphics2D) {
                    Paint p = new Color(0, 0, 0, 0);
                    Graphics2D g2d = (Graphics2D) g;
                    g2d.setPaint(p);
                    g2d.fillRect(0, 0, getWidth(), getHeight());
                } else {
                    super.paintComponent(g);
                }
            }
        };
    }
}