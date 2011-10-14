package org.multibit.qrcode;

/**
 * Uses code from com.google.zxing.qrcode.QRCodeWriter which is:
 * Copyright 2008 ZXing authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import com.google.zxing.WriterException;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;
import com.google.zxing.qrcode.encoder.ByteMatrix;
import com.google.zxing.qrcode.encoder.Encoder;
import com.google.zxing.qrcode.encoder.QRCode;

/**
 * Class to generate swatches (QR codes + text of QR code in an image)
 * 
 * @author jim
 * 
 */
public class SwatchGenerator {
    private static final int QUIET_ZONE_SIZE = 4;

    public SwatchGenerator() {

    }

    /**
     * generate a Swatch
     * 
     * @param address
     *            Bitcoin address to show
     * @param amount
     *            amount of BTC to show - text
     * @param label
     *            label for swatch
     * @return
     */
    public BufferedImage generateSwatch(String address, String amount, String label) {
        String bitcoinURI = BitcoinURI.convertToBitcoinURI(address, amount, label);
       
        Font addressFont = new Font("Verdana", Font.PLAIN, 10);
        Font labelFont = new Font("Times Roman", Font.PLAIN, 16);
        Font amountFont = new Font("Times Roman", Font.PLAIN, 16);

        // get a byte matrix for the data
        ByteMatrix matrix;
        try {
            matrix = encode(bitcoinURI);
        } catch (com.google.zxing.WriterException e) {
            // exit the method
            return null;
        }

        // generate an image from the byte matrix
        int matrixWidth = matrix.getWidth();
        int matrixHeight = matrix.getHeight();
        
        // work out the width of the swatch
        BufferedImage emptyImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
        Graphics emptyGraphics = emptyImage.getGraphics();
        
        Dimension addressBox = getBoundingBox(emptyGraphics, address, addressFont);
        Dimension amountBox = getBoundingBox(emptyGraphics, amount + " BTC", amountFont);
        Dimension labelBox = getBoundingBox(emptyGraphics, label, labelFont);

        int swatchWidth = matrixWidth + 26 + (int)Math.max(Math.max(addressBox.getWidth(), amountBox.getWidth()),labelBox.getWidth()) + QUIET_ZONE_SIZE;
        // create buffered image to draw to
        BufferedImage image = new BufferedImage(swatchWidth, matrixHeight, BufferedImage.TYPE_INT_RGB);

        // iterate through the matrix and draw the pixels to the image
        for (int y = 0; y < matrixHeight; y++) {
            for (int x = 0; x < matrixWidth; x++) {
                byte imageValue = matrix.get(x, y);
                image.setRGB(x, y, imageValue);
            }
        }

        // fill in the rest of the image as white
        for (int y = 0; y < matrixHeight; y++) {
            for (int x = matrixWidth; x < swatchWidth; x++) {
                image.setRGB(x, y, 0xFFFFFF);
            }
        }

        // draw the text box
        for (int y = QUIET_ZONE_SIZE; y < matrixHeight - QUIET_ZONE_SIZE; y++) {
            // left hand side
            image.setRGB(matrixWidth, y, 0x000000);
            image.setRGB(matrixWidth + 1, y, 0x000000);

            // right hand side
            image.setRGB(swatchWidth - QUIET_ZONE_SIZE - 1, y, 0x000000);
            image.setRGB(swatchWidth - QUIET_ZONE_SIZE - 2, y, 0x000000);
        }

        for (int x = matrixWidth + QUIET_ZONE_SIZE - 2; x < swatchWidth - QUIET_ZONE_SIZE; x++) {
            // top side
            image.setRGB(x, QUIET_ZONE_SIZE, 0x000000);
            image.setRGB(x, QUIET_ZONE_SIZE + 1, 0x000000);

            // bottom side
            image.setRGB(x, matrixHeight - QUIET_ZONE_SIZE - 1, 0x000000);
            image.setRGB(x, matrixHeight - QUIET_ZONE_SIZE - 2, 0x000000);
        }

        Graphics2D g2 = image.createGraphics();

        g2.setColor(Color.black);
        g2.setFont(addressFont);
        g2.drawString(address, matrixWidth + QUIET_ZONE_SIZE, matrixHeight - QUIET_ZONE_SIZE - 6);

        g2.setFont(labelFont);
        g2.drawString(label, matrixWidth + QUIET_ZONE_SIZE + 2, QUIET_ZONE_SIZE + 20);

        g2.setFont(amountFont);
        g2.drawString(amount + " BTC", matrixWidth + QUIET_ZONE_SIZE + 2, QUIET_ZONE_SIZE + 40);

        return image;
    }

    private Dimension getBoundingBox(Graphics graphics, String text, Font font) {
        // get metrics from the graphics
        FontMetrics metrics = graphics.getFontMetrics(font);
        // get the height of a line of text in this font and render context
        int height = metrics.getHeight();
        // get the advance of my text in this font and render context
        int advance = metrics.stringWidth(text);
        // calculate the size of a box to hold the text with some padding.
        Dimension size = new Dimension(advance + 2, height + 2);
        return size;
    }

    public static void main(String[] args) {
        SwatchGenerator swatchGenerator = new SwatchGenerator();
        String address = "15BGmyMKxGFkejW1oyf2Gwv3NHqeUP7aWh";
        String amount = "1.23";
        String label = " A Quiet American";

        BufferedImage swatch = swatchGenerator.generateSwatch(address, amount, label);
        ImageIcon icon = new ImageIcon(swatch);
        //String text = label + "\n" + amount + "\n" + address;
        String text = "";
        JOptionPane.showMessageDialog(null, text , "Swatch Generator",
                JOptionPane.INFORMATION_MESSAGE, icon);
        
        //write the image to the output stream
        try {
            ImageIO.write(swatch, "png",new File("swatch.png"));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * This object renders a QR Code as a ByteMatrix 2D array of greyscale
     * values.
     * 
     * @author dswitkin@google.com (Daniel Switkin)
     */
    public ByteMatrix encode(String contents) throws WriterException {

        if (contents == null || contents.length() == 0) {
            throw new IllegalArgumentException("Found empty contents");
        }

        QRCode code = new QRCode();
        Encoder.encode(contents, ErrorCorrectionLevel.L, null, code);
        return renderResult(code, 2);
    }

    // Note that the input matrix uses 0 == white, 1 == black, while the output
    // matrix uses
    // 0 == black, 255 == white (i.e. an 8 bit greyscale bitmap).
    private static ByteMatrix renderResult(QRCode code, int multiple) {
        ByteMatrix input = code.getMatrix();
        int inputWidth = input.getWidth();
        int inputHeight = input.getHeight();
        int qrWidth = multiple * inputWidth + (QUIET_ZONE_SIZE << 1);
        int qrHeight = multiple * inputHeight + (QUIET_ZONE_SIZE << 1);

        ByteMatrix output = new ByteMatrix(qrWidth, qrHeight);
        byte[][] outputArray = output.getArray();

        // We could be tricky and use the first row in each set of multiple as
        // the temporary storage,
        // instead of allocating this separate array.
        byte[] row = new byte[qrWidth];

        // 1. Write the white lines at the top
        for (int y = 0; y < QUIET_ZONE_SIZE; y++) {
            setRowColor(outputArray[y], (byte) 255);
        }

        // 2. Expand the QR image to the multiple
        byte[][] inputArray = input.getArray();
        for (int y = 0; y < inputHeight; y++) {
            // a. Write the white pixels at the left of each row
            for (int x = 0; x < QUIET_ZONE_SIZE; x++) {
                row[x] = (byte) 255;
            }

            // b. Write the contents of this row of the barcode
            int offset = QUIET_ZONE_SIZE;
            for (int x = 0; x < inputWidth; x++) {
                byte value = (inputArray[y][x] == 1) ? 0 : (byte) 255;
                for (int z = 0; z < multiple; z++) {
                    row[offset + z] = value;
                }
                offset += multiple;
            }

            // c. Write the white pixels at the right of each row
            offset = QUIET_ZONE_SIZE + (inputWidth * multiple);
            for (int x = offset; x < qrWidth; x++) {
                row[x] = (byte) 255;
            }

            // d. Write the completed row multiple times
            offset = QUIET_ZONE_SIZE + (y * multiple);
            for (int z = 0; z < multiple; z++) {
                System.arraycopy(row, 0, outputArray[offset + z], 0, qrWidth);
            }
        }

        // 3. Write the white lines at the bottom
        int offset = QUIET_ZONE_SIZE + (inputHeight * multiple);
        for (int y = offset; y < qrHeight; y++) {
            setRowColor(outputArray[y], (byte) 255);
        }

        return output;
    }

    private static void setRowColor(byte[] row, byte value) {
        for (int x = 0; x < row.length; x++) {
            row[x] = value;
        }
    }
}
