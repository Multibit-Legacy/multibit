package org.multibit.qrcode;

import java.awt.image.BufferedImage;

import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

public class QRCodeEncoderDecoder {

    private int width;
    private int height;
    
    public QRCodeEncoderDecoder(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public BufferedImage encode(String data) {
        // get a byte matrix for the data
        BitMatrix matrix;
        com.google.zxing.Writer writer = new QRCodeWriter();
        try {
            matrix = writer.encode(data, com.google.zxing.BarcodeFormat.QR_CODE, width, height);
        } catch (com.google.zxing.WriterException e) {
            // exit the method
            return null;
        }

        // generate an image from the byte matrix
        int width = matrix.getWidth();
        int height = matrix.getHeight();

        // create buffered image to draw to
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        // iterate through the matrix and draw the pixels to the image
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                boolean imageValue = matrix.get(x, y);
                image.setRGB(x, y, (imageValue ? 0 : 0xFFFFFF));
            }
        }

        // //write the image to the output stream
        // ImageIO.write(image, "png", outputStream);

        return image;
    }
}
