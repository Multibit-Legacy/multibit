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
package org.multibit.qrcode;

import java.awt.image.BufferedImage;
import java.util.Hashtable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.zxing.BinaryBitmap;
import com.google.zxing.LuminanceSource;
import com.google.zxing.ReaderException;
import com.google.zxing.Result;
import com.google.zxing.client.j2se.BufferedImageLuminanceSource;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.common.HybridBinarizer;
import com.google.zxing.qrcode.QRCodeReader;
import com.google.zxing.qrcode.QRCodeWriter;

public class QRCodeEncoderDecoder {
    private final Logger log = LoggerFactory.getLogger(QRCodeEncoderDecoder.class);

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

    public String decode(BufferedImage image) {

        // convert the image to a binary bitmap source
        LuminanceSource source = new BufferedImageLuminanceSource(image);
        BinaryBitmap bitmap = new BinaryBitmap(new HybridBinarizer(source));

        // decode the barcode
        QRCodeReader reader = new QRCodeReader();

        try {
            @SuppressWarnings("rawtypes")
            Hashtable hints = new Hashtable();
            Result result = reader.decode(bitmap, hints);
            log.info("Decoded image successfully, result was : '" + result.getText() + "'");

            return result.getText();
        } catch (ReaderException e) {
            // the data is improperly formatted
            log.debug(e.getMessage());
            log.error("Error while decoding image", e);
        }

        return "";
    }
}
