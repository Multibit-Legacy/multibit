/**
 * Copyright 2012 multibit.org
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
import java.awt.image.BufferedImage;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.model.bitcoin.BitcoinModel;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.uri.BitcoinURI;
import com.google.zxing.WriterException;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;
import com.google.zxing.qrcode.encoder.ByteMatrix;
import com.google.zxing.qrcode.encoder.Encoder;
import com.google.zxing.qrcode.encoder.QRCode;


/**
 * Class to generate QR codes
 * 
 * @author jim
 * 
 */
public class QRCodeGenerator {
    private static final int QUIET_ZONE_SIZE = 4;
    private static int QR_CODE_ELEMENT_MULTIPLE = 2;

    private QRCode code;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    public QRCodeGenerator(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;

        code = new QRCode();
    }

    public BufferedImage generateQRcode(String address, String amount, String label) {
        return generateQRcode(address, amount, label, 1);
    }

    /**
     * generate a QR code
     * 
     * @param address
     *            Bitcoin address to show
     * @param amount
     *            amount of BTC to show - text
     * @param label
     *            label for swatch
     * @return
     */
    public BufferedImage generateQRcode(String address, String amount, String label, int scaleFactor) {
        String bitcoinURI = "";
        try {
            Address decodeAddress = null;
            if (address != null && !"".equals(address) && this.bitcoinController.getMultiBitService() != null
                    && this.bitcoinController.getModel().getNetworkParameters() != null) {
                decodeAddress = new Address(this.bitcoinController.getModel().getNetworkParameters(), address);
            }
            if (decodeAddress != null && !"".equals(decodeAddress)) {
                if (amount != null && !"".equals(amount)) {
                    CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(amount);
                    if (converterResult.isBtcMoneyValid()) {
                        bitcoinURI = BitcoinURI.convertToBitcoinURI(decodeAddress, converterResult.getBtcMoney().getAmount().toBigInteger(), label, null);
                    } else {
                       // No parsable amount - show nothing.  
                    }                } else {
                    bitcoinURI = BitcoinURI.convertToBitcoinURI(decodeAddress, null, label, null);
                }
            }
            this.bitcoinController.getModel().setActiveWalletPreference(BitcoinModel.SEND_PERFORM_PASTE_NOW, "false");
        } catch (IllegalArgumentException e) {
            //log.warn("The address '" + address + "' could not be converted to a bitcoin address. (IAE)");
            return null;
        } catch (AddressFormatException e) {
            //log.warn("The address '" + address + "' could not be converted to a bitcoin address. (AFE)");
            return null;
        }

        // get a byte matrix for the data
        ByteMatrix matrix;
        try {
            matrix = encode(bitcoinURI);
        } catch (com.google.zxing.WriterException e) {
            // exit the method
            return null;
        } catch (IllegalArgumentException e) {
            // exit the method
            return null;
        }

        // generate an image from the byte matrix
        int matrixWidth = matrix.getWidth();
        int matrixHeight = matrix.getHeight();
        int swatchWidth = matrixWidth * scaleFactor;
        int swatchHeight = matrixHeight * scaleFactor;

        // create buffered image to draw to
        BufferedImage image = new BufferedImage(swatchWidth, swatchHeight, BufferedImage.TYPE_INT_RGB);

        // iterate through the matrix and draw the pixels to the image
        for (int y = 0; y < matrixHeight; y++) {
            for (int x = 0; x < matrixWidth; x++) {
                byte imageValue = matrix.get(x, y);
                for (int scaleX = 0; scaleX < scaleFactor; scaleX++) {
                    for (int scaleY = 0; scaleY < scaleFactor; scaleY++)  {
                        image.setRGB(x * scaleFactor + scaleX, y * scaleFactor + scaleY, imageValue);                       
                    }
                }
            }
        }

        return image;
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
        this.code = Encoder.encode(contents, ErrorCorrectionLevel.L);
        return renderResult(code, QR_CODE_ELEMENT_MULTIPLE);
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
