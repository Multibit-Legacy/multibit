package org.multibit.scanner;

import javax.swing.JLabel;

/**
 * Interface for Scanners used by MultiBit in QR code recognition
 * 
 * Note that implementing classes must have a no-args constructor
 * 
 * @author jim
 *
 */
public interface Scanner {

    public void setLabel(JLabel label);
    public JLabel getLabel();
    
    public void setScannerCallBack(ScannerCallBack scannerCallBack);
    
      /**
     * Start scanning for QR code.
     * Should not block.
     */
    public void startScan();

    /**
     * Stop scanning for QR code.
     * Should not block 
     */
    public void stopScan();

    /**
     * Did the scan successfully find a QR code ?
     * @return
     */
    public boolean wasScanSuccessful();

    /**
     * get the QR code found, or "" if none was found
     * @return
     */
    public String getDecodedResult();

    /**
     * is the scanner currently still scanning ?
     * @return
     */
    public boolean isScanningInProgress();
    
    /**
     * is the scanner supported for the current operating system
     * @return
     */
    public boolean isScannerSupported();

}