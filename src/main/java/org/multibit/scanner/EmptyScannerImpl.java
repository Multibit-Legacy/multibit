package org.multibit.scanner;

import javax.swing.JLabel;

/**
 * An empty, do nothing scanner
 * @author jim
 *
 */
public class EmptyScannerImpl implements Scanner {

    public void setLabel(JLabel label) {
    }
    
    public JLabel getLabel() {
        return null;
    }
    
    
    @Override
    public void startScan() {
    }

    @Override
    public void stopScan() {
    }

    @Override
    public boolean wasScanSuccessful() {
        return false;
    }

    @Override
    public String getDecodedResult() {
        return null;
    }

    @Override
    public boolean isScanningInProgress() {
        return false;
    }

    @Override
    public boolean isScannerSupported() {
        return false;
    }


    @Override
    public void setScannerCallBack(ScannerCallBack scannerCallBack) {
        // TODO Auto-generated method stub
        
    }
}
