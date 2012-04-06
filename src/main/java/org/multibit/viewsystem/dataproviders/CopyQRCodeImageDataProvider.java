package org.multibit.viewsystem.dataproviders;

import javax.swing.JLabel;

/**
 * DataProvider for copy QR code image action
 * @author jim
 *
 */
public interface CopyQRCodeImageDataProvider extends DataProvider { 
    /**
     * Get the URI image
     */
    public JLabel getURIImage();
}
