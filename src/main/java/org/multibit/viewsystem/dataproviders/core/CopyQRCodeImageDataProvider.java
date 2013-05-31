package org.multibit.viewsystem.dataproviders.core;

import javax.swing.JLabel;
import org.multibit.viewsystem.dataproviders.DataProvider;

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
