package org.multibit.network;

import java.util.Date;
import java.util.List;

import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanelDownloadListener;

import com.google.bitcoin.core.DownloadListener;

/**
 * A class encapsulating a request on one or more wallets to perform a
 * blockchain replay
 */
public class ReplayTask {

    private final List<PerWalletModelData> perWalletModelDataToReplay;
    
    private final Date startDate;
    private final Date stopDate;
    
    private final int stopBlockHeight;
    
    /**
     * A single wallet download listener that will be informed of progress of the download.
     */
    private final SingleWalletPanelDownloadListener singleWalletPanelDownloadListener;
    
    public ReplayTask( List<PerWalletModelData> perWalletModelDataToReplay, SingleWalletPanelDownloadListener singleWalletPanelDownloadListener, Date startDate, Date stopDate, int stopBlockHeight) {
        this.perWalletModelDataToReplay = perWalletModelDataToReplay;
        this.startDate = startDate;
        this.stopDate = stopDate;
        this.stopBlockHeight = stopBlockHeight;
        this.singleWalletPanelDownloadListener = singleWalletPanelDownloadListener;
    }

    public List<PerWalletModelData> getPerWalletModelDataToReplay() {
        return perWalletModelDataToReplay;
    }

    public Date getStartDate() {
        return startDate;
    }

    public Date getStopDate() {
        return stopDate;
    }

    public int getStopBlockHeight() {
        return stopBlockHeight;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((perWalletModelDataToReplay == null) ? 0 : perWalletModelDataToReplay.hashCode());
        result = prime * result + ((startDate == null) ? 0 : startDate.hashCode());
        result = prime * result + stopBlockHeight;
        result = prime * result + ((stopDate == null) ? 0 : stopDate.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof ReplayTask))
            return false;
        ReplayTask other = (ReplayTask) obj;
        if (perWalletModelDataToReplay == null) {
            if (other.perWalletModelDataToReplay != null)
                return false;
        } else if (!perWalletModelDataToReplay.equals(other.perWalletModelDataToReplay))
            return false;
        if (startDate == null) {
            if (other.startDate != null)
                return false;
        } else if (!startDate.equals(other.startDate))
            return false;
        if (stopBlockHeight != other.stopBlockHeight)
            return false;
        if (stopDate == null) {
            if (other.stopDate != null)
                return false;
        } else if (!stopDate.equals(other.stopDate))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "ReplayTask [startDate=" + startDate + ", stopDate=" + stopDate
                + ", stopBlockHeight=" + stopBlockHeight + "]";
    }

    public SingleWalletPanelDownloadListener getSingleWalletPanelDownloadListener() {
        return singleWalletPanelDownloadListener;
    }
}
