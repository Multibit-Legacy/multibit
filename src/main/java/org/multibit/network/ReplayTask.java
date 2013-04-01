package org.multibit.network;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.multibit.message.Message;
import org.multibit.model.PerWalletModelData;

/**
 * A class encapsulating a request on one or more wallets to perform a
 * blockchain replay
 */
public class ReplayTask {

    private final List<PerWalletModelData> perWalletModelDataToReplay;
    
    private final Date startDate;
    
    private final UUID uuid;
    
    private long percentComplete;
       
    public ReplayTask( List<PerWalletModelData> perWalletModelDataToReplay, Date startDate) {
        this.perWalletModelDataToReplay = perWalletModelDataToReplay;
        this.startDate = startDate;
        this.percentComplete = Message.NOT_RELEVANT_PERCENTAGE_COMPLETE;
        this.uuid = UUID.randomUUID();
    }

    public List<PerWalletModelData> getPerWalletModelDataToReplay() {
        return perWalletModelDataToReplay;
    }

    public Date getStartDate() {
        return startDate;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((perWalletModelDataToReplay == null) ? 0 : perWalletModelDataToReplay.hashCode());
        result = prime * result + (int) (percentComplete ^ (percentComplete >>> 32));
        result = prime * result + ((startDate == null) ? 0 : startDate.hashCode());
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
        if (percentComplete != other.percentComplete)
            return false;
        if (startDate == null) {
            if (other.startDate != null)
                return false;
        } else if (!startDate.equals(other.startDate))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "ReplayTask [perWalletModelDataToReplay=" + perWalletModelDataToReplay + ", startDate=" + startDate + ", uuid="
                + uuid + ", percentComplete=" + percentComplete + "]";
    }

    public UUID getUuid() {
        return uuid;
    }

    public long getPercentComplete() {
        return percentComplete;
    }

    public void setPercentComplete(long percentComplete) {
        this.percentComplete = percentComplete;
    }
}
