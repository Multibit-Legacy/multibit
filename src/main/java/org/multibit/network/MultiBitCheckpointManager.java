package org.multibit.network;

import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.Map;

import com.google.bitcoin.core.CheckpointManager;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.StoredBlock;

public class MultiBitCheckpointManager extends CheckpointManager {

    public MultiBitCheckpointManager(NetworkParameters params, InputStream inputStream) throws IOException {
        super(params, inputStream);
    }
    
    /**
     * Returns a {@link Date} representing the date of the last checkpoint before the given block height, for example, normally
     * you would want to know the checkpoint before the last block the wallet had seen.
     */
    public Date getCheckpointDateBeforeOrAtHeight(int height) {
        Map.Entry<Long, StoredBlock> highestCheckpointBeforeHeight = null;
        
        for (Map.Entry<Long, StoredBlock> loop : checkpoints.entrySet()) {
            if (loop.getValue().getHeight() < height) {
                // This checkpoint is before the specified height.
                if (highestCheckpointBeforeHeight == null) {
                    highestCheckpointBeforeHeight = loop;
                } else {
                    if (highestCheckpointBeforeHeight.getValue().getHeight() < loop.getValue().getHeight()) {
                        // This entry is later.
                        highestCheckpointBeforeHeight = loop;
                    }
                }
            }
        }
        
        if (highestCheckpointBeforeHeight == null) {
            return params.genesisBlock.getTime();
        }
        return new Date(highestCheckpointBeforeHeight.getKey() * 1000);
    }
}