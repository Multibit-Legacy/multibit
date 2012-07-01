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
package org.multibit.network;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Stack;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ProtocolException;
import com.google.bitcoin.core.StoredBlock;

/**
 * Enum singleton to manage cached blocks.
 * Note: You must do a call to setController with the MultiBitController before first use.
 * 
 * @author jim
 */
public enum CacheManager {
    INSTANCE();

    private static final Logger log = LoggerFactory.getLogger(CacheManager.class);

    public static final String CACHE_DIRECTORY = "cache";
    public static final String BLOCK_CACHE_DIRECTORY = "block";
    public static final String BLOCK_SUFFIX = ".block";

    private String cacheDirectory;
    private String blockCacheDirectory;
    private MultiBitController controller;
      
    CacheManager() {
        String applicationDataDirectory = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory();
        cacheDirectory = applicationDataDirectory + File.separator + CACHE_DIRECTORY;
        File cacheDirectoryFile = new File(cacheDirectory);
        if (!cacheDirectoryFile.exists()) {
            cacheDirectoryFile.mkdir();
        }   
        
        blockCacheDirectory = cacheDirectory + File.separator + BLOCK_CACHE_DIRECTORY;
        File blockCacheDirectoryFile = new File(blockCacheDirectory);
        if (!blockCacheDirectoryFile.exists()) {
            blockCacheDirectoryFile.mkdir();
        }   
    }
    
    
    /**
     * Replay from block cache.
     * Replay the blocks in the stack given from the block cache.
     * If any blocks are not cached then return that block as the new block hash to be the truncated block chain head.
     * @return StoredBlock The block from which conventional download replay must start.
     */
    public StoredBlock replayFromBlockCache(Stack<StoredBlock> blockStack) {
        log.debug("Replaying a stack of " + blockStack.size() + " from the block cache.");
        
        assert(CacheManager.INSTANCE.getController() != null);
        
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitService.replayingFromBlockCache")));
        StoredBlock newChainHead = blockStack.peek();
              
        while (!blockStack.isEmpty()) {
            newChainHead = blockStack.pop();
              
            // See if block is already cached.
            String blockFilename = blockCacheDirectory + File.separator + newChainHead.getHeader().getHashAsString() + MultiBitController.BLOCK_SUFFIX;
            File blockFile = new File(blockFilename);
            
            if (blockFile.exists()) {
                byte[] blockBytes;
                try {
                    blockBytes = getBytesFromFile(blockFile);
                    Block replayBlock = new Block(controller.getMultiBitService().getNetworkParameters(), blockBytes);
                    // Replay the block.
                    controller.onBlock(newChainHead, replayBlock);
                    controller.onBlocksDownloaded(null, replayBlock, -1);
                } catch (IOException e) {
                    // We did not successfully read the block so start downloading from here.
                    return newChainHead;
                } catch (ProtocolException e) {
                    // We did not successfully read the block so start downloading from here.
                    return newChainHead;
                }
            } else {
                // The block is not cached so start downloading.
                return newChainHead;
            }
        }
        
        return newChainHead;
    }
      
    private byte[] getBytesFromFile(File file) throws IOException {
        InputStream is = new FileInputStream(file);

        // Get the size of the file
        long length = file.length();

        // You cannot create an array using a long type.
        // It needs to be an int type.
        // Before converting to an int type, check
        // to ensure that file is not larger than Integer.MAX_VALUE.
        if (length > Integer.MAX_VALUE) {
            // File is too large
        }

        // Create the byte array to hold the data
        byte[] bytes = new byte[(int)length];

        // Read in the bytes
        int offset = 0;
        int numRead = 0;
        while (offset < bytes.length
               && (numRead=is.read(bytes, offset, bytes.length-offset)) >= 0) {
            offset += numRead;
        }

        // Ensure all the bytes have been read in
        if (offset < bytes.length) {
            throw new IOException("Could not completely read file "+file.getName());
        }

        // Close the input stream and return bytes
        is.close();
        return bytes;
    }

    public MultiBitController getController() {
        return controller;
    }

    public void setController(MultiBitController controller) {
        this.controller = controller;
    }

    public String getCacheDirectory() {
        return cacheDirectory;
    }

    public String getBlockCacheDirectory() {
        return blockCacheDirectory;
    }
    
    public File getBlockCacheFile(StoredBlock storedBlock) {
        String blockFilename = blockCacheDirectory + File.separator + storedBlock.getHeader().getHashAsString() + MultiBitController.BLOCK_SUFFIX;
        return new File(blockFilename);
    }
}
