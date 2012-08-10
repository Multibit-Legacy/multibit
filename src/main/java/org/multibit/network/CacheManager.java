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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
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

    private static int SIZE = 128 * 1024;
    
    private String applicationDataDirectory = null;
    private String cacheDirectory = null;
    private String blockCacheDirectory = null;
    private MultiBitController controller = null;
      
    CacheManager() {  
    }
    
    
    /**
     * Replay from block cache.
     * Replay the blocks in the stack given from the block cache.
     * If any blocks are not cached then return that block as the new block hash to be the truncated block chain head.
     * @return StoredBlock The block from which conventional download replay must start.
     */
    public StoredBlock replayFromBlockCache(Stack<StoredBlock> blockStack) {
        System.out.println("CM Ping 1");

        assert(CacheManager.INSTANCE.getController() != null);
        
        initialise();

        log.debug("Replaying a stack of " + blockStack.size() + " from the block cache.");
        MessageManager.INSTANCE.addMessage(new Message("Replaying a stack of " + blockStack.size() + " from the block cache."));
        System.out.println("CM Ping 2");
               
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitService.replayingFromBlockCache"), false));
        StoredBlock newChainHead = blockStack.peek();
        System.out.println("CM Ping 3");

        while (!blockStack.isEmpty()) {
 
            newChainHead = blockStack.pop();
              
            boolean getTimings = false;
            if (blockStack.size() % 1000 == 0) {
                log.debug("The block stack is of size " + blockStack.size());
                MessageManager.INSTANCE.addMessage(new Message("The block stack is of size " + blockStack.size()));
                getTimings = true;
            }
 
            long time1 = 0, time2 = 0, time3 = 0, time4 = 0, time5 = 0;
            if (getTimings) time1 = System.currentTimeMillis();

            // See if block is already cached.
            String blockFilename = blockCacheDirectory + File.separator + newChainHead.getHeader().getHashAsString() + BLOCK_SUFFIX;
            File blockFile = new File(blockFilename);
            
            if (blockFile.exists()) {
                byte[] blockBytes;
                try {
                    blockBytes = getBytesFromFile(blockFile);
                    if (getTimings) time2 = System.currentTimeMillis();
                    Block replayBlock = new Block(controller.getModel().getNetworkParameters(), blockBytes);
                    if (getTimings) time3 = System.currentTimeMillis();
                    // Replay the block.
                    controller.onBlock(newChainHead, replayBlock);
                    if (getTimings) time4 = System.currentTimeMillis();
                    controller.onBlocksDownloaded(null, replayBlock, -1);
                    if (getTimings)  {
                        time5 = System.currentTimeMillis();
                        log.debug("Timings : " + (time2 - time1) + ", " +  (time3 - time2) + ", " +  (time4 - time3) + ", " +  (time5 - time4)) ;                  
                    }
                } catch (IOException e) {
                    // We did not successfully read the block so start downloading from here.
                    String messageText = e.getClass().getCanonicalName() + " " +e.getMessage();
                    log.error(messageText);
                    MessageManager.INSTANCE.addMessage(new Message(messageText));

                    return newChainHead;
                } catch (ProtocolException e) {
                    // We did not successfully read the block so start downloading from here.
                    String messageText = e.getClass().getCanonicalName() + " " +e.getMessage();
                    log.error(messageText);
                    MessageManager.INSTANCE.addMessage(new Message(messageText));
                    return newChainHead;
                }
            } else {
                // The block is not cached so start downloading.
                MessageManager.INSTANCE.addMessage(new Message("Start downloading from block height " + newChainHead.getHeight()));
                return newChainHead;
            }
        }
        System.out.println("CM Ping 4");

        MessageManager.INSTANCE.addMessage(new Message(" "));
        return newChainHead;
    }
    
    public void initialise() {
        //System.out.println("CM I Ping 1");
        if (applicationDataDirectory == null) {
            applicationDataDirectory = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory();
        }
        //System.out.println("CM I Ping 2");
        
        if (cacheDirectory == null) {
            cacheDirectory = applicationDataDirectory + File.separator + CACHE_DIRECTORY;
            File cacheDirectoryFile = new File(cacheDirectory);
            if (cacheDirectoryFile != null && !cacheDirectoryFile.exists()) {
                cacheDirectoryFile.mkdir();
            }   
        }
        //System.out.println("CM I Ping 3");
        
        if (blockCacheDirectory == null) {
            blockCacheDirectory = cacheDirectory + File.separator + BLOCK_CACHE_DIRECTORY;
            File blockCacheDirectoryFile = new File(blockCacheDirectory);
            if (blockCacheDirectoryFile != null && !blockCacheDirectoryFile.exists()) {
                blockCacheDirectoryFile.mkdir();
            }   
        }
        //System.out.println("CM I Ping 4");
    }
      
    public void writeFile(Block block) {
        initialise();
        File blockFile = CacheManager.INSTANCE.getBlockCacheFile(block);

        if ((blockFile != null) && !blockFile.exists()) {
            try {
                block.bitcoinSerialize(new FileOutputStream(blockFile));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
    
    private byte[] getBytesFromFile(File file) throws IOException {
        byte[] toReturn = null;
        RandomAccessFile out = null;

        try {
            out = new RandomAccessFile(file.getAbsolutePath(), "r");
            FileChannel fileChannel = out.getChannel();

            // FileChannel fileChannel = new FileInputStream(file).getChannel();
            ByteBuffer buffer = ByteBuffer.allocate((int) fileChannel.size());
            fileChannel.read(buffer);
            toReturn = buffer.array();
        } finally {
            if (out != null) {
                out.close();
            }
        }
        return toReturn;
    }
    
//    private byte[] getBytesFromFile(File file) throws IOException {
//        InputStream inputStream = new FileInputStream(file);
//
//        // Get the size of the file
//        long length = file.length();
//
//        // You cannot create an array using a long type.
//        // It needs to be an int type.
//        // Before converting to an int type, check
//        // to ensure that file is not larger than Integer.MAX_VALUE.
//        if (length > Integer.MAX_VALUE) {
//            // File is too large
//        }
//
//        // Create the byte array to hold the data
//        byte[] bytes = new byte[(int)length];
//
//        // Read in the bytes
//        int offset = 0;
//        int numRead = 0;
//        while (offset < bytes.length
//               && (numRead = inputStream.read(bytes, offset, bytes.length-offset)) >= 0) {
//            offset += numRead;
//        }
//
//        // Ensure all the bytes have been read in
//        if (offset < bytes.length) {
//            throw new IOException("Could not completely read file " + file.getName());
//        }
//
//        // Close the input stream and return bytes
//        inputStream.close();
//        return bytes;
//    }

    public MultiBitController getController() {
        return controller;
    }

    public void setController(MultiBitController controller) {
        this.controller = controller;
    }

    public String getCacheDirectory() {
        initialise();
        return cacheDirectory;
    }

    public String getBlockCacheDirectory() {
        initialise();
        return blockCacheDirectory;
    }
    
    public File getBlockCacheFile(StoredBlock storedBlock) {
        if (storedBlock == null || storedBlock.getHeader() == null) {
            return null;
        }
        return getBlockCacheFile(storedBlock.getHeader());
    }    
    
    public File getBlockCacheFile(Block block) {
        initialise();
        String blockFilename = blockCacheDirectory + File.separator +  block.getHashAsString() + BLOCK_SUFFIX;
        return new File(blockFilename);
    }
}

