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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Stack;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

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
    
    private final int BUFFER = 2048;

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
        assert(CacheManager.INSTANCE.getController() != null);
        
        initialise();

        log.debug("Replaying a stack of " + blockStack.size() + " from the block cache.");
        
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitService.replayingFromBlockCache"), false));
        StoredBlock newChainHead = blockStack.peek();
              
        while (!blockStack.isEmpty()) {
            newChainHead = blockStack.pop();
              
            // See if block is already cached.
            String blockFilename = blockCacheDirectory + File.separator + newChainHead.getHeader().getHashAsString() + BLOCK_SUFFIX;
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
                MessageManager.INSTANCE.addMessage(new Message(" "));
                return newChainHead;
            }
        }
        
        MessageManager.INSTANCE.addMessage(new Message(" "));
        return newChainHead;
    }
    
    private void initialise() {
        if (applicationDataDirectory == null) {
            applicationDataDirectory = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory();
        }
        
        if (cacheDirectory == null) {
            cacheDirectory = applicationDataDirectory + File.separator + CACHE_DIRECTORY;
            File cacheDirectoryFile = new File(cacheDirectory);
            if (!cacheDirectoryFile.exists()) {
                cacheDirectoryFile.mkdir();
            }   
        }
        
        if (blockCacheDirectory == null) {
            blockCacheDirectory = cacheDirectory + File.separator + BLOCK_CACHE_DIRECTORY;
            File blockCacheDirectoryFile = new File(blockCacheDirectory);
            if (!blockCacheDirectoryFile.exists()) {
                blockCacheDirectoryFile.mkdir();
            }   
        }
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
    
    private void writeZipFile(String zipEntryName, byte[] bytes, File outputZipFile) {
        try {
            ByteArrayInputStream origin = null;
            FileOutputStream dest = new FileOutputStream(outputZipFile);
            ZipOutputStream out = new ZipOutputStream(new BufferedOutputStream(dest));
            out.setMethod(ZipOutputStream.DEFLATED);
            byte data[] = new byte[BUFFER];
            origin = new ByteArrayInputStream(bytes);
            ZipEntry entry = new ZipEntry(zipEntryName);
            out.putNextEntry(entry);
            int count;
            while ((count = origin.read(data, 0, BUFFER)) != -1) {
                out.write(data, 0, count);
            }
            origin.close();
            out.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private byte[] readZipFile(String zipEntryName, File inZipFile) {
        ByteArrayOutputStream dest = null;
        try {
            BufferedInputStream is = null;
            ZipEntry entry;
            ZipFile zipfile = new ZipFile(inZipFile);
            Enumeration<? extends ZipEntry> e = zipfile.entries();

            // Assume only one entry per zip.
            entry = (ZipEntry) e.nextElement();
            dest = new ByteArrayOutputStream();

            is = new BufferedInputStream(zipfile.getInputStream(entry));
            int count;
            byte data[] = new byte[BUFFER];

            while ((count = is.read(data, 0, BUFFER)) != -1) {
                dest.write(data, 0, count);
            }
            dest.flush();
            dest.close();
            is.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
        if (dest != null) {
            return dest.toByteArray();
        } else {
            return null;
        }
    }

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
        return getBlockCacheFile(storedBlock.getHeader());
    }    
    
    public File getBlockCacheFile(Block block) {
        initialise();
        String blockFilename = blockCacheDirectory + File.separator +  block.getHashAsString() + BLOCK_SUFFIX;
        return new File(blockFilename);
    }
}
