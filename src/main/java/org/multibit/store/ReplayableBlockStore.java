/**
 * Copyright 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.multibit.store;

import com.google.bitcoin.core.*;
import com.google.bitcoin.utils.NamedSemaphores;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import static com.google.common.base.Preconditions.checkState;

import org.multibit.IsMultiBitClass;
import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;

import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;

/**
 * Stores the block chain to disk.<p>
 *
 * This implementation is designed to have constant memory usage, regardless of the size of the block chain being
 * stored. It exploits operating system level buffering and the fact that get() requests are, in normal usage,
 * localized in chain space.<p>
 *
 * Blocks are stored sequentially. Most blocks are fetched out of a small in-memory cache. The slowest part is
 * traversing difficulty transition points, which requires seeking backwards over around 2000 blocks. On a Google
 * Nexus S phone this takes a couple of seconds. On a MacBook Pro it takes around 50msec.<p>
 *
 * The store has much room for optimization. Expanding the size of the cache will likely allow us to traverse
 * difficulty transitions without using too much memory and without hitting the disk at all, for the case of initial
 * block chain download. Storing the hashes on disk would allow us to avoid deserialization and hashing which is
 * expensive on Android.
 *
 * This variant of BlockStore has the ability to replay blocks
 */
public class ReplayableBlockStore implements BlockStore, IsMultiBitClass {
    private static final Logger log = LoggerFactory.getLogger(ReplayableBlockStore.class);
    private static final byte FILE_FORMAT_VERSION = 1;
    private static NamedSemaphores semaphores = new NamedSemaphores();

    private RandomAccessFile file;
    // We keep some recently found blocks in the blockCache. It can help to optimize some cases where we are
    // looking up blocks we recently stored or requested. When the cache gets too big older entries are deleted.
    private LinkedHashMap<Sha256Hash, StoredBlock> blockCache = new LinkedHashMap<Sha256Hash, StoredBlock>() {
        private static final long serialVersionUID = -6744550980315506146L;

        @Override
        protected boolean removeEldestEntry(Map.Entry<Sha256Hash, StoredBlock> entry) {
            return size() > 2050;  // Slightly more than the difficulty transition period.
        }
    };
    // Use a separate cache to track get() misses. This is to efficiently handle the case of an unconnected block
    // during chain download. Each new block will do a get() on the unconnected block so if we haven't seen it yet we
    // must efficiently respond.
    //
    // We don't care about the value in this cache. It is always notFoundMarker. Unfortunately LinkedHashSet does not
    // provide the removeEldestEntry control.
    private static final StoredBlock notFoundMarker = new StoredBlock(null, null, -1);
    private LinkedHashMap<Sha256Hash, StoredBlock> notFoundCache = new LinkedHashMap<Sha256Hash, StoredBlock>() {

        private static final long serialVersionUID = -2985188391503596244L;

        @Override
        protected boolean removeEldestEntry(Map.Entry<Sha256Hash, StoredBlock> entry) {
            return size() > 100;  // This was chosen arbitrarily.
        }
    };

    private Sha256Hash chainHead;
    private final NetworkParameters params;
    private FileChannel channel;
    private FileLock lock;
    private String fileName;

    private static class Record {
        // A BigInteger representing the total amount of work done so far on this chain. As of May 2011 it takes 8
        // bytes to represent this field, so 16 bytes should be plenty for a long time.
        private static final int CHAIN_WORK_BYTES = 16;
        private static final byte[] EMPTY_BYTES = new byte[CHAIN_WORK_BYTES];

        private int height;           // 4 bytes
        private byte[] chainWork;     // 16 bytes
        private byte[] blockHeader;   // 80 bytes

        public static final int SIZE = 4 + Record.CHAIN_WORK_BYTES + Block.HEADER_SIZE;

        public Record() {
            height = 0;
            chainWork = new byte[CHAIN_WORK_BYTES];
            blockHeader = new byte[Block.HEADER_SIZE];
        }

        public static void write(FileChannel channel, StoredBlock block) throws IOException {
            ByteBuffer buf = ByteBuffer.allocate(Record.SIZE);
            buf.putInt(block.getHeight());
            byte[] chainWorkBytes = block.getChainWork().toByteArray();
            checkState(chainWorkBytes.length <= CHAIN_WORK_BYTES, "Ran out of space to store chain work!");
            if (chainWorkBytes.length < CHAIN_WORK_BYTES) {
                // Pad to the right size.
                buf.put(EMPTY_BYTES, 0, CHAIN_WORK_BYTES - chainWorkBytes.length);
            }
            buf.put(chainWorkBytes);
            buf.put(block.getHeader().bitcoinSerialize());
            buf.position(0);
            channel.position(channel.size());
            if (channel.write(buf) < Record.SIZE)
                throw new IOException("Failed to write record!");
            channel.position(channel.size() - Record.SIZE);
        }

        public boolean read(FileChannel channel, long position, ByteBuffer buffer) throws IOException {
            buffer.position(0);
            long bytesRead = channel.read(buffer, position);
            if (bytesRead < Record.SIZE)
                return false;
            buffer.position(0);
            height = buffer.getInt();
            buffer.get(chainWork);
            buffer.get(blockHeader);
            return true;
        }

        public BigInteger getChainWork() {
            return new BigInteger(1, chainWork);
        }

        public Block getHeader(NetworkParameters params) throws ProtocolException {
            return new Block(params, blockHeader);
        }

        public int getHeight() {
            return height;
        }

        public StoredBlock toStoredBlock(NetworkParameters params) throws ProtocolException {
            return new StoredBlock(getHeader(params), getChainWork(), getHeight());
        }
    }
    
    public ReplayableBlockStore(NetworkParameters params, File file, boolean alwaysCreateNewStore) throws BlockStoreException {
        this.params = params;
        this.fileName = file.getAbsolutePath();
        
        if (alwaysCreateNewStore) {
            createBlockStore(params, file, false);
        } else {
            try {
                load(file);
            } catch (Exception e) {
                log.error("failed to load block store from file", e);
                createBlockStore(params, file, true);
            }
        }
    }

    synchronized private void createBlockStore(NetworkParameters params, File file, boolean copyInstalledBlockChain) throws BlockStoreException {
        // Create a new block store if the file wasn't found or anything went wrong whilst reading.
        clearCaches();
        try {
            boolean blockChainLoadedOk = false;
            if (copyInstalledBlockChain) {
                // Recopy in the installer blockchain.
                MultiBitController controller = MultiBit.getController();
                FileHandler fileHandler = controller.getFileHandler();
                if (fileHandler != null) {
                    fileHandler.copyBlockChainFromInstallationDirectory(file.getAbsolutePath(), true);

                    if (file.exists() && file.length() > 0) {
                        load(file);
                        blockChainLoadedOk = true;
                    }
                }
            } 
            
            if (!blockChainLoadedOk) {
                // Could not read in the original blockchain and copying in the
                // installed version failed. Create a new one.
                if (file.exists()) {
                    file.delete();
                }
                // Set up the genesis block. When we start out fresh, it is by
                // definition the top of the chain.
                Block genesis = params.genesisBlock.cloneAsHeader();
                StoredBlock storedGenesis = new StoredBlock(genesis, genesis.getWork(), 0);

                // Create fresh. The d makes writes synchronous.
                this.file = new RandomAccessFile(file, "rwd");
                this.channel = this.file.getChannel();
                this.file.write(FILE_FORMAT_VERSION);
                this.chainHead = storedGenesis.getHeader().getHash();
                this.file.write(this.chainHead.getBytes());
                put(storedGenesis);
              
                // truncate to just the genesis block
                setChainHeadAndTruncate(storedGenesis);
            }
        } catch (VerificationException e1) {
            throw new RuntimeException(e1);  // Cannot happen.
        } catch (IOException e) {
            throw new BlockStoreException(e);
        }
    }

    private void load(File file) throws IOException, BlockStoreException {
        log.info("Reading block store from {}", file);
        // Open in synchronous mode. See above.
        this.file = new RandomAccessFile(file, "rwd");
        channel = this.file.getChannel();
        log.info("Reading block store from {}", file);
        
        if (lock == null) {
           lock();
        }
        try {
            // Read a version byte.
            int version = this.file.read();
            if (version == -1) {
                // No such file or the file was empty.
                close();
                throw new FileNotFoundException(file.getName() + " does not exist or is empty");
            }
            if (version != FILE_FORMAT_VERSION) {
                throw new BlockStoreException("Bad version number: " + version);
            }
            // Chain head pointer is the first thing in the file.
            byte[] chainHeadHash = new byte[32];
            if (this.file.read(chainHeadHash) < chainHeadHash.length)
                throw new BlockStoreException("Truncated store: could not read chain head hash.");
            this.chainHead = new Sha256Hash(chainHeadHash);
            log.info("Read chain head from disk: {}", this.chainHead);
            channel.position(channel.size() - Record.SIZE);
        } catch (IOException e) {
            if (channel != null) {
                channel.close();
            }
            if (this.file != null) {
                this.file.close(); 
            }
            throw e;
        } catch (BlockStoreException e) {
            if (channel != null) {
                channel.close();
            }
            if (this.file != null) {
                this.file.close(); 
            }
            throw e;
        }
    }

    private void lock() throws IOException, BlockStoreException {
        if (!semaphores.tryAcquire(fileName)) {
            throw new BlockStoreException("File in use");
        }
        try {
            lock = channel.tryLock();
        } catch (OverlappingFileLockException e) {
            semaphores.release(fileName);
            lock = null;
        }
        if (lock == null) {
            try {
                this.file.close();
            } finally {
                this.file = null;
            }
            throw new BlockStoreException("Could not lock file");
        }
    }

    private void ensureOpen() throws BlockStoreException {
        if (file == null) {
            throw new BlockStoreException("BlockStore was closed");
        }
    }

    @Override
    public synchronized void put(StoredBlock block) throws BlockStoreException {
        ensureOpen();
        try {
            Sha256Hash hash = block.getHeader().getHash();
            // Append to the end of the file.
            Record.write(channel, block);
            blockCache.put(hash, block);
        } catch (IOException e) {
            throw new BlockStoreException(e);
        }
    }

    @Override
    public synchronized StoredBlock get(Sha256Hash hash) throws BlockStoreException {
        ensureOpen();
        // Check the memory cache first.
        StoredBlock fromMem = blockCache.get(hash);
        if (fromMem != null) {
            return fromMem;
        }
        if (notFoundCache.get(hash) == notFoundMarker) {
            return null;
        }

        try {
            Record fromDisk = getRecord(hash);
            StoredBlock block = null;
            if (fromDisk == null) {
                notFoundCache.put(hash, notFoundMarker);
            } else {
                block = fromDisk.toStoredBlock(params);
                blockCache.put(hash, block);
            }
            return block;
        } catch (IOException e) {
            throw new BlockStoreException(e);
        } catch (ProtocolException e) {
            throw new BlockStoreException(e);
        }
    }

    private ByteBuffer buf = ByteBuffer.allocateDirect(Record.SIZE);

    private Record getRecord(Sha256Hash hash) throws BlockStoreException, IOException, ProtocolException {
        long startPos = channel.position();
        // Use our own file pointer within the tight loop as updating channel positions is really expensive.
        long pos = startPos;
        Record record = new Record();
        int numMoves = 0;
        long startTime = new Date().getTime();
        do {
            if (!record.read(channel, pos, buf))
                throw new IOException("Failed to read buffer");
            if (record.getHeader(params).getHash().equals(hash)) {
                // Found it. Update file position for next time.
                channel.position(pos);
                long endTime = new Date().getTime();
                if (endTime - startTime > 100) {
                    log.info("Spent {} seconds doing {} backwards seeks", (endTime - startTime) / 1000.0, numMoves);
                }
                return record;
            }
            // Did not find it.
            if (pos == 1 + 32) {
                // At the start so wrap around to the end.
                pos = channel.size() - Record.SIZE;
            } else {
                // Move backwards.
                pos = pos - Record.SIZE;
                checkState(pos >= 1 + 32, pos);
            }
            numMoves++;
        } while (pos != startPos);
        // Was never stored.
        channel.position(pos);
        long endTime = new Date().getTime();
        if (endTime - startTime > 1000) {
            log.info("Spent {} seconds doing {} backwards seeks", (endTime - startTime) / 1000.0, numMoves);
        }
        return null;
    }

    @Override
    public synchronized StoredBlock getChainHead() throws BlockStoreException {
        ensureOpen();
        // This will hit the cache
        StoredBlock head = get(chainHead);
        if (head == null)
            throw new BlockStoreException("Corrupted block store: chain head not found");
        return head;
    }

    @Override
    public synchronized void setChainHead(StoredBlock chainHead) throws BlockStoreException {
        ensureOpen();
        try {
            this.chainHead = chainHead.getHeader().getHash();
            // Write out new hash to the first 32 bytes of the file past one (first byte is version number).
            channel.write(ByteBuffer.wrap(this.chainHead.getBytes()), 1);
        } catch (IOException e) {
            throw new BlockStoreException(e);
        }
    }

    /**
     * Set the chainhead for the blockstore to the specified block and delete
     * all blocks that were received later than this. This functionality is for
     * blockchain replay
     * 
     * @param chainHead
     * @throws BlockStoreException
     * @throws ProtocolException
     * @throws IOException
     */
    public synchronized void setChainHeadAndTruncate(StoredBlock chainHead) throws BlockStoreException {
        try {
            Block chainHeadBlock = chainHead.getHeader();

            setChainHead(chainHead);

            // Find block as a record and delete past it.
            Record chainHeadRecord = getRecord(chainHeadBlock.getHash());
            if (chainHeadRecord != null) {
                // The record was found.

                // Set the length of the file to be the end of the current record.
                log.debug("File length before truncate was " + file.length());
                file.setLength(channel.position() + Record.SIZE);
                log.debug("File length is now " + file.length());
            }

            // Also clear the caches so that there are no references to later blocks
            clearCaches();
        } catch (IOException e) {
            throw new BlockStoreException(e);
        } catch (ProtocolException e) {
            throw new BlockStoreException(e);
        }
    }

    public RandomAccessFile getFile() {
        return file;
    }

    private synchronized void clearCaches() {
        blockCache.clear();
        notFoundCache.clear();
    }
    
    @Override
    public void close() throws BlockStoreException {
        ensureOpen();
        try {
            file.close();
        } catch (IOException e) {
            throw new BlockStoreException(e);
        } finally {
            try {
                semaphores.release(this.fileName);
                file = null;
            } catch (NullPointerException npe) {
                log.debug(npe.toString());
            }
        }
    }
}
