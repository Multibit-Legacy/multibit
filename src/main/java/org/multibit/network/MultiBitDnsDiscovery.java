/**
 * Copyright 2011 John Sample
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

package org.multibit.network;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.Date;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import javax.swing.SwingWorker;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.discovery.PeerDiscovery;
import com.google.bitcoin.discovery.PeerDiscoveryException;

/**
 * Supports peer discovery through DNS.
 * <p>
 * 
 * This class does not support the testnet as currently there are no DNS servers
 * providing testnet hosts. If this class is being used for testnet you must
 * specify the hostnames to use.
 * <p>
 * 
 * Failure to resolve individual host names will not cause an Exception to be
 * thrown. However, if all hosts passed fail to resolve a PeerDiscoveryException
 * will be thrown during getPeers().
 */
public class MultiBitDnsDiscovery implements PeerDiscovery {
    private static final Logger log = LoggerFactory.getLogger(MultiBitDnsDiscovery.class);

    private static final int SLEEP_PERIOD = 3000; // milliseconds
    private static final int MAXIMUM_CONNECT_TIME = 60 * 1000; // milliseconds
    private static final int MINIMUM_NUMBER_OF_PEERS_TO_FIND = 4;

    private String[] hostNames;
    private NetworkParameters netParams;

    private final Set<InetSocketAddress> addresses = new HashSet<InetSocketAddress>();

    public static final String[] defaultHosts = new String[] { 
            "seed.bitcoin.sipa.be",       // Pieter Wuille
            "dnsseed.bluematt.me",        // Matt Corallo
            "dnsseed.bitcoin.dashjr.org", // Luke Dashjr
            "bitseed.xf2.org"             // Jeff Garzik
    };
    
    private boolean canReturnCache = false;
    private boolean haveReturnedCurrentCache = false;
    private Random random;

    /**
     * Supports finding peers through DNS A records. Community run DNS entry
     * points will be used.
     * 
     * @param netParams
     *            Network parameters to be used for port information.
     */
    public MultiBitDnsDiscovery(NetworkParameters netParams) {
        this.hostNames = defaultHosts;
        this.netParams = netParams;
        
        random = new Random();
    }

    public InetSocketAddress[] getPeers() throws PeerDiscoveryException {
        if (canReturnCache && !haveReturnedCurrentCache) {
            InetSocketAddress[] addressesToReturn;
            
            synchronized(addresses) {
                addressesToReturn = createAddressesArray(addresses);
            }
            haveReturnedCurrentCache = true;
            
            return addressesToReturn;
        }
        
        canReturnCache = false;
        haveReturnedCurrentCache = false;
 
        // Clear any existing addresses found as they may be offline.
        synchronized(addresses) {
            addresses.clear();
        }    
     
        for (String hostName : hostNames) {
            try {
                // Find inetSocketAddresses in background.
                findPeerAddressesInBackground(hostName);
            } catch (Exception e) {
                log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
            }
        }

        // Wait until we see at least the minimum required addresses 
        int waitTime = 0;
        while (waitTime < MAXIMUM_CONNECT_TIME 
                && addresses.size() < MINIMUM_NUMBER_OF_PEERS_TO_FIND) {
            try {
                log.debug("At time '" + waitTime + "' had found " +  addresses.size() + " peers.");
                Thread.sleep(SLEEP_PERIOD);
                waitTime = waitTime + SLEEP_PERIOD;
            } catch (InterruptedException e) {
                log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
            }
        }
        log.debug("At time '" + waitTime + "' had found " +  addresses.size() + " peers.");
        
        if (addresses.size() >= 1) {
            canReturnCache = true;
        }

        InetSocketAddress[] addressesToReturn;
        
        synchronized(addresses) {
            addressesToReturn = createAddressesArray(addresses);
//            for (int i = 0; i < addressesToReturn.length; i++) {
//                log.debug("addressToReturn(" + i + ") = " +  addressesToReturn[i].toString());
//            }
        }
        return addressesToReturn;
    }

    /**
     * Create randomised list of peers.
     */
    private InetSocketAddress[] createAddressesArray(Set<InetSocketAddress> addresses) {
        InetSocketAddress[] peersArray = addresses.toArray(new InetSocketAddress[] {});
        
        // Randomly swop the first half of the addresses with any other for load balancing across clients.
        for (int i = 0; i < (int)(peersArray.length *.5); i++) {
            InetSocketAddress temp = peersArray[i];
            int randomNumber = random.nextInt(peersArray.length);
            
            peersArray[i] = peersArray[randomNumber];
            peersArray[randomNumber] = temp;
        }
        
        return peersArray;
    }
    
    /**
     * Returns the well known discovery host names on the production network.
     */
    public static String[] getDefaulHostNames() {
        return defaultHosts;
    }

    /** We don't have a way to abort a DNS lookup, so this does nothing */
    public void shutdown() {
    }

    /**
     * Get the peer addresses in a background Swing worker thread.
     */
    private void findPeerAddressesInBackground(final String hostName) {
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            private Set<InetSocketAddress> backgroundAddresses = new HashSet<InetSocketAddress>();

            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure = Boolean.FALSE;

                try {
                    log.debug("Discovery of all addresses for '" + hostName + "' started at + " + (new Date()).toString());
                    InetAddress[] hostAddresses = InetAddress.getAllByName(hostName);
                    log.debug("Discovery of all addresses for '" + hostName + "' finished at + " + (new Date()).toString());

                    for (InetAddress inetAddress : hostAddresses) {
                        // DNS isn't going to provide us with the port.
                        // Grab the port from the specified NetworkParameters.
                        InetSocketAddress socketAddress = new InetSocketAddress(inetAddress, netParams.port);

                        // Only add the new address if it's not already in the
                        // combined list.
                        if (!backgroundAddresses.contains(socketAddress)) {
                            backgroundAddresses.add(socketAddress);
                        }
                    }
                    successMeasure = Boolean.TRUE;

                    log.debug("Discovery of hostname '" + hostName + "' adding addresses finished at + " + (new Date()).toString());
                } catch (Exception e2) {
                    log.info("DNS lookup for " + hostName + " failed. Error was '" + e2.getClass() + " " + e2.getMessage());
                }

                return successMeasure;
            }

            protected void done() {
                try {
                    // Add addresses for this host onto the allhosts - set.
                    Boolean wasSuccessful = get();

                    if (wasSuccessful) {
                        synchronized (addresses) {
                            addresses.addAll(backgroundAddresses);
                        }
                    }
                } catch (Exception e) {
                    // Not really used but caught so that SwingWorker shuts down
                    // cleanly.
                    log.error(e.getClass() + " " + e.getMessage());
                }
            }
        };
        log.debug("Finding peers in background SwingWorker thread for host " + hostName);
        worker.execute();
    }
}
