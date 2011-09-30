/**
 * Copyright 2011 MultiBit
 */

package org.multibit.network;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.Localiser;
import org.multibit.Constants;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.Wallet;

public class FileHandlerTest extends TestCase {

    public static final String WALLET_TESTDATA_DIRECTORY = "wallets";

    public static final String WALLET_TEST1 = "test1.wallet";
    public static final BigInteger WALLET_TEST1_BALANCE = new BigInteger("6700000");;

    public static final String WALLET_TEST2 = "test2.wallet";
    public static final BigInteger WALLET_TEST2_BALANCE = new BigInteger("2000000");;

    @Test
    public void testLoadTest1() {
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        FileHandler fileHandler = new FileHandler(controller);

        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator + WALLET_TESTDATA_DIRECTORY + File.separator
                + WALLET_TEST1;

        File walletFile = new File(walletName);
        try {
            walletFile.createNewFile();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        assertTrue(walletFile.exists());
        Wallet wallet = fileHandler.loadWalletFromFile(walletFile);

        assertNotNull(wallet);

        assertEquals(WALLET_TEST1_BALANCE, wallet.getBalance());
    }

    @Test
    public void testLoadTest2() {
        MultiBitController controller = new MultiBitController();
        Localiser localiser = new Localiser();
        MultiBitModel model = new MultiBitModel(controller);

        controller.setLocaliser(localiser);
        controller.setModel(model);

        FileHandler fileHandler = new FileHandler(controller);

        File directory = new File(".");
        String currentPath = directory.getAbsolutePath();

        String walletName = currentPath + File.separator + Constants.TESTDATA_DIRECTORY + File.separator + WALLET_TESTDATA_DIRECTORY + File.separator
                + WALLET_TEST2;

        File walletFile = new File(walletName);
        try {
            walletFile.createNewFile();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        assertTrue(walletFile.exists());
        Wallet wallet = fileHandler.loadWalletFromFile(walletFile);

        assertNotNull(wallet);

        assertEquals(WALLET_TEST2_BALANCE, wallet.getBalance());
    }
}
