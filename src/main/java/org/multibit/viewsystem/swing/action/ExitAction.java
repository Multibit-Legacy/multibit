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
package org.multibit.viewsystem.swing.action;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.multibit.ApplicationInstanceManager;
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletSaveException;
import org.multibit.file.WalletVersionException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Exit the application.
 * 
 * @author jim
 * 
 */
public class ExitAction extends AbstractAction {
    private static final long serialVersionUID = 8784284740245520863L;
    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private static final Logger log = LoggerFactory.getLogger(ExitAction.class);

    /**
     * Creates a new {@link ExitAction}.
     */
    public ExitAction(MultiBitController controller, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("exitAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("exitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("exitAction.mnemonicKey"));
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        log.debug("exit 1");
        if (mainFrame != null) {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                }});
           
        }
        log.debug("exit 2");

        // Save all the wallets and put their filenames in the user preferences.
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                try {
                    log.debug("exit 3a");
                    controller.getFileHandler().savePerWalletModelData(loopPerWalletModelData, false);
                    log.debug("exit 3b");
                } catch (WalletSaveException wse) {
                    log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                    MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
                    
                    // Save to backup.
                    try {
                        log.debug("exit 4a");
                        controller.getFileHandler().backupPerWalletModelData(loopPerWalletModelData, null);
                        log.debug("exit 4b");
                    } catch (WalletSaveException wse2) {
                        log.error(wse2.getClass().getCanonicalName() + " " + wse2.getMessage());
                        MessageManager.INSTANCE.addMessage(new Message(wse2.getClass().getCanonicalName() + " " + wse2.getMessage()));
                    }
                } catch (WalletVersionException wve) {
                    log.error(wve.getClass().getCanonicalName() + " " + wve.getMessage());
                    MessageManager.INSTANCE.addMessage(new Message(wve.getClass().getCanonicalName() + " " + wve.getMessage()));
                }
            }
        }
        log.debug("exit 5");

        // Write the user properties.
        log.debug("Saving user preferences ...");
        FileHandler.writeUserPreferences(controller);
        log.debug("exit 6");

        log.debug("Shutting down Bitcoin URI checker ...");
        ApplicationInstanceManager.shutdownSocket();
        log.debug("exit 7");

        // Get rid of main display.
        if (mainFrame != null) {
            mainFrame.setVisible(false);
        }
        log.debug("exit 8");

        if (controller.getMultiBitService() != null && controller.getMultiBitService().getPeerGroup() != null) {
            log.debug("Closing Bitcoin network connection...");
            @SuppressWarnings("rawtypes")
            SwingWorker worker = new SwingWorker() {
                @Override
                protected Object doInBackground() throws Exception {
                    log.debug("exit background-a");
                    controller.getMultiBitService().getPeerGroup().stop();
                    log.debug("exit background-b");
                    return null; // return not used
                }
            };
            worker.execute();
        }
        log.debug("exit 8");


        if (mainFrame != null) {
            mainFrame.dispose();
        }
        log.debug("exit 9");

        System.exit(0);
        log.debug("exit 10");

    }
}
