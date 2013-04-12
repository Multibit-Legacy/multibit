/**
 * Copyright 2012 multibit.org
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

import junit.framework.TestCase;

import org.junit.Test;
import org.multibit.CreateControllers;
import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.viewsystem.swing.view.panels.ResetTransactionsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;

public class ResetTransactionsSubmitActionTest extends TestCase {
    public static final String EXPECTED_NO_WALLET_IS_SELECTED = "There is no wallet selected. Nothing to do.";

    @Test
    public void testNoWalletSelected() throws Exception {
        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers();
        MultiBitController controller = controllers.multiBitController;

        // This test runs against an empty PerWalletModelDataList.
        assertTrue("There was an active wallet when there should not be", controller.getModel().thereIsNoActiveWallet());

        // Create a new ResetTransactionsSubmitAction to test.
        FontSizer.INSTANCE.initialise(controller);
        ResetTransactionsPanel resetTransactionsPanel = new ResetTransactionsPanel(controller, null);
        ResetTransactionsSubmitAction resetTransactionsSubmitAction = resetTransactionsPanel.getResetTransactionsSubmitAction();

        assertNotNull("resetTransactionsSubmitAction was not created successfully", resetTransactionsSubmitAction);

        // Execute.
        resetTransactionsSubmitAction.actionPerformed(null);
        Object[] messages = MessageManager.INSTANCE.getMessages().toArray();
        assertTrue("There were no messages but there should have been", messages != null && messages.length > 0);
        assertEquals("Wrong message after reset transactions with no active wallet", EXPECTED_NO_WALLET_IS_SELECTED, ((Message)messages[messages.length - 1]).getText());
    }
}
