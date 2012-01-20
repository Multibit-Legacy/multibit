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
package org.multibit.action;

import org.multibit.model.DataProvider;

public interface Action {
    public static final int HOME_PAGE_ACTION = 1001;
    public static final int SEND_BITCOINS_ACTION = 1002;
    public static final int SEND_BITCOINS_CONFIRM_ACTION = 1003;
    public static final int RECEIVE_BITCOINS_ACTION = 1004;
    public static final int HELP_CONTENTS_ACTION = 1005;
    public static final int HELP_ABOUT_ACTION = 1006;
    public static final int ADDRESS_BOOK_ACTION = 1007;
    public static final int OPEN_WALLET_ACTION = 1008;
    public static final int SAVE_WALLET_AS_ACTION = 1009;;

    /**
     * execute the action using data from the data provider
     */
    public void execute(DataProvider dataProvider);
}
