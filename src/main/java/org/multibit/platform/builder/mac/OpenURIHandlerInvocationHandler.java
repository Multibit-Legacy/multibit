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
package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericOpenURIHandler;
import org.multibit.platform.listener.GenericOpenURIEvent;

/**
 * <p>Implementation of invocation handler for OpenURI</p>
 */
public class OpenURIHandlerInvocationHandler extends BaseMacInvocationHandler<GenericOpenURIHandler, GenericOpenURIEvent> {

    OpenURIHandlerInvocationHandler(GenericOpenURIHandler genericHandler, Class<GenericOpenURIEvent> genericEventClass) {
        super(genericHandler, genericEventClass);
    }
}
