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
package org.multibit.utils;

import java.io.IOException;

import junit.framework.TestCase;

import org.junit.Test;

public class WhitespaceTrimmerTest extends TestCase {
    @Test
    public void testLTrim() throws IOException {
        assertEquals("noTrim", WhitespaceTrimmer.ltrim("noTrim"));
        assertEquals("noEmbedded Trim", WhitespaceTrimmer.ltrim("noEmbedded Trim"));

        assertEquals("leftTrim", WhitespaceTrimmer.ltrim(" leftTrim"));
        assertEquals("leftTrim", WhitespaceTrimmer.ltrim("\nleftTrim"));
        assertEquals("leftTrim", WhitespaceTrimmer.ltrim("\tleftTrim"));
        assertEquals("noRightTrim ", WhitespaceTrimmer.ltrim("noRightTrim "));
        assertEquals("leftTrimNoRightTrim ", WhitespaceTrimmer.ltrim(" leftTrimNoRightTrim "));
    }

    @Test
    public void testRTrim() throws IOException {
        assertEquals("noTrim", WhitespaceTrimmer.rtrim("noTrim"));
        assertEquals("noEmbedded Trim", WhitespaceTrimmer.rtrim("noEmbedded Trim"));

        assertEquals("rightTrim", WhitespaceTrimmer.rtrim("rightTrim "));
        assertEquals("rightTrim", WhitespaceTrimmer.rtrim("rightTrim\n"));
        assertEquals("rightTrim", WhitespaceTrimmer.rtrim("rightTrim\t"));
        assertEquals(" noLeftTrim", WhitespaceTrimmer.rtrim(" noLeftTrim"));
        assertEquals(" rightTrimNoLeftTrim", WhitespaceTrimmer.rtrim(" rightTrimNoLeftTrim\n"));
    }

    @Test
    public void testTrim() throws IOException {
        assertEquals("noTrim", WhitespaceTrimmer.trim("noTrim"));
        assertEquals("noEmbedded Trim", WhitespaceTrimmer.trim("noEmbedded Trim"));

        assertEquals("rightTrim", WhitespaceTrimmer.trim("rightTrim "));
        assertEquals("leftTrim", WhitespaceTrimmer.trim(" leftTrim"));
        assertEquals("rightTrimAndLeftTrim", WhitespaceTrimmer.trim(" rightTrimAndLeftTrim "));
        assertEquals("rightTrimAndLeftTrim", WhitespaceTrimmer.trim("\nrightTrimAndLeftTrim\n"));
        assertEquals("rightTrimAndLeftTrim", WhitespaceTrimmer.trim("\trightTrimAndLeftTrim\t"));
    }
}
