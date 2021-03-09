package com.kuretru.api.common.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class StringUtilsTest {

    @Test
    void bytesToHexString() {
        String expected = "123456ABCDEF";
        final byte[] bytes = {0x12, 0x34, 0x56, (byte)0xAB, (byte)0xCD, (byte)0xEF};
        String text = StringUtils.bytesToHexString(bytes);
        assertEquals(expected, text);
    }

}