package com.kuretru.api.common.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class HashUtilsTest {

    @Test
    void hmacSha256() {
        final String message = "中文测试123456ABCDEF";
        final String key = "ABCDEF123456中文测试";
        final String expected = "3868DE9C998F70AAF67522ABA215C1F725C05CD910A2CDDE017AD6F764EB600F";
        String result = HashUtils.hmacSha256(message, key);
        assertEquals(expected, result);
    }

    @Test
    void hmacMd5() {
        final String message = "中文测试123456ABCDEF";
        final String key = "ABCDEF123456中文测试";
        final String expected = "500894FB0746626B1E804B38DAB4572A";
        String result = HashUtils.hmacMd5(message, key);
        assertEquals(expected, result);
    }

}