package com.kuretru.api.common.util;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class HashUtilsTest {

    @Test
    public void computeMd5() {
        String result = "e10adc3949ba59abbe56e057f20f883e";
        assertEquals(result, HashUtils.computeMd5("123456"));
    }

}