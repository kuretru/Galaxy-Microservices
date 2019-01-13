package com.kuretru.api.common.util;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class StringUtilsTest {

    @Test
    public void isNullOrEmpty() {
        String nullString = null;
        String emptyString = "";
        String normalString = "hello world";
        assertTrue(StringUtils.isNullOrEmpty(nullString));
        assertTrue(StringUtils.isNullOrEmpty(emptyString));
        assertFalse(StringUtils.isNullOrEmpty(normalString));
    }

}