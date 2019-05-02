package com.kuretru.api.common.util;

import com.kuretru.api.common.configuration.GeneralConstants;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

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

    @Test
    public void listToString() {
        List<String> list = new ArrayList<>();
        list.add("xxx");
        list.add("yyy");
        String result = StringUtils.listToString(list, GeneralConstants.ITEMS_RAW_SEPARATOR);
        assertEquals("xxx|yyy", result);
    }

    @Test
    public void stringToList() {
        String text = "xxx|yyy";
        List<String> list = StringUtils.stringToList(text, GeneralConstants.ITEMS_SEPARATOR);
        assertEquals("xxx", list.get(0));
        assertEquals("yyy", list.get(1));
    }

}