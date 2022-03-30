package com.kuretru.microservices.common.utils;

import org.junit.jupiter.api.Test;

import java.util.*;

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

    @Test
    void nullToEmpty() {
        assertEquals("", StringUtils.nullToEmpty(null));
        assertEquals("", StringUtils.nullToEmpty(""));
        assertEquals("abc", StringUtils.nullToEmpty("abc"));
    }

    @Test
    void randomUUID() {
        assertEquals(32, StringUtils.randomUUID().length());
    }

    @Test
    void collectionToString() {
        List<String> list = new ArrayList<>();
        list.add("xxx");
        list.add("yyy");
        list.add("zzz");
        assertEquals("xxx|yyy|zzz", StringUtils.collectionToString(list));
        assertEquals("xxx,yyy,zzz", StringUtils.collectionToString(list, ","));

        Set<String> set = new TreeSet<>();
        set.add("xxx");
        set.add("yyy");
        set.add("zzz");
        assertEquals("xxx|yyy|zzz", StringUtils.collectionToString(set));
        assertEquals("xxx,yyy,zzz", StringUtils.collectionToString(set, ","));
    }

    @Test
    void stringToList() {
        List<String> list = StringUtils.stringToList("xxx|yyy|zzz");
        assertEquals("xxx", list.get(0));
        assertEquals("yyy", list.get(1));
        assertEquals("zzz", list.get(2));
        list = StringUtils.stringToList("xxx,yyy,zzz", ",");
        assertEquals("xxx", list.get(0));
        assertEquals("yyy", list.get(1));
        assertEquals("zzz", list.get(2));
    }

    @Test
    void stringToSet() {
        Set<String> set = StringUtils.stringToSet("xxx|yyy|zzz");
        Iterator<String> iterator = set.iterator();
        assertEquals("xxx", iterator.next());
        assertEquals("yyy", iterator.next());
        assertEquals("zzz", iterator.next());
        set = StringUtils.stringToSet("xxx,yyy,zzz", ",");
        iterator = set.iterator();
        assertEquals("xxx", iterator.next());
        assertEquals("yyy", iterator.next());
        assertEquals("zzz", iterator.next());
    }

    @Test
    void stringToCollection() {
        List<String> list = new ArrayList<>();
        StringUtils.stringToCollection(list, "xxx|yyy|zzz");
        assertEquals("xxx", list.get(0));
        assertEquals("yyy", list.get(1));
        assertEquals("zzz", list.get(2));
        list = new ArrayList<>();
        StringUtils.stringToCollection(list, "xxx,yyy,zzz", ",");
        assertEquals("xxx", list.get(0));
        assertEquals("yyy", list.get(1));
        assertEquals("zzz", list.get(2));

        Set<String> set = new TreeSet<>();
        StringUtils.stringToCollection(set, "xxx|yyy|zzz");
        Iterator<String> iterator = set.iterator();
        assertEquals("xxx", iterator.next());
        assertEquals("yyy", iterator.next());
        assertEquals("zzz", iterator.next());
        set = new TreeSet<>();
        StringUtils.stringToCollection(set, "xxx,yyy,zzz", ",");
        iterator = set.iterator();
        assertEquals("xxx", iterator.next());
        assertEquals("yyy", iterator.next());
        assertEquals("zzz", iterator.next());
    }

}
