package com.kuretru.microservices.common.utils;

import lombok.NonNull;

import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 字符串相关的静态工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class StringUtils {

    public static final String DEFAULT_SEPARATOR = "|";
    public static final String DEFAULT_SEPARATOR_TO_LIST = "\\|";

    private static final byte[] HEX_ARRAY = "0123456789ABCDEF".getBytes(StandardCharsets.US_ASCII);

    private StringUtils() {

    }

    /**
     * 将字节数组转换为16进制字符串
     *
     * @param bytes 字节数组
     * @return 16进制字符串
     */
    public static String bytesToHexString(@NonNull byte[] bytes) {
        byte[] result = new byte[bytes.length << 1];
        for (int i = 0; i < bytes.length; i++) {
            int c = bytes[i] & 0xFF;
            result[i << 1] = HEX_ARRAY[c >>> 4];
            result[(i << 1) + 1] = HEX_ARRAY[c & 0x0F];
        }
        return new String(result, StandardCharsets.UTF_8);
    }

    /**
     * 返回一个32位的小写字符串
     * e.g.: 7bf290551c7742878fb2d2fb73480856
     *
     * @return 32位的小写字符串
     */
    public static String randomUUID() {
        return UUID.randomUUID().toString().replace("-", "");
    }


    /**
     * 使用|作为分隔符将集合转换为单一字符串
     *
     * @param messages 集合
     * @return 单一字符串
     */
    public static String collectionToString(Collection<?> messages) {
        return collectionToString(messages, DEFAULT_SEPARATOR);
    }

    /**
     * 使用指定分隔符将集合转换为单一字符串
     *
     * @param messages  集合
     * @param separator 分隔符
     * @return 单一字符串
     */
    public static String collectionToString(Collection<?> messages, String separator) {
        if (messages == null) {
            return null;
        } else if (messages.isEmpty()) {
            return "";
        }
        Iterator<?> iterator = messages.iterator();
        StringBuilder builder = new StringBuilder(iterator.next().toString());
        while (iterator.hasNext()) {
            builder.append(separator);
            builder.append(iterator.next());
        }
        return builder.toString();
    }

    /**
     * 使用|作为分隔符将单一字符串转换为字符串列表
     *
     * @param text 单一字符串
     */
    public static List<String> stringToList(String text) {
        return stringToList(text, DEFAULT_SEPARATOR_TO_LIST);
    }

    /**
     * 指定分隔符将单一字符串转换为字符串列表
     *
     * @param text      单一字符串
     * @param separator 分隔符
     */
    public static List<String> stringToList(String text, String separator) {
        List<String> result = new ArrayList<>();
        stringToCollection(result, text, separator);
        return result;
    }

    /**
     * 使用|作为分隔符将单一字符串转换为字符串Set
     *
     * @param text 单一字符串
     */
    public static Set<String> stringToSet(String text) {
        return stringToSet(text, DEFAULT_SEPARATOR_TO_LIST);
    }

    /**
     * 指定分隔符将单一字符串转换为字符串Set
     *
     * @param text      单一字符串
     * @param separator 分隔符
     */
    public static Set<String> stringToSet(String text, String separator) {
        Set<String> result = new TreeSet<>();
        stringToCollection(result, text, separator);
        return result;
    }

    /**
     * 使用|作为分隔符将单一字符串转换为字符串集合
     *
     * @param collection 集合
     * @param text       单一字符串
     */
    public static void stringToCollection(Collection<String> collection, String text) {
        if (org.springframework.util.StringUtils.hasText(text)) {
            collection.addAll(Arrays.asList(text.split(DEFAULT_SEPARATOR_TO_LIST)));
        }
    }

    /**
     * 指定分隔符将单一字符串转换为字符串集合
     *
     * @param collection 集合
     * @param text       单一字符串
     * @param separator  分隔符
     */
    public static void stringToCollection(Collection<String> collection, String text, String separator) {
        if (org.springframework.util.StringUtils.hasText(text) && org.springframework.util.StringUtils.hasText(separator)) {
            collection.addAll(Arrays.asList(text.split(separator)));
        }
    }

}
