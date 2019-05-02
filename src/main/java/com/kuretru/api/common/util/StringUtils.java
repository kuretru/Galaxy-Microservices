package com.kuretru.api.common.util;

import com.kuretru.api.common.configuration.GeneralConstants;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class StringUtils {

    /**
     * 判断字符串是否含有内容
     *
     * @param text 待检测字符串
     * @return 是否为空
     */
    public static boolean isNullOrEmpty(String text) {
        return text == null || text.isEmpty();
    }

    /**
     * 将字符串列表转换为单一字符串
     *
     * @param messages  字符串列表
     * @param separator 分隔符
     * @return 单一字符串
     */
    public static String listToString(List<String> messages, String separator) {
        if (messages == null) {
            return null;
        }
        if (messages.isEmpty()) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < messages.size(); i++) {
            builder.append(messages.get(i));
            if (i < messages.size() - 1) {
                builder.append(separator);
            }
        }
        return builder.toString();
    }

    /**
     * 使用|作为分隔符将字符串列表转换为单一字符串
     *
     * @param messages 字符串列表
     * @return 单一字符串
     */
    public static String listToString(List<String> messages) {
        return listToString(messages, GeneralConstants.ITEMS_RAW_SEPARATOR);
    }

    /**
     * 将单一字符串转换为单字符串列表
     *
     * @param text      单一字符串
     * @param separator 分隔符
     * @return 字符串列表
     */
    public static List<String> stringToList(String text, String separator) {
        if (StringUtils.isNullOrEmpty(text) || StringUtils.isNullOrEmpty(separator)) {
            return new ArrayList<>();
        }
        return Arrays.asList(text.split(separator));
    }

    /**
     * 使用|作为分隔符将单一字符串转换为单字符串列表
     *
     * @param text 单一字符串
     * @return 字符串列表
     */
    public static List<String> stringToList(String text) {
        return stringToList(text, GeneralConstants.ITEMS_SEPARATOR);
    }

}
