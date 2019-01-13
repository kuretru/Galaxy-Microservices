package com.kuretru.api.common.util;

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

}
