package com.kuretru.api.common.util;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class InstantUtils {

    /**
     * 通用日期格式
     */
    public static final String GENERAL_DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";

    /**
     * 通用时区
     */
    public static final String GENERAL_TIME_ZONE = "GMT+8";

    private static final DateTimeFormatter FORMATTER = DateTimeFormatter
            .ofPattern(GENERAL_DATE_FORMAT)
            .withZone(ZoneId.systemDefault());

    /**
     * 将Instant类的时间转换为String形式
     *
     * @param time 某一时间
     * @return 字符串形式的时间
     */
    public static String instantToString(Instant time) {
        return FORMATTER.format(time);
    }

    /**
     * 将String形式的时间转换为Instant类
     *
     * @param text 字符串形式的时间
     * @return 某一时间
     */
    public static Instant stringToInstant(String text) {
        return Instant.from(FORMATTER.parse(text));
    }

}
