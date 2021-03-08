package com.kuretru.api.common.util;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * String与Instant类型的时间互相转换的静态工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public final class InstantUtils {

    /** 默认日期格式 */
    public static final String DEFAULT_DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
    /** 默认时区 */
    public static final ZoneId DEFAULT_ZONE_ID = ZoneId.of("Asia/Shanghai");
    /** 默认日期格式及时区下的格式化器 */
    private static final DateTimeFormatter DEFAULT_FORMATTER =
            DateTimeFormatter.ofPattern(DEFAULT_DATE_FORMAT).withZone(DEFAULT_ZONE_ID);

    private InstantUtils() {

    }

    /**
     * 使用默认日期格式及默认时区格式化输出指定时刻
     *
     * @param time 指定时刻
     * @return 格式化后的时间字符串
     */
    public static String toString(Instant time) {
        return toString(time, DEFAULT_FORMATTER);
    }

    /**
     * 使用指定格式化器格式化输出指定时刻
     *
     * @param time      指定时刻
     * @param formatter 格式化器
     * @return 格式化的时间字符串
     */
    public static String toString(Instant time, DateTimeFormatter formatter) {
        return formatter.format(time);
    }

    /**
     * 使用默认日期格式及默认时区解析时间字符串
     *
     * @param time 时间字符串
     * @return Instant类型的时间
     */
    public static Instant fromString(String time) {
        return Instant.from(DEFAULT_FORMATTER.parse(time));
    }

    /**
     * 使用指定格式化器解析时间字符串
     *
     * @param time      时间字符串
     * @param formatter 格式化器
     * @return Instant类型的时间
     */
    public static Instant fromString(String time, DateTimeFormatter formatter) {
        return Instant.from(formatter.parse(time));
    }

}
