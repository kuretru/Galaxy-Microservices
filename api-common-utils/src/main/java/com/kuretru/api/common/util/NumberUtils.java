package com.kuretru.api.common.util;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class NumberUtils {

    /**
     * 将小数转换为百分比，并保留2位小数
     *
     * @param value 原小数
     * @return 百分比小数
     */
    public static double toPercentage(double value) {
        return (Math.round(value * 10000) / 100.0);
    }

}
