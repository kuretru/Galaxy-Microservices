package com.kuretru.api.common.util;

import lombok.NonNull;

import java.nio.charset.StandardCharsets;
import java.util.UUID;

/**
 * 字符串相关的静态工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class StringUtils {

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

}
