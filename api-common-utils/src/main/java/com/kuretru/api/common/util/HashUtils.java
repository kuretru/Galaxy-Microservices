package com.kuretru.api.common.util;

import java.security.MessageDigest;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class HashUtils {

    /**
     * 计算文本MD5值
     *
     * @param text 文本
     * @return MD5值
     */
    public static String computeMd5(String text) {
        StringBuilder stringBuilder = new StringBuilder();
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("MD5");
            messageDigest.update(text.getBytes("UTF-8"));
            byte[] result = messageDigest.digest();
            for (byte b : result) {
                stringBuilder.append(String.format("%02x", b & 0xff));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return stringBuilder.toString();
    }

}
