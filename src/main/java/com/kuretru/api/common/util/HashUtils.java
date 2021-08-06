package com.kuretru.api.common.util;

import lombok.SneakyThrows;

import javax.crypto.KeyGenerator;
import javax.crypto.Mac;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class HashUtils {

    private static final String HMAC_SHA256 = "HmacSHA256";
    private static final String HMAC_MD5 = "HmacMD5";

    private HashUtils() {

    }

    /**
     * 给定信息及加密密钥，计算HMAC-SHA256值
     *
     * @param message 信息
     * @param key     加密密钥
     * @return HMAC-SHA256值
     */
    public static String hmacSha256(final String message, final String key) {
        return computeHmacHash(message, key, HMAC_SHA256);
    }

    /**
     * 给定信息及加密密钥，计算HMAC-MD5值
     *
     * @param message 信息
     * @param key     加密密钥
     * @return HMAC-MD5值
     */
    public static String hmacMd5(final String message, final String key) {
        return computeHmacHash(message, key, HMAC_MD5);
    }

    /**
     * 给定信息、加密密钥及HASH算法，计算HMAC值
     *
     * @param message 信息
     * @param key     加密密钥
     * @param hash    HASH算法
     * @return HMAC值
     */
    @SneakyThrows()
    private static String computeHmacHash(final String message, final String key, String hash) {
        byte[] messageBytes = message.getBytes(StandardCharsets.UTF_8);
        byte[] keyBytes = key.getBytes(StandardCharsets.UTF_8);
        SecretKeySpec secretKeySpec = new SecretKeySpec(keyBytes, hash);
        Mac mac = Mac.getInstance(hash);
        mac.init(secretKeySpec);
        byte[] resultBytes = mac.doFinal(messageBytes);
        return StringUtils.bytesToHexString(resultBytes);
    }

    /**
     * 生成一个安全的SHA256加密密钥
     *
     * @return SHA256加密密钥
     */
    public static String generateSha256Key() {
        return generateKey(HMAC_SHA256);
    }

    /**
     * 生成一个安全的MD5加密密钥
     *
     * @return MD5加密密钥
     */
    public static String generateMd5Key() {
        return generateKey(HMAC_MD5);
    }

    /**
     * 生成一个安全的加密密钥
     *
     * @param hash HASH算法
     * @return 字符串形式的加密密钥
     */
    @SneakyThrows
    private static String generateKey(String hash) {
        KeyGenerator keyGenerator = KeyGenerator.getInstance(hash);
        SecretKey secretKey = keyGenerator.generateKey();
        byte[] encodedKey = secretKey.getEncoded();
        return StringUtils.bytesToHexString(encodedKey);
    }

}
